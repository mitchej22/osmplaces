#' Count OpenStreetMap places by census geography
#'
#' Uses the ohsome API to count OpenStreetMap features from the SDOH place-type
#' taxonomy (see [places_taxonomy()]) inside census tracts, block groups, or
#' ZIP code tabulation areas, as of a given date.
#'
#' @section Choosing the area of interest:
#' Supply either `state` (optionally with `county`) or `bbox`.
#' * `state` alone returns every geography in the state.
#' * `state` + `county` restricts to one or more counties.
#' * `bbox` returns every geography that intersects the box.
#'
#' For `geography = "zip_code"` with a state or county, a ZCTA is kept when its
#' representative point falls inside the area. ZCTAs approximate, but are not
#' identical to, USPS ZIP codes.
#'
#' @section ohsome API versions:
#' `api = "v1"` sends many polygons per request and needs no key, but the v1
#' service is scheduled to shut down on 2026-11-30. `api = "v2"` needs a free
#' API key (`api_key` or the `OHSOME_API_KEY` environment variable) and accepts
#' only one polygon per request, so it issues one request per geography per tag.
#' Set `options(osmplaces.ohsome_url = "...")` to use a different server.
#'
#' @section Counting caveats:
#' Counts are numbers of OSM elements matching each taxonomy entry. The same
#' physical place can be counted more than once when it is mapped as both a
#' node and a building outline, or matches more than one tag, and an element
#' crossing a geography boundary can be counted in each geography it touches.
#'
#' @param state A single state: abbreviation, name, or FIPS code.
#' @param county Optional county names, FIPS codes, or 5-digit GEOIDs within
#'   `state`.
#' @param bbox Optional bounding box in WGS84 longitude/latitude, either a
#'   numeric vector `c(xmin, ymin, xmax, ymax)` or an [sf::st_bbox()] object.
#' @param geography One of `"block_group"` (default), `"tract"`, or
#'   `"zip_code"`. Aliases such as `"bg"` and `"zcta"` are accepted.
#' @param year A year (`2019` or `"2019"`, meaning January 1 of that year), an
#'   ISO date (`"2019-06-15"`), or a `Date`. `NULL` uses January 1 of the
#'   current year.
#' @param tags `"all"` (default) or a character vector of taxonomy labels or
#'   aliases, e.g. `c("grocery", "pharmacy", "bank")`.
#' @param geometry If `TRUE`, return an [sf][sf::st_sf] object including the
#'   boundary polygons.
#' @param boundary_year Census (TIGER) vintage used for the boundaries. `NULL`
#'   uses the year of `year`, limited to 2010 through
#'   `getOption("tigris_year", 2022)`.
#' @param cb Use generalized cartographic boundary files? `NULL` (default)
#'   uses them only for `"zip_code"`, where the national TIGER/Line file is very
#'   large.
#' @param drop_water Drop geographies with zero land area (such as the Lake Erie
#'   tracts)?
#' @param api ohsome API version, `"v1"` or `"v2"`.
#' @param api_key ohsome API key, required for `api = "v2"`.
#' @param chunk_size Number of polygons per request for `api = "v1"`.
#' @param verbose Print progress messages?
#'
#' @return A data frame with one row per geography: a `geoid` column and one
#'   integer count column per requested tag. With `geometry = TRUE`, an `sf`
#'   object with the same columns plus a geometry column; pass it to
#'   [map_places()] to draw it. The snapshot date, boundary vintage, and tag
#'   labels are stored in the `"date"`, `"boundary_year"`, and `"tags"`
#'   attributes.
#'
#' @examples
#' \dontrun{
#' get_places(
#'   state = "OH", county = "Cuyahoga", year = "2019",
#'   tags = "all", geography = "block_group"
#' )
#'
#' get_places(
#'   bbox = c(-81.75, 41.45, -81.60, 41.55), geography = "tract",
#'   tags = c("grocery", "pharmacy"), geometry = TRUE
#' )
#' }
#' @export
get_places <- function(state = NULL,
                       county = NULL,
                       bbox = NULL,
                       geography = "block_group",
                       year = NULL,
                       tags = "all",
                       geometry = FALSE,
                       boundary_year = NULL,
                       cb = NULL,
                       drop_water = TRUE,
                       api = c("v1", "v2"),
                       api_key = NULL,
                       chunk_size = 250L,
                       verbose = TRUE) {
  api <- match.arg(api)
  geography <- normalize_geography(geography)
  date <- resolve_date(year)
  validate_aoi_args(state, county, bbox)

  taxonomy <- read_taxonomy()
  labels <- resolve_tags(tags, taxonomy)

  if (is.null(boundary_year)) boundary_year <- default_boundary_year(date)
  if (is.null(cb)) cb <- identical(geography, "zip_code")
  chunk_size <- as.integer(chunk_size)
  if (length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1L) {
    stop("`chunk_size` must be a positive integer.", call. = FALSE)
  }

  client <- ohsome_client(api = api, api_key = api_key,
                          chunk_size = chunk_size, verbose = verbose)

  if (verbose) message(sprintf("Fetching %s boundaries (TIGER %s)...",
                               geography, boundary_year))
  aoi <- resolve_aoi(state, county, bbox, boundary_year)
  boundaries <- fetch_boundaries(geography, aoi, boundary_year, cb, drop_water)
  n_geo <- nrow(boundaries)
  if (verbose) message(sprintf("Found %d %s geographies.", n_geo, geography))

  api_boundaries <- list(
    ids = boundaries$geoid,
    geom = planar_valid_4326(sf::st_geometry(boundaries))
  )

  if (verbose && identical(api, "v2")) {
    message(sprintf("ohsome v2 needs one request per geography per tag: %d requests.",
                    n_geo * length(labels)))
  }

  result <- boundaries
  missing_counts <- character(0)
  for (i in seq_along(labels)) {
    label <- labels[[i]]
    if (verbose) message(sprintf("[%d/%d] Counting '%s' as of %s...",
                                 i, length(labels), label, format(date)))
    filter <- build_ohsome_filter(taxonomy[[label]])
    counts <- count_places(api_boundaries, filter, date, client)
    if (anyNA(counts)) missing_counts <- c(missing_counts, label)
    result[[label]] <- as.integer(round(counts))
  }

  if (length(missing_counts)) {
    warning("ohsome returned no count for some geographies (set to NA) for: ",
            paste(missing_counts, collapse = ", "), call. = FALSE)
  }

  if (!isTRUE(geometry)) result <- sf::st_drop_geometry(result)
  attr(result, "date") <- format(date)
  attr(result, "boundary_year") <- boundary_year
  attr(result, "tags") <- labels
  result
}

validate_aoi_args <- function(state, county, bbox) {
  if (!is.null(bbox)) {
    if (!is.null(state) || !is.null(county)) {
      stop("Provide either `bbox` or `state`/`county`, not both.", call. = FALSE)
    }
  } else if (is.null(state)) {
    stop("Provide `state` (optionally with `county`) or `bbox`.", call. = FALSE)
  }
  if (!is.null(state) && (!is.atomic(state) || length(state) != 1L || is.na(state))) {
    stop("`state` must be a single state abbreviation, name, or FIPS code.",
         call. = FALSE)
  }
  if (!is.null(county) && is.null(state)) {
    stop("`county` requires `state`.", call. = FALSE)
  }
  invisible(TRUE)
}
