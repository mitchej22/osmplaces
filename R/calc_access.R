#' Spatial accessibility to places (two-step floating catchment area)
#'
#' Turns the place counts from [get_places()] into accessibility scores that
#' account for how many people compete for those places. For each place type
#' it runs a floating catchment area (FCA) measure from the
#' [accessibility][accessibility::floating_catchment_area] package:
#'
#' 1. Every geography with places gets a supply-to-demand ratio: its places
#'    divided by the population of all geographies within `catchment` of it.
#' 2. Every geography's score is the sum of the ratios of all geographies
#'    within `catchment` of it.
#'
#' The default scores are places per 1,000 residents (`per = 1000`). A score
#' of 0 means no places are reachable. `NA` means the catchments around the
#' geography hold no population, so the ratio is undefined.
#'
#' By default the cost of travelling between two geographies is the
#' straight-line (great-circle) distance between their centroids, and
#' geographies are weighted 1 inside `catchment` and 0 outside (the classic
#' 2SFCA). Supply and population are both treated as sitting at the centroid,
#' which is coarse for large geographies such as ZIP codes or rural tracts. For
#' travel times, build a matrix with a routing package such as r5r or osrm and
#' pass it as `travel_matrix`. For distance decay (the enhanced 2SFCA), pass a
#' function from [accessibility::decay_exponential()],
#' [accessibility::decay_stepped()] and friends as `decay`.
#'
#' Scores are relative to the geographies in `x`. Geographies near the edge of
#' the area you pulled have neighbours that are missing from `x`, so their
#' scores are understated. Pull a larger area (such as the whole state) and
#' subset afterwards if the edges matter.
#'
#' The `population` and the `boundary_year` used by [get_places()] should
#' describe the same geographies, for example ACS 2015-2019 estimates with
#' `boundary_year = 2019`.
#'
#' @param x Output of [get_places()]: a data frame with a `geoid` column and
#'   one count column per tag. Needs geometry (`geometry = TRUE`) unless you
#'   supply `travel_matrix`.
#' @param tag Character vector of count columns to score. `NULL` (default)
#'   scores every tag in `x`.
#' @param population Where to find the population (the demand). Either the name
#'   of a numeric column in `x`, or a data frame with a `geoid` column and a
#'   `population` column, such as ACS estimates from tidycensus
#'   (`transmute(acs, geoid = GEOID, population = estimate)`). Missing values
#'   count as 0 with a warning.
#' @param method `"2sfca"` (default) or `"bfca"`, the balanced floating
#'   catchment area measure. See [accessibility::floating_catchment_area()].
#' @param catchment The largest travel cost that counts as reachable, in
#'   `distance_units` for the default straight-line distances, or in the units
#'   of your `travel_matrix` (such as minutes). Required, because scores are
#'   very sensitive to it. Geography pairs farther apart than this are
#'   ignored, including by `decay`.
#' @param distance_units `"miles"` (default), `"km"` or `"meters"`. Used for
#'   `catchment` when distances are computed here; ignored with
#'   `travel_matrix`.
#' @param decay A decay function from the accessibility package, such as
#'   `accessibility::decay_exponential(0.1)`. `NULL` (default) is a plain
#'   cutoff at `catchment`.
#' @param travel_matrix Optional data frame of travel costs with `from_id` and
#'   `to_id` columns holding `geoid`s, and one cost column. Pairs missing from
#'   it are treated as unreachable. A geography is always reachable from itself
#'   at cost 0.
#' @param travel_cost Name of the cost column in `travel_matrix`. Optional when
#'   the matrix has just one cost column.
#' @param per Scores are multiplied by this: `1000` (default) gives places per
#'   1,000 residents, `1` gives places per resident.
#' @param verbose Print progress messages?
#'
#' @return `x` with one new column per tag, named `access_<tag>`, plus a
#'   `population` column if you supplied population as a data frame. It keeps
#'   the `date`, `boundary_year` and `tags` attributes of `x`, so it works with
#'   [map_places()], which maps the access columns by default.
#'
#' @examples
#' \dontrun{
#' d <- get_places(
#'   state = "OH", county = "Cuyahoga", year = 2019,
#'   tags = c("grocery", "pharmacy"), geography = "block_group", geometry = TRUE
#' )
#' pop <- data.frame(geoid = acs$GEOID, population = acs$estimate)  # e.g. tidycensus
#'
#' # Places per 1,000 residents within 1 mile
#' a <- calc_access(d, population = pop, catchment = 1)
#' map_places(a)
#'
#' # Distance decay, and a larger catchment
#' calc_access(d, "grocery", population = pop, catchment = 3,
#'             decay = accessibility::decay_exponential(0.8))
#'
#' # Travel times from r5r instead of straight-line distance
#' calc_access(d, population = pop, catchment = 15,
#'             travel_matrix = times, travel_cost = "travel_time_p50")
#' }
#' @export
calc_access <- function(x, tag = NULL, population,
                        method = c("2sfca", "bfca"), catchment,
                        distance_units = c("miles", "km", "meters"),
                        decay = NULL, travel_matrix = NULL, travel_cost = NULL,
                        per = 1000, verbose = TRUE) {
  require_pkg("accessibility", "to calculate accessibility scores")
  method <- match.arg(method)
  distance_units <- match.arg(distance_units)

  if (!is.data.frame(x) || !"geoid" %in% names(x)) {
    stop("`x` must be the output of get_places(): a data frame with a `geoid` column.",
         call. = FALSE)
  }
  if (missing(population)) {
    stop("`population` is required: a column name in `x`, or a data frame with ",
         "`geoid` and `population` columns.", call. = FALSE)
  }
  if (missing(catchment)) {
    stop("`catchment` is required: the largest travel cost that counts as reachable ",
         "(in `distance_units`, or the units of `travel_matrix`).", call. = FALSE)
  }
  check_positive(catchment, "catchment")
  check_positive(per, "per")
  if (!is.null(decay) && !is.function(decay)) {
    stop("`decay` must be NULL or a function such as accessibility::decay_exponential(0.1).",
         call. = FALSE)
  }

  ids <- as.character(x$geoid)
  if (anyNA(ids) || anyDuplicated(ids)) {
    stop("`x$geoid` must be unique with no missing values.", call. = FALSE)
  }
  tags <- access_tag_columns(x, tag)
  pop <- resolve_population(x, population, ids)
  if (anyNA(pop$values)) {
    warning(sum(is.na(pop$values)), " geographies have no population; counting them as 0.",
            call. = FALSE)
    pop$values[is.na(pop$values)] <- 0
  }

  if (is.null(travel_matrix)) {
    if (!inherits(x, "sf")) {
      stop("`x` has no geometry. Re-run get_places() with `geometry = TRUE`, ",
           "or supply `travel_matrix`.", call. = FALSE)
    }
    meters_per_unit <- c(miles = 1609.344, km = 1000, meters = 1)[[distance_units]]
    costs <- straight_line_costs(sf::st_geometry(x), ids,
                                 catchment * meters_per_unit, meters_per_unit)
    cost_units <- distance_units
  } else {
    costs <- prepare_travel_matrix(travel_matrix, travel_cost, ids, catchment)
    cost_units <- "travel cost units"
  }
  costs <- add_self_pairs(costs, ids)
  if (verbose) {
    message(sprintf("%s access for %d tag(s): %d geographies, %s origin-destination pairs within %s.",
                    toupper(method), length(tags), length(ids),
                    format(nrow(costs), big.mark = ","), catchment_text(catchment, cost_units)))
  }

  decay_fn <- decay %||% accessibility::decay_binary(cutoff = catchment)
  land <- data.frame(id = ids, opportunity = 0, demand = pop$values,
                     stringsAsFactors = FALSE)

  result <- x
  if (isTRUE(pop$added)) result[[pop$column]] <- pop$values
  specs <- attr(x, "access")
  if (is.null(specs)) specs <- list()
  new_cols <- character(0)
  for (tg in tags) {
    supply <- as.numeric(x[[tg]])
    if (anyNA(supply)) {
      warning(sum(is.na(supply)), " geographies have no count for '", tg,
              "'; counting them as 0, so nearby scores are lower bounds.", call. = FALSE)
      supply[is.na(supply)] <- 0
    }
    land$opportunity <- supply
    fca <- accessibility::floating_catchment_area(
      travel_matrix = costs, land_use_data = land,
      opportunity = "opportunity", travel_cost = "travel_cost", demand = "demand",
      method = method, decay_function = decay_fn
    )
    score <- fca[["opportunity"]][match(ids, fca$id)]
    score[!is.finite(score)] <- NA_real_

    col <- paste0("access_", tg)
    result[[col]] <- score * per
    new_cols <- c(new_cols, col)
    specs[[col]] <- list(
      tag = tg, method = method, catchment = catchment, units = cost_units, per = per,
      population = pop$column,
      label = access_label(method, per, catchment, cost_units)
    )
  }

  for (a in c("date", "boundary_year", "tags")) {
    if (!is.null(attr(x, a))) attr(result, a) <- attr(x, a)
  }
  attr(result, "access_columns") <- union(attr(x, "access_columns"), new_cols)
  attr(result, "access") <- specs
  result
}

access_label <- function(method, per, catchment, units) {
  rate <- if (per == 1) "places per resident" else
    paste0("places per ", format(per, big.mark = ","), " residents")
  paste0(toupper(method), ": ", rate, ", ", catchment_text(catchment, units), " catchment")
}

catchment_text <- function(catchment, units) {
  if (units == "miles" && catchment == 1) units <- "mile"
  paste(format(catchment), units)
}

check_positive <- function(v, name) {
  if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v <= 0) {
    stop("`", name, "` must be a single positive number.", call. = FALSE)
  }
}

access_tag_columns <- function(x, tag) {
  if (is.null(tag)) {
    tags <- attr(x, "tags")
    if (is.null(tags)) {
      stop("Can't tell which columns are place counts. Pass them as `tag`.", call. = FALSE)
    }
    tags <- intersect(tags, names(x))
    if (!length(tags)) stop("No count columns found in `x`.", call. = FALSE)
    return(tags)
  }
  tag <- unique(as.character(tag))
  bad <- tag[!tag %in% names(x) | !vapply(tag, function(t) is.numeric(x[[t]]), logical(1))]
  if (length(bad)) {
    stop("Not a numeric count column in `x`: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  tag
}

# Returns the population values aligned with `ids`, the name of the column they
# live in, and whether that column still needs adding to the result.
resolve_population <- function(x, population, ids) {
  if (is.character(population) && length(population) == 1L) {
    if (!population %in% names(x)) {
      stop("`population` column '", population, "' not found in `x`.", call. = FALSE)
    }
    out <- list(values = x[[population]], column = population, added = FALSE)
  } else if (is.data.frame(population)) {
    nm <- names(population)
    id_col <- nm[tolower(nm) == "geoid"]
    if (length(id_col) != 1L || !"population" %in% nm) {
      stop("A `population` data frame needs `geoid` and `population` columns.",
           call. = FALSE)
    }
    pop_ids <- as.character(population[[id_col]])
    if (anyDuplicated(pop_ids)) {
      stop("`population` has duplicated geoids.", call. = FALSE)
    }
    out <- list(values = population[["population"]][match(ids, pop_ids)],
                column = "population", added = TRUE)
  } else {
    stop("`population` must be a column name in `x` or a data frame with ",
         "`geoid` and `population` columns.", call. = FALSE)
  }
  if (!is.numeric(out$values)) stop("Population must be numeric.", call. = FALSE)
  if (any(out$values < 0, na.rm = TRUE)) {
    stop("Population cannot be negative.", call. = FALSE)
  }
  out$values <- as.numeric(out$values)
  out
}

# Great-circle distance in meters, using the radius s2 uses.
haversine_m <- function(lon1, lat1, lon2, lat2, radius = 6371010) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * radius * asin(pmin(1, sqrt(a)))
}

# Centroid-to-centroid distances for every pair of geographies within
# `catchment_m`, as a long table in `unit_m` meters. Only nearby pairs are
# built, so state-sized inputs do not create an n x n matrix.
straight_line_costs <- function(geom, ids, catchment_m, unit_m) {
  if (is.na(sf::st_crs(geom))) {
    stop("`x` has no coordinate reference system, so distances can't be computed.",
         call. = FALSE)
  }
  if (any(sf::st_is_empty(geom))) {
    stop("`x` has empty geometries; drop them before calling calc_access().",
         call. = FALSE)
  }
  geom <- sf::st_transform(geom, 4326)
  # Planar centroids are accurate for geographies this small; s2 rejects some
  # TIGER polygons outright.
  cent <- with_planar(suppressMessages(suppressWarnings(sf::st_centroid(geom))))
  near <- with_s2(sf::st_is_within_distance(cent, cent, dist = catchment_m, sparse = TRUE))

  from <- rep.int(seq_along(near), lengths(near))
  to <- unlist(near, use.names = FALSE)
  xy <- sf::st_coordinates(cent)
  dist_m <- haversine_m(xy[from, 1], xy[from, 2], xy[to, 1], xy[to, 2])
  data.frame(from_id = ids[from], to_id = ids[to], travel_cost = dist_m / unit_m,
             stringsAsFactors = FALSE)
}

prepare_travel_matrix <- function(travel_matrix, travel_cost, ids, catchment) {
  if (!is.data.frame(travel_matrix) ||
      !all(c("from_id", "to_id") %in% names(travel_matrix))) {
    stop("`travel_matrix` must be a data frame with `from_id` and `to_id` columns.",
         call. = FALSE)
  }
  if (is.null(travel_cost)) {
    others <- setdiff(names(travel_matrix), c("from_id", "to_id"))
    if (length(others) != 1L) {
      stop("`travel_cost` must name the cost column in `travel_matrix`.", call. = FALSE)
    }
    travel_cost <- others
  }
  if (!is.character(travel_cost) || length(travel_cost) != 1L ||
      !travel_cost %in% names(travel_matrix)) {
    stop("`travel_cost` must name a column of `travel_matrix`.", call. = FALSE)
  }

  costs <- data.frame(
    from_id = as.character(travel_matrix$from_id),
    to_id = as.character(travel_matrix$to_id),
    travel_cost = as.numeric(travel_matrix[[travel_cost]]),
    stringsAsFactors = FALSE
  )
  known <- costs$from_id %in% ids & costs$to_id %in% ids
  if (!any(known)) {
    stop("No `from_id`/`to_id` in `travel_matrix` match `x$geoid`.", call. = FALSE)
  }
  costs <- costs[known & !is.na(costs$travel_cost) & costs$travel_cost <= catchment, ,
                 drop = FALSE]
  if (anyDuplicated(costs[c("from_id", "to_id")])) {
    stop("`travel_matrix` must have one row per from_id/to_id pair; filter to ",
         "a single departure time or summary statistic first.", call. = FALSE)
  }
  costs
}

# The accessibility package silently gives a geography none of its own places
# if the matrix lacks its from_id == to_id row.
add_self_pairs <- function(costs, ids) {
  have <- costs$from_id[costs$from_id == costs$to_id]
  lacking <- setdiff(ids, have)
  if (!length(lacking)) return(costs)
  rbind(costs, data.frame(from_id = lacking, to_id = lacking, travel_cost = 0,
                          stringsAsFactors = FALSE))
}
