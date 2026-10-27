normalize_geography <- function(geography) {
  if (!is.character(geography) || length(geography) != 1L || is.na(geography)) {
    stop("`geography` must be one of \"block_group\", \"tract\", or \"zip_code\".",
         call. = FALSE)
  }
  key <- gsub("[^a-z0-9]+", "_", tolower(trimws(geography)))
  map <- c(
    block_group = "block_group", block_groups = "block_group",
    blockgroup = "block_group", bg = "block_group", bgs = "block_group",
    tract = "tract", tracts = "tract",
    zip_code = "zip_code", zip_codes = "zip_code", zip = "zip_code",
    zcta = "zip_code", zctas = "zip_code"
  )
  out <- map[key]
  if (is.na(out)) {
    stop(sprintf("Unknown geography '%s'. Use \"block_group\", \"tract\", or \"zip_code\".",
                 geography), call. = FALSE)
  }
  unname(out)
}

# Turn a bbox (numeric c(xmin, ymin, xmax, ymax) or sf bbox) into a WGS84 sfc.
as_bbox_geom <- function(bbox) {
  crs <- sf::st_crs(4326)
  if (inherits(bbox, "bbox")) {
    if (!is.na(sf::st_crs(bbox))) crs <- sf::st_crs(bbox)
    u <- unclass(bbox)
    vals <- as.numeric(u[c("xmin", "ymin", "xmax", "ymax")])
  } else if (is.numeric(bbox) && length(bbox) == 4L) {
    vals <- as.numeric(if (all(c("xmin", "ymin", "xmax", "ymax") %in% names(bbox))) {
      bbox[c("xmin", "ymin", "xmax", "ymax")]
    } else {
      bbox
    })
  } else {
    stop("`bbox` must be c(xmin, ymin, xmax, ymax) or an sf bbox.", call. = FALSE)
  }
  if (any(!is.finite(vals)) || vals[1] >= vals[3] || vals[2] >= vals[4]) {
    stop("`bbox` must be finite with xmin < xmax and ymin < ymax.", call. = FALSE)
  }
  names(vals) <- c("xmin", "ymin", "xmax", "ymax")
  geom <- sf::st_as_sfc(sf::st_bbox(vals, crs = crs))
  if (!isTRUE(sf::st_crs(geom) == sf::st_crs(4326))) geom <- sf::st_transform(geom, 4326)
  b <- sf::st_bbox(geom)
  if (b[["xmin"]] < -180 || b[["xmax"]] > 180 || b[["ymin"]] < -90 || b[["ymax"]] > 90) {
    stop("`bbox` must be in WGS84 longitude/latitude.", call. = FALSE)
  }
  geom
}

# Wrap a tigris call so failures say which boundary set and year were requested.
call_tigris <- function(fun, what, args) {
  tryCatch(
    do.call(fun, args),
    error = function(e) {
      stop(sprintf("tigris could not fetch %s boundaries for year %s: %s",
                   what, args$year, conditionMessage(e)), call. = FALSE)
    }
  )
}

# County files use different column names in older vintages.
standardize_counties <- function(x) {
  if (!"STATEFP" %in% names(x) && "STATE" %in% names(x)) x$STATEFP <- as.character(x$STATE)
  if (!"COUNTYFP" %in% names(x) && "COUNTY" %in% names(x)) x$COUNTYFP <- as.character(x$COUNTY)
  if (!"GEOID" %in% names(x)) x$GEOID <- paste0(x$STATEFP, x$COUNTYFP)
  if (!all(c("STATEFP", "COUNTYFP", "GEOID", "NAME") %in% names(x))) {
    stop("Unexpected county boundary columns returned by tigris.", call. = FALSE)
  }
  x
}

# Keep counties named or numbered in `county` (name, 3-digit FIPS, or GEOID).
match_counties <- function(counties, county) {
  norm <- function(x) tolower(trimws(sub("\\s+county$", "", trimws(x), ignore.case = TRUE)))
  county <- as.character(county)

  hits <- lapply(county, function(cn) {
    hit <- norm(counties$NAME) == norm(cn)
    if (grepl("^[0-9]+$", trimws(cn))) {
      cn <- trimws(cn)
      hit <- hit |
        counties$COUNTYFP == sprintf("%03d", as.integer(cn)) |
        (nchar(cn) == 5L & counties$GEOID == cn)
    }
    which(hit)
  })

  missing <- county[lengths(hits) == 0L]
  if (length(missing)) {
    stop("County not found: ", paste(shQuote(missing), collapse = ", "),
         ". Available: ", paste(sort(counties$NAME), collapse = ", "), call. = FALSE)
  }
  counties[sort(unique(unlist(hits))), , drop = FALSE]
}

fetch_counties <- function(state, year) {
  args <- list(cb = TRUE, year = year, progress_bar = FALSE)
  if (!is.null(state)) args$state <- state
  standardize_counties(call_tigris(tigris::counties, "county", args))
}

# Resolve state/county/bbox to the counties involved (plus the bbox, if any).
resolve_aoi <- function(state, county, bbox, year) {
  if (!is.null(bbox)) {
    bbox_geom <- as_bbox_geom(bbox)
    counties <- fetch_counties(NULL, year)
    hit <- with_planar(
      lengths(sf::st_intersects(planar_valid_4326(sf::st_geometry(counties)), bbox_geom)) > 0
    )
    counties <- counties[hit, , drop = FALSE]
    if (!nrow(counties)) {
      stop("No U.S. counties intersect `bbox`. It must be in longitude/latitude order ",
           "c(xmin, ymin, xmax, ymax).", call. = FALSE)
    }
    return(list(counties = counties, whole_state = FALSE, bbox_geom = bbox_geom))
  }

  counties <- fetch_counties(state, year)
  whole_state <- is.null(county)
  if (!whole_state) counties <- match_counties(counties, county)
  list(counties = counties, whole_state = whole_state, bbox_geom = NULL)
}

fetch_census_units <- function(geography, aoi, year, cb) {
  fun <- switch(geography, tract = tigris::tracts, block_group = tigris::block_groups)
  what <- gsub("_", " ", geography)
  pieces <- lapply(unique(aoi$counties$STATEFP), function(st) {
    county_arg <- if (aoi$whole_state) {
      NULL
    } else {
      aoi$counties$COUNTYFP[aoi$counties$STATEFP == st]
    }
    args <- list(state = st, cb = cb, year = year, progress_bar = FALSE)
    if (!is.null(county_arg)) args$county <- county_arg
    call_tigris(fun, what, args)
  })
  do.call(rbind, pieces)
}

fetch_zctas <- function(aoi, year, cb) {
  z <- call_tigris(tigris::zctas, "ZCTA",
                   list(cb = cb, year = year, progress_bar = FALSE))
  geom <- sf::st_geometry(z)

  keep <- with_planar({
    if (!is.null(aoi$bbox_geom)) {
      lengths(sf::st_intersects(sf::st_transform(geom, 4326), aoi$bbox_geom)) > 0
    } else {
      pts <- sf::st_transform(suppressWarnings(sf::st_point_on_surface(geom)), 4326)
      area <- sf::st_union(sf::st_transform(sf::st_geometry(aoi$counties), 4326))
      lengths(sf::st_intersects(pts, area)) > 0
    }
  })
  z[keep, , drop = FALSE]
}

# Reduce a tigris result to `geoid` + geometry, optionally dropping water-only
# areas, regardless of vintage-specific column names.
standardize_boundaries <- function(x, drop_water = TRUE) {
  id_col <- intersect(c("GEOID", "GEOID20", "GEOID10", "ZCTA5CE20", "ZCTA5CE10"), names(x))
  if (length(id_col)) {
    geoid <- as.character(x[[id_col[[1L]]]])
  } else if ("GEO_ID" %in% names(x)) {
    geoid <- sub("^.*US", "", as.character(x[["GEO_ID"]]))
  } else {
    stop("Could not find a GEOID column in the boundary data.", call. = FALSE)
  }

  keep <- rep(TRUE, length(geoid))
  land_col <- intersect(c("ALAND", "ALAND20", "ALAND10"), names(x))
  if (drop_water && length(land_col)) {
    land <- suppressWarnings(as.numeric(x[[land_col[[1L]]]]))
    keep <- is.na(land) | land > 0
  }

  geoid <- trimws(geoid)
  out <- sf::st_sf(geoid = geoid, geometry = sf::st_geometry(x))[keep, , drop = FALSE]
  out[!duplicated(out$geoid), , drop = FALSE]
}

fetch_boundaries <- function(geography, aoi, year, cb, drop_water) {
  raw <- if (identical(geography, "zip_code")) {
    fetch_zctas(aoi, year, cb)
  } else {
    fetch_census_units(geography, aoi, year, cb)
  }
  out <- standardize_boundaries(raw, drop_water)

  if (!is.null(aoi$bbox_geom) && !identical(geography, "zip_code")) {
    keep <- with_planar(
      lengths(sf::st_intersects(sf::st_transform(sf::st_geometry(out), 4326),
                                aoi$bbox_geom)) > 0
    )
    out <- out[keep, , drop = FALSE]
  }

  if (!nrow(out)) {
    stop("No ", gsub("_", " ", geography), " boundaries found for the requested area.",
         call. = FALSE)
  }
  out
}
