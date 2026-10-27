# ohsome backends ---------------------------------------------------------
#
# count_places() takes
#   boundaries: list(ids = <character geoids>, geom = <sfc in EPSG:4326>)
#   filter:     an ohsome filter string
#   date:       a Date (snapshot)
#   client:     from ohsome_client()
# and returns a numeric vector of counts aligned with `boundaries$ids`
# (NA where the API returned nothing).

ohsome_client <- function(api = c("v1", "v2"),
                          api_key = NULL,
                          chunk_size = 250L,
                          max_active = 5L,
                          timeout = 620,
                          verbose = TRUE) {
  api <- match.arg(api)

  default_url <- switch(api,
    v1 = "https://api.ohsome.org/v1",
    v2 = "https://api.heigit.org/ohsome-api/v2-rc"
  )
  base_url <- sub("/+$", "", getOption("osmplaces.ohsome_url", default_url))

  if (identical(api, "v1") && Sys.Date() >= OHSOME_V1_SHUTDOWN) {
    warning("The ohsome API v1 was scheduled to shut down on ", OHSOME_V1_SHUTDOWN,
            ". Use `api = \"v2\"` with an API key.", call. = FALSE)
  }

  if (identical(api, "v2")) {
    api_key <- api_key %||% Sys.getenv("OHSOME_API_KEY", unset = "")
    if (!nzchar(api_key)) {
      stop("`api = \"v2\"` needs an API key. Pass `api_key` or set OHSOME_API_KEY ",
           "(free sign-up: https://account.heigit.org/signup).", call. = FALSE)
    }
  }

  list(api = api, base_url = base_url, api_key = api_key, chunk_size = chunk_size,
       max_active = max_active, timeout = timeout, verbose = verbose)
}

count_places <- function(boundaries, filter, date, client) {
  switch(client$api,
    v1 = count_places_v1(boundaries, filter, date, client),
    v2 = count_places_v2(boundaries, filter, date, client)
  )
}

# Shared HTTP plumbing ----------------------------------------------------

ohsome_user_agent <- function() "osmplaces (R package)"

ohsome_error_body <- function(resp) {
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (!is.list(body)) return(NULL)
  msg <- body$message %||% body$detail %||% body$error
  if (is.null(msg)) return(NULL)
  if (is.list(msg)) msg <- as.character(jsonlite::toJSON(msg, auto_unbox = TRUE))
  as.character(msg)
}

ohsome_finalize <- function(req, client) {
  req <- httr2::req_user_agent(req, ohsome_user_agent())
  req <- httr2::req_timeout(req, client$timeout)
  req <- httr2::req_retry(
    req,
    max_tries = 3L,
    is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 502L, 503L, 504L)
  )
  httr2::req_error(req, body = ohsome_error_body)
}

# v1: many polygons per request ------------------------------------------

# Build a GeoJSON FeatureCollection string. Each feature carries its id as an
# `id` property, which ohsome v1 echoes back as `groupByObject`.
boundaries_to_geojson <- function(geom, ids) {
  obj <- sf::st_sf(id = as.character(ids), geometry = geom)
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(obj, tmp, driver = "GeoJSON", quiet = TRUE,
               layer_options = "COORDINATE_PRECISION=6")
  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

ohsome_post_v1 <- function(path, params, client) {
  req <- httr2::request(client$base_url)
  req <- httr2::req_url_path_append(req, path)
  req <- do.call(httr2::req_body_form, c(list(req), params))
  req <- ohsome_finalize(req, client)
  resp <- httr2::req_perform(req)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

parse_v1_group_by_boundary <- function(parsed) {
  groups <- parsed$groupByResult
  if (is.null(groups)) {
    stop("Unexpected ohsome response: no 'groupByResult' element.", call. = FALSE)
  }
  ids <- vapply(groups, function(g) as.character(g$groupByObject %||% ""), "")
  values <- vapply(groups, function(g) {
    r <- g$result
    if (length(r) >= 1L && !is.null(r[[1L]]$value)) as.numeric(r[[1L]]$value) else NA_real_
  }, numeric(1))
  stats::setNames(values, ids)
}

# Put counts back in the order of `ids`. If ohsome did not echo our ids, fall
# back to request order.
align_v1_counts <- function(res, ids) {
  m <- match(ids, names(res))
  if (all(is.na(m)) && length(res) == length(ids)) {
    warning("ohsome did not return the supplied boundary ids; assuming results ",
            "are in request order.", call. = FALSE)
    return(unname(res))
  }
  unname(res[m])
}

count_places_v1 <- function(boundaries, filter, date, client) {
  n <- length(boundaries$ids)
  chunks <- split(seq_len(n), ceiling(seq_len(n) / client$chunk_size))
  out <- rep(NA_real_, n)

  for (k in seq_along(chunks)) {
    idx <- chunks[[k]]
    if (client$verbose && length(chunks) > 1L) {
      message(sprintf("    request %d/%d", k, length(chunks)))
    }
    params <- list(
      bpolys = boundaries_to_geojson(boundaries$geom[idx], boundaries$ids[idx]),
      filter = filter,
      time = format(date),
      format = "json"
    )
    parsed <- ohsome_post_v1("elements/count/groupBy/boundary", params, client)
    out[idx] <- align_v1_counts(parse_v1_group_by_boundary(parsed), boundaries$ids[idx])
  }
  out
}

# v2: one polygon per request --------------------------------------------

# A snapshot is requested as a one-day time series and the first value is used.
# Adjust here if v2 accepts a plain timestamp for the count endpoint.
v2_time_spec <- function(date) {
  list(start = format(date), end = format(date + 1), interval = "P1D")
}

ohsome_v2_request <- function(wkt, filter, date, client) {
  req <- httr2::request(client$base_url)
  req <- httr2::req_url_path_append(req, "stats/features/count.json")
  req <- httr2::req_headers(req, Authorization = client$api_key)
  req <- httr2::req_body_json(
    req,
    list(aoi = wkt, filter = filter, time = v2_time_spec(date)),
    auto_unbox = TRUE
  )
  ohsome_finalize(req, client)
}

parse_v2_count <- function(txt) {
  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  v <- unlist(parsed$result$value)
  if (length(v)) as.numeric(v[[1L]]) else NA_real_
}

# Run requests concurrently; anything that failed is retried one at a time so
# the usual retry/backoff and error messages apply.
perform_many <- function(reqs, client) {
  resps <- httr2::req_perform_parallel(
    reqs, on_error = "continue", progress = client$verbose,
    max_active = client$max_active
  )
  failed <- which(!vapply(resps, inherits, logical(1), what = "httr2_response"))
  for (i in failed) resps[[i]] <- httr2::req_perform(reqs[[i]])
  resps
}

count_places_v2 <- function(boundaries, filter, date, client) {
  wkt <- sf::st_as_text(boundaries$geom)
  reqs <- lapply(wkt, ohsome_v2_request, filter = filter, date = date, client = client)

  # Send the first request alone so a bad key or filter fails fast.
  resps <- list(httr2::req_perform(reqs[[1L]]))
  if (length(reqs) > 1L) resps <- c(resps, perform_many(reqs[-1L], client))

  vapply(resps, function(r) parse_v2_count(httr2::resp_body_string(r)), numeric(1))
}
