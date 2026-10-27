#' Resolve the `year` argument to a snapshot date
#'
#' @param year `NULL`, a year, an ISO date string, or a `Date`.
#' @return A `Date`.
#' @noRd
resolve_date <- function(year = NULL) {
  today <- Sys.Date()

  if (is.null(year)) {
    date <- as.Date(sprintf("%s-01-01", format(today, "%Y")))
  } else {
    if (length(year) != 1L || is.na(year)) {
      stop("`year` must be a single year or date.", call. = FALSE)
    }
    if (inherits(year, "Date")) {
      date <- year
    } else if (inherits(year, "POSIXt")) {
      date <- as.Date(year)
    } else {
      x <- trimws(as.character(year))
      if (grepl("^[0-9]{4}$", x)) {
        date <- as.Date(paste0(x, "-01-01"), format = "%Y-%m-%d")
      } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
        date <- as.Date(x, format = "%Y-%m-%d")
      } else {
        stop("`year` must be a four-digit year (e.g. 2019) or an ISO date ",
             "(e.g. \"2019-06-15\").", call. = FALSE)
      }
    }
  }

  if (is.na(date)) {
    stop("`year` is not a valid date.", call. = FALSE)
  }
  if (date < OHSOME_EARLIEST) {
    stop(sprintf("ohsome has no OpenStreetMap history before %s.", OHSOME_EARLIEST),
         call. = FALSE)
  }
  if (date > today) {
    stop("`year` is in the future.", call. = FALSE)
  }
  date
}

# Census boundary vintage matching the snapshot year. Block group and tract
# files start at 2010; the upper limit follows tigris' default year.
default_boundary_year <- function(date) {
  max_year <- as.integer(getOption("tigris_year", 2022L))
  min(max(as.integer(format(date, "%Y")), 2010L), max_year)
}
