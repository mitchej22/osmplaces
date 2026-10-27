`%||%` <- function(x, y) if (is.null(x)) y else x

# Earliest date ohsome has OSM history for, and the announced end of the v1 API.
OHSOME_EARLIEST <- as.Date("2007-10-08")
OHSOME_V1_SHUTDOWN <- as.Date("2026-11-30")

# Run `expr` with s2 switched off so sf uses planar GEOS operations. TIGER
# polygons often fail s2's stricter validity checks.
with_planar <- function(expr) {
  old <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  expr
}

# Run `expr` with s2 switched on: spherical distances for lon/lat data.
with_s2 <- function(expr) {
  old <- suppressMessages(sf::sf_use_s2(TRUE))
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  expr
}

# Transform to WGS84 (what ohsome expects) and repair invalid polygons.
planar_valid_4326 <- function(geom) {
  with_planar({
    geom <- sf::st_transform(geom, 4326)
    bad <- !sf::st_is_valid(geom)
    if (any(bad, na.rm = TRUE)) geom[bad] <- sf::st_make_valid(geom[bad])
    geom
  })
}
