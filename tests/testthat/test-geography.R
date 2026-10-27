test_that("normalize_geography accepts aliases", {
  expect_equal(normalize_geography("block_group"), "block_group")
  expect_equal(normalize_geography("BG"), "block_group")
  expect_equal(normalize_geography("Block Group"), "block_group")
  expect_equal(normalize_geography("tracts"), "tract")
  expect_equal(normalize_geography("zcta"), "zip_code")
  expect_equal(normalize_geography("zip-code"), "zip_code")
  expect_error(normalize_geography("county"), "Unknown geography")
  expect_error(normalize_geography(c("tract", "bg")), "must be one of")
})

test_that("as_bbox_geom validates and builds a WGS84 polygon", {
  g <- as_bbox_geom(c(-82, 41, -81, 42))
  expect_s3_class(g, "sfc")
  expect_equal(unname(as.numeric(sf::st_bbox(g))), c(-82, 41, -81, 42))

  named <- as_bbox_geom(c(ymin = 41, xmax = -81, xmin = -82, ymax = 42))
  expect_equal(unname(as.numeric(sf::st_bbox(named))), c(-82, 41, -81, 42))

  from_sf <- as_bbox_geom(sf::st_bbox(c(xmin = -82, ymin = 41, xmax = -81, ymax = 42),
                                      crs = sf::st_crs(4326)))
  expect_equal(unname(as.numeric(sf::st_bbox(from_sf))), c(-82, 41, -81, 42))

  expect_error(as_bbox_geom(c(-81, 41, -82, 42)), "xmin < xmax")
  expect_error(as_bbox_geom(c(1, 2, 3)), "must be")
  expect_error(as_bbox_geom(c(-200, 41, -81, 42)), "longitude/latitude")
})

test_that("match_counties matches names, FIPS and GEOIDs", {
  counties <- data.frame(
    STATEFP = "39",
    COUNTYFP = c("035", "049", "093"),
    GEOID = c("39035", "39049", "39093"),
    NAME = c("Cuyahoga", "Franklin", "Lorain"),
    stringsAsFactors = FALSE
  )
  expect_equal(match_counties(counties, "cuyahoga county")$GEOID, "39035")
  expect_equal(match_counties(counties, c("035", "39049"))$GEOID, c("39035", "39049"))
  expect_equal(match_counties(counties, 93)$NAME, "Lorain")
  expect_error(match_counties(counties, "Nowhere"), "Nowhere")
})

test_that("standardize_boundaries returns geoid + geometry and drops water", {
  b <- sf::st_sf(
    GEOID = c("390350001001", "390359900000"),
    ALAND = c(1000, 0),
    geometry = sf::st_sfc(square(0), square(2), crs = 4269)
  )
  kept <- standardize_boundaries(b, drop_water = TRUE)
  expect_equal(kept$geoid, "390350001001")
  expect_equal(names(kept), c("geoid", "geometry"))
  expect_equal(nrow(standardize_boundaries(b, drop_water = FALSE)), 2L)

  z <- sf::st_sf(
    ZCTA5CE20 = "44101", ALAND20 = 5,
    geometry = sf::st_sfc(square(0), crs = 4269)
  )
  expect_equal(standardize_boundaries(z)$geoid, "44101")
})

test_that("planar_valid_4326 returns WGS84 geometry", {
  g <- planar_valid_4326(sf::st_sfc(square(0), crs = 4269))
  expect_equal(sf::st_crs(g)$epsg, 4326L)
})
