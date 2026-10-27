make_places_sf <- function() {
  sf::st_sf(
    geoid = c("a", "b", "c"),
    pharmacy = c(0L, 2L, 9L),
    bank = c(1L, 1L, 1L),
    geometry = sf::st_sfc(square(0), square(1), square(2), crs = 4326)
  )
}

test_that("map_places requires an sf object", {
  expect_error(map_places(data.frame(geoid = "a", pharmacy = 1L)), "geometry = TRUE")
})

test_that("place_count_columns finds numeric columns and validates tags", {
  x <- make_places_sf()
  expect_equal(place_count_columns(x), c("pharmacy", "bank"))
  expect_equal(place_count_columns(x, "bank"), "bank")
  expect_error(place_count_columns(x, "nope"), "Not a count column")

  attr(x, "tags") <- "pharmacy"
  expect_equal(place_count_columns(x), "pharmacy")
})

test_that("static map returns a ggplot", {
  skip_if_not_installed("ggplot2")
  x <- make_places_sf()
  expect_s3_class(map_places(x, "pharmacy", interactive = FALSE), "ggplot")
  expect_s3_class(map_places(x, interactive = FALSE), "ggplot")
  expect_s3_class(map_places(x, "bank", interactive = FALSE), "ggplot")
})

test_that("count_classes separates zeros, missing values and quantile classes", {
  skip_if_not_installed("leaflet")
  cl <- count_classes(c(0, 0, 1, 2, 3, 10, NA))
  expect_length(cl$fill, 7L)
  expect_equal(cl$labels[[1]], "0")
  expect_equal(cl$labels[[length(cl$labels)]], "No data")
  expect_equal(cl$fill[[1]], cl$fill[[2]])
  expect_false(cl$fill[[1]] == cl$fill[[6]])
  expect_equal(length(cl$colors), length(cl$labels))
  expect_true(any(grepl("10", cl$labels)))
  expect_false(anyDuplicated(cl$colors) > 0)

  expect_equal(count_classes(c(0, 0, 0))$labels, "0")
  expect_equal(count_classes(c(NA_real_, NA_real_))$labels, "No data")
  single <- count_classes(c(0, 4, 4, 4))
  expect_equal(single$labels, c("0", "1\u20134"))
})

test_that("places_popups lists only non-zero tags, largest first", {
  counts <- data.frame(pharmacy = c(0L, 3L), grocery_store = c(0L, 12L))
  pop <- places_popups(c("a", "b"), counts)
  expect_match(pop[[1]], "no places")
  expect_match(pop[[2]], "GEOID b")
  expect_match(pop[[2]], "grocery store.*12.*pharmacy.*3")
})

test_that("add_basemap validates its argument", {
  skip_if_not_installed("leaflet")
  expect_error(map_places(make_places_sf(), basemap = "nope"), "basemap")
  expect_error(map_places(make_places_sf(), basemap = 1), "basemap")
})

test_that("interactive map returns a leaflet widget with the tag payload", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")
  x <- make_places_sf()
  attr(x, "date") <- "2019-01-01"

  m <- map_places(x)
  expect_s3_class(m, "leaflet")
  payload <- m$jsHooks$render[[1]]$data
  expect_equal(unclass(payload$ids), c("a", "b", "c"))
  expect_equal(unclass(payload$display), c("pharmacy", "bank"))
  expect_equal(unclass(payload$totals), c(11, 3))
  expect_equal(payload$date, "2019-01-01")
  expect_length(payload$fill, 2L)

  expect_s3_class(map_places(x, "pharmacy", basemap = "none"), "leaflet")
  expect_s3_class(map_places(x, basemap = "osm"), "leaflet")
  expect_s3_class(
    map_places(x, basemap = "https://tiles.example.com/{z}/{x}/{y}.png"),
    "leaflet"
  )
})
