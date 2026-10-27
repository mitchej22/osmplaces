test_that("v1 responses are parsed and aligned to the requested ids", {
  parsed <- list(groupByResult = list(
    list(groupByObject = "b", result = list(list(timestamp = "t", value = 4))),
    list(groupByObject = "a", result = list(list(timestamp = "t", value = 0)))
  ))
  res <- parse_v1_group_by_boundary(parsed)
  expect_equal(res, c(b = 4, a = 0))
  expect_equal(align_v1_counts(res, c("a", "b", "c")), c(0, 4, NA))
  expect_error(parse_v1_group_by_boundary(list()), "groupByResult")
})

test_that("align_v1_counts falls back to request order when ids are not echoed", {
  res <- c(feature1 = 3, feature2 = 5)
  expect_warning(out <- align_v1_counts(res, c("x", "y")), "request order")
  expect_equal(out, c(3, 5))
})

test_that("v2 responses are parsed", {
  txt <- '{"result":{"timestamp":["2019-01-01T00:00:00Z","2019-01-02T00:00:00Z"],"value":[5,7]}}'
  expect_equal(parse_v2_count(txt), 5)
  expect_true(is.na(parse_v2_count('{"result":{"timestamp":[],"value":[]}}')))
})

test_that("boundaries_to_geojson writes ids as an id property", {
  geojson <- boundaries_to_geojson(
    sf::st_sfc(square(0), square(2), crs = 4326), c("a", "b")
  )
  parsed <- jsonlite::fromJSON(geojson, simplifyVector = FALSE)
  expect_equal(parsed$type, "FeatureCollection")
  expect_length(parsed$features, 2L)
  expect_equal(parsed$features[[1]]$properties$id, "a")
  expect_equal(parsed$features[[2]]$properties$id, "b")
})

test_that("ohsome_client requires a key for v2", {
  withr_key <- Sys.getenv("OHSOME_API_KEY", unset = NA)
  Sys.unsetenv("OHSOME_API_KEY")
  on.exit(if (!is.na(withr_key)) Sys.setenv(OHSOME_API_KEY = withr_key), add = TRUE)
  expect_error(ohsome_client("v2"), "API key")
  expect_equal(ohsome_client("v2", api_key = "k")$api_key, "k")
})

test_that("count_places_v1 maps returned ids and leaves missing ones NA", {
  boundaries <- list(ids = c("a", "b"), geom = sf::st_sfc(square(0), square(2), crs = 4326))
  client <- suppressWarnings(ohsome_client("v1", verbose = FALSE))

  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(groupByResult = list(
      list(groupByObject = "b", result = list(list(value = 4)))
    )))
  })

  out <- count_places_v1(boundaries, "amenity=pharmacy", as.Date("2019-01-01"), client)
  expect_equal(out, c(NA, 4))
})

test_that("count_places_v2 reads the first snapshot value", {
  boundaries <- list(ids = "a", geom = sf::st_sfc(square(0), crs = 4326))
  client <- ohsome_client("v2", api_key = "k", verbose = FALSE)

  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(
      timestamp = list("2019-01-01T00:00:00Z", "2019-01-02T00:00:00Z"),
      value = list(9, 10)
    )))
  })

  out <- count_places_v2(boundaries, "amenity=pharmacy", as.Date("2019-01-01"), client)
  expect_equal(out, 9)
})
