test_that("get_places validates arguments before any network call", {
  expect_error(get_places(), "state")
  expect_error(get_places(state = "OH", bbox = c(-82, 41, -81, 42)), "not both")
  expect_error(get_places(county = "Cuyahoga", bbox = NULL), "state")
  expect_error(get_places(state = c("OH", "PA")), "single state")
  expect_error(get_places(state = "OH", geography = "county"), "Unknown geography")
  expect_error(get_places(state = "OH", tags = "nope"), "Unknown tag")
  expect_error(get_places(state = "OH", year = "20x9"), "four-digit year")
  expect_error(get_places(state = "OH", chunk_size = 0), "chunk_size")
  expect_error(get_places(state = "OH", api = "v2", api_key = ""), "API key")
})

test_that("live: Cuyahoga tracts", {
  skip_if_not(identical(Sys.getenv("OSMPLACES_LIVE"), "true"),
              "Set OSMPLACES_LIVE=true to run live API tests.")

  res <- get_places(
    state = "OH", county = "Cuyahoga", year = 2019,
    tags = c("pharmacy", "bank"), geography = "tract", verbose = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_true(all(c("geoid", "pharmacy", "financial_mainstream") %in% names(res)))
  expect_gt(sum(res$pharmacy, na.rm = TRUE), 0)
  expect_equal(attr(res, "date"), "2019-01-01")

  sf_res <- get_places(
    state = "OH", county = "Cuyahoga", year = 2019, tags = "pharmacy",
    geography = "tract", geometry = TRUE, verbose = FALSE
  )
  expect_s3_class(sf_res, "sf")
})
