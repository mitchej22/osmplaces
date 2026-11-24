test_that("get_places validates arguments before any network call", {
  expect_error(get_places(), "state")
  expect_error(get_places(state = "OH", bbox = c(-82, 41, -81, 42)), "not both")
  expect_error(get_places(county = "Cuyahoga", bbox = NULL), "state")
  expect_error(get_places(state = c("OH", "PA")), "single state")
  expect_error(get_places(state = "OH", geography = "county"), "Unknown geography")
  expect_error(get_places(state = "OH", tags = "nope"), "Unknown tag")
  expect_error(get_places(state = "OH", domains = "nope"), "Unknown domain")
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

test_that("domain-based filtering works", {
  skip_if_not(identical(Sys.getenv("OSMPLACES_LIVE"), "true"),
              "Set OSMPLACES_LIVE=true to run live API tests.")

  # Test single domain
  res_domain <- get_places(
    state = "OH", county = "Cuyahoga", year = 2019,
    domains = "food_environment", geography = "tract", verbose = FALSE
  )
  expect_s3_class(res_domain, "data.frame")
  expect_true("geoid" %in% names(res_domain))
  expect_true(all(c("grocery", "convenience_store", "fast_food", "restaurant_cafe") %in% names(res_domain)))
  expect_false("pharmacy" %in% names(res_domain))

  # Test multiple domains
  res_domains <- get_places(
    state = "OH", county = "Cuyahoga", year = 2019,
    domains = c("food_environment", "health_services"), geography = "tract", verbose = FALSE
  )
  expect_true(all(c("grocery", "pharmacy", "healthcare") %in% names(res_domains)))
  expect_false("fitness" %in% names(res_domains))

  # Test that domains parameter takes precedence over tags
  res_domain_precedence <- get_places(
    state = "OH", county = "Cuyahoga", year = 2019,
    tags = "pharmacy", domains = "food_environment", geography = "tract", verbose = FALSE
  )
  expect_true("grocery" %in% names(res_domain_precedence))
  expect_false("pharmacy" %in% names(res_domain_precedence))
})

