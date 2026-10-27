test_that("resolve_date accepts years, ISO dates, Dates and NULL", {
  expect_equal(resolve_date(2019), as.Date("2019-01-01"))
  expect_equal(resolve_date("2019"), as.Date("2019-01-01"))
  expect_equal(resolve_date("2019-06-15"), as.Date("2019-06-15"))
  expect_equal(resolve_date(as.Date("2020-03-01")), as.Date("2020-03-01"))
  expect_equal(
    resolve_date(NULL),
    as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))
  )
})

test_that("resolve_date rejects bad input", {
  expect_error(resolve_date("2019-02-30"), "valid date")
  expect_error(resolve_date("19"), "four-digit year")
  expect_error(resolve_date(c(2019, 2020)), "single")
  expect_error(resolve_date(2005), "2007")
  expect_error(resolve_date(as.integer(format(Sys.Date(), "%Y")) + 1L), "future")
})

test_that("default_boundary_year is clamped", {
  expect_equal(default_boundary_year(as.Date("2005-01-01")), 2010L)
  expect_equal(default_boundary_year(as.Date("2015-01-01")), 2015L)
  old <- options(tigris_year = 2021)
  on.exit(options(old), add = TRUE)
  expect_equal(default_boundary_year(as.Date("2025-01-01")), 2021L)
})
