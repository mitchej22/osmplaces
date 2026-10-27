skip_if_not_installed("accessibility")

# Three squares in a row, ~833 m apart at this latitude (A-B, B-C) and ~1,666 m
# for A-C. With a 1,000 m catchment, A and C each reach B but not each other.
make_access_sf <- function(grocery = c(10L, 0L, 5L), pop = c(100, 50, 25)) {
  sf::st_sf(
    geoid = c("A", "B", "C"),
    grocery = grocery,
    pop = pop,
    geometry = sf::st_sfc(
      square(-81.50, 41.5, 0.002), square(-81.49, 41.5, 0.002),
      square(-81.48, 41.5, 0.002), crs = 4326
    )
  )
}

# Hand calculation for the default data and a 1,000 m binary catchment:
#   R_A = 10 / (100 + 50) = 1/15, R_B = 0, R_C = 5 / (50 + 25) = 1/15
#   score_A = R_A + R_B = 1/15
#   score_B = R_A + R_B + R_C = 2/15
#   score_C = R_B + R_C = 1/15
hand_scores <- c(1, 2, 1) / 15

calc <- function(x, ...) {
  calc_access(x, population = "pop", catchment = 1000, distance_units = "meters",
              per = 1, verbose = FALSE, ...)
}

test_that("2SFCA matches the hand calculation", {
  res <- calc(make_access_sf(), tag = "grocery")
  expect_equal(res$access_grocery, hand_scores, tolerance = 1e-8)
  expect_s3_class(res, "sf")
  expect_equal(res$geoid, c("A", "B", "C"))
})

test_that("scores are per 1,000 residents by default", {
  res <- calc_access(make_access_sf(), "grocery", population = "pop",
                     catchment = 1000, distance_units = "meters", verbose = FALSE)
  expect_equal(res$access_grocery, hand_scores * 1000, tolerance = 1e-6)
})

test_that("catchment units are converted", {
  x <- make_access_sf()
  miles <- calc_access(x, "grocery", population = "pop", catchment = 1000 / 1609.344,
                       distance_units = "miles", per = 1, verbose = FALSE)
  km <- calc_access(x, "grocery", population = "pop", catchment = 1,
                    distance_units = "km", per = 1, verbose = FALSE)
  expect_equal(miles$access_grocery, hand_scores, tolerance = 1e-8)
  expect_equal(km$access_grocery, hand_scores, tolerance = 1e-8)

  wide <- calc(x, "grocery")
  small <- calc_access(x, "grocery", population = "pop", catchment = 500,
                       distance_units = "meters", per = 1, verbose = FALSE)
  # Nothing but each geography itself is within 500 m.
  expect_equal(small$access_grocery, c(10 / 100, 0, 5 / 25), tolerance = 1e-8)
  expect_false(isTRUE(all.equal(wide$access_grocery, small$access_grocery)))
})

test_that("results keep geometry, attributes, and record how scores were made", {
  x <- make_access_sf()
  attr(x, "date") <- "2019-01-01"
  attr(x, "boundary_year") <- 2019L
  attr(x, "tags") <- "grocery"

  res <- calc(x)
  expect_equal(attr(res, "date"), "2019-01-01")
  expect_equal(attr(res, "boundary_year"), 2019L)
  expect_equal(attr(res, "tags"), "grocery")
  expect_equal(attr(res, "access_columns"), "access_grocery")
  spec <- attr(res, "access")$access_grocery
  expect_equal(spec$method, "2sfca")
  expect_match(spec$label, "2SFCA: places per resident, 1000 meters catchment")
  expect_equal(sf::st_geometry(res), sf::st_geometry(x))
  expect_equal(res$grocery, x$grocery)
})

test_that("tags default to the tags attribute and can be scored together", {
  x <- make_access_sf()
  x$pharmacy <- c(0L, 3L, 0L)
  expect_error(calc(x), "Pass them as `tag`")

  attr(x, "tags") <- c("grocery", "pharmacy")
  res <- calc(x)
  expect_equal(attr(res, "access_columns"), c("access_grocery", "access_pharmacy"))
  expect_equal(res$access_grocery, hand_scores, tolerance = 1e-8)
  # R_B = 3 / (100 + 50 + 25) reaches everyone within 1,000 m of B
  expect_equal(res$access_pharmacy, rep(3 / 175, 3), tolerance = 1e-8)
})

test_that("rerunning keeps earlier access columns tracked", {
  res <- calc(make_access_sf(), "grocery")
  res$pharmacy <- c(0L, 3L, 0L)
  res2 <- calc(res, "pharmacy")
  expect_equal(attr(res2, "access_columns"), c("access_grocery", "access_pharmacy"))
  expect_named(attr(res2, "access"), c("access_grocery", "access_pharmacy"))
})

test_that("a supplied travel matrix replaces straight-line distance", {
  x <- make_access_sf()
  tm <- data.frame(
    from_id = c("A", "B", "B", "C"), to_id = c("B", "A", "C", "B"),
    minutes = c(10, 10, 10, 10)
  )
  # No self rows on purpose: they are added at cost 0.
  res <- calc_access(sf::st_drop_geometry(x), "grocery", population = "pop",
                     catchment = 15, travel_matrix = tm, travel_cost = "minutes",
                     per = 1, verbose = FALSE)
  expect_equal(res$access_grocery, hand_scores, tolerance = 1e-8)
  expect_match(attr(res, "access")$access_grocery$label, "15 travel cost units")

  # Only one cost column, so travel_cost can be omitted.
  res2 <- calc_access(sf::st_drop_geometry(x), "grocery", population = "pop",
                      catchment = 15, travel_matrix = tm, per = 1, verbose = FALSE)
  expect_equal(res2$access_grocery, hand_scores, tolerance = 1e-8)

  # Pairs slower than the catchment are ignored. With B->C out of reach, B no
  # longer counts C's supply (score_B = R_A + R_B = 1/15) and C's catchment
  # loses B's people (R_C = 5/25, score_C = R_B + R_C = 0.2).
  far <- tm
  far$minutes[far$from_id == "B" & far$to_id == "C"] <- 40
  res3 <- calc_access(sf::st_drop_geometry(x), "grocery", population = "pop",
                      catchment = 15, travel_matrix = far, per = 1, verbose = FALSE)
  expect_equal(res3$access_grocery, c(1 / 15, 1 / 15, 0.2), tolerance = 1e-8)
})

test_that("travel matrices are validated", {
  x <- sf::st_drop_geometry(make_access_sf())
  run <- function(tm, ...) {
    calc_access(x, "grocery", population = "pop", catchment = 15, travel_matrix = tm,
                verbose = FALSE, ...)
  }
  expect_error(run(data.frame(a = 1)), "from_id")
  expect_error(run(data.frame(from_id = "A", to_id = "B", m1 = 1, m2 = 2)), "travel_cost")
  expect_error(run(data.frame(from_id = "A", to_id = "B", m = 1), travel_cost = "zzz"),
               "must name a column")
  expect_error(run(data.frame(from_id = "Z", to_id = "Y", m = 1)), "match")
  dup <- data.frame(from_id = c("A", "A"), to_id = c("B", "B"), m = c(1, 2))
  expect_error(run(dup), "one row per")
})

test_that("geographies whose catchments hold no people get NA", {
  res <- calc(make_access_sf(grocery = c(10L, 0L, 0L), pop = c(0, 0, 25)), "grocery")
  expect_equal(res$access_grocery, c(NA, NA, 0))
})

test_that("population can be a data frame of geoids", {
  x <- make_access_sf()
  pop <- data.frame(GEOID = c("C", "A", "B"), population = c(25, 100, 50))
  res <- calc_access(x, "grocery", population = pop, catchment = 1000,
                     distance_units = "meters", per = 1, verbose = FALSE)
  expect_equal(res$access_grocery, hand_scores, tolerance = 1e-8)
  expect_equal(res$population, c(100, 50, 25))

  partial <- pop[pop$GEOID != "B", ]
  expect_warning(
    res2 <- calc_access(x, "grocery", population = partial, catchment = 1000,
                        distance_units = "meters", per = 1, verbose = FALSE),
    "no population"
  )
  expect_equal(res2$population, c(100, 0, 25))
})

test_that("missing supply is counted as 0 with a warning", {
  x <- make_access_sf()
  x$grocery[3] <- NA
  expect_warning(res <- calc(x, "grocery"), "lower bounds")
  # R_A = 10 / 150, R_B = R_C = 0
  expect_equal(res$access_grocery, c(10 / 150, 10 / 150, 0), tolerance = 1e-8)
})

test_that("decay functions and the balanced method are passed through", {
  x <- make_access_sf()
  binary <- calc(x, "grocery")
  decayed <- calc(x, "grocery", decay = accessibility::decay_exponential(0.002))

  # Adjacent geographies are weighted w = exp(-0.002 * d); A-C is outside the
  # catchment. R_A = 10 / (100 + 50 w), R_B = 0, R_C = 5 / (25 + 50 w).
  d <- haversine_m(-81.499, 41.501, -81.489, 41.501)
  w <- exp(-0.002 * d)
  r_a <- 10 / (100 + 50 * w)
  r_c <- 5 / (25 + 50 * w)
  expect_equal(decayed$access_grocery, c(r_a, w * r_a + w * r_c, r_c), tolerance = 1e-6)
  expect_false(isTRUE(all.equal(decayed$access_grocery, binary$access_grocery)))
  bal <- calc(x, "grocery", method = "bfca")
  expect_true(all(is.finite(bal$access_grocery)))
  expect_false(isTRUE(all.equal(bal$access_grocery, binary$access_grocery)))
  expect_equal(attr(bal, "access")$access_grocery$method, "bfca")
})

test_that("calc_access validates its arguments", {
  x <- make_access_sf()
  expect_error(calc_access(data.frame(a = 1), population = "a", catchment = 1),
               "geoid")
  expect_error(calc_access(x, "grocery", catchment = 1), "`population` is required")
  expect_error(calc_access(x, "grocery", population = "pop"), "`catchment` is required")
  expect_error(calc_access(x, "grocery", population = "pop", catchment = -1), "positive")
  expect_error(calc_access(x, "grocery", population = "pop", catchment = 1, per = 0),
               "positive")
  expect_error(calc_access(x, "grocery", population = "pop", catchment = 1,
                           decay = "exp"), "must be NULL or a function")
  expect_error(calc_access(x, "nope", population = "pop", catchment = 1), "Not a numeric")
  expect_error(calc_access(x, "geoid", population = "pop", catchment = 1), "Not a numeric")
  expect_error(calc_access(x, "grocery", population = "zzz", catchment = 1),
               "not found")
  expect_error(calc_access(x, "grocery", population = 1:3, catchment = 1),
               "column name")

  neg <- x
  neg$pop[1] <- -5
  expect_error(calc_access(neg, "grocery", population = "pop", catchment = 1),
               "negative")

  dup <- x
  dup$geoid[2] <- "A"
  expect_error(calc_access(dup, "grocery", population = "pop", catchment = 1), "unique")

  expect_error(
    calc_access(sf::st_drop_geometry(x), "grocery", population = "pop", catchment = 1),
    "no geometry"
  )
})

test_that("haversine distances are sensible", {
  expect_equal(haversine_m(0, 0, 0, 1), 6371010 * pi / 180, tolerance = 1e-9)
  expect_equal(haversine_m(-81.5, 41.5, -81.5, 41.5), 0)
})

test_that("straight-line costs include self pairs and respect the catchment", {
  x <- make_access_sf()
  costs <- straight_line_costs(sf::st_geometry(x), x$geoid, 1000, 1)
  key <- paste(costs$from_id, costs$to_id)
  expect_true(all(c("A A", "B B", "C C", "A B", "B A", "B C", "C B") %in% key))
  expect_false(any(c("A C", "C A") %in% key))
  expect_true(all(costs$travel_cost <= 1000))
  expect_equal(costs$travel_cost[key == "A B"], 832.8, tolerance = 0.01)
  expect_equal(costs$travel_cost[key == "A A"], 0)
})

test_that("empty or CRS-less geometry is rejected", {
  x <- make_access_sf()
  geom <- sf::st_geometry(x)
  sf::st_crs(geom) <- NA
  expect_error(straight_line_costs(geom, x$geoid, 1000, 1), "coordinate reference")
  empty <- sf::st_sfc(sf::st_polygon(), square(1), crs = 4326)
  expect_error(straight_line_costs(empty, c("a", "b"), 1000, 1), "empty")
})

test_that("map_places maps access scores by default", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")
  x <- make_access_sf()
  x$pharmacy <- c(0L, 3L, 0L)
  attr(x, "tags") <- c("grocery", "pharmacy")
  res <- calc_access(x, population = "pop", catchment = 1000, distance_units = "meters",
                     verbose = FALSE)

  expect_equal(place_count_columns(res), c("access_grocery", "access_pharmacy"))
  expect_equal(place_count_columns(res, "grocery"), "grocery")

  m <- map_places(res, basemap = "none")
  expect_s3_class(m, "leaflet")
  payload <- m$jsHooks$render[[1]]$data
  expect_equal(unclass(payload$display), c("access grocery", "access pharmacy"))
  expect_equal(unclass(payload$isCount), c(FALSE, FALSE))
  expect_equal(payload$title, "Accessibility")
  expect_match(unclass(payload$notes)[[1]], "2SFCA: places per 1,000 residents")

  both <- map_places(res, c("grocery", "access_grocery"), basemap = "none")
  p2 <- both$jsHooks$render[[1]]$data
  expect_equal(unclass(p2$isCount), c(TRUE, FALSE))
  expect_equal(p2$title, "Place type")
  expect_equal(unclass(p2$notes)[[1]], "")
})

test_that("static maps work for access scores", {
  skip_if_not_installed("ggplot2")
  x <- make_access_sf()
  res <- calc_access(x, "grocery", population = "pop", catchment = 1000,
                     distance_units = "meters", verbose = FALSE)
  expect_s3_class(map_places(res, interactive = FALSE), "ggplot")
})

test_that("count_classes handles fractional scores", {
  skip_if_not_installed("leaflet")
  vals <- c(0, 0, 0.012, 0.05, 0.2, 0.5, 1.3, NA)
  cl <- count_classes(vals)
  expect_equal(length(cl$colors), length(cl$labels))
  expect_equal(cl$labels[[1]], "0")
  expect_equal(cl$labels[[length(cl$labels)]], "No data")
  expect_true(any(grepl("\u2264", cl$labels)))
  expect_false(anyDuplicated(cl$labels) > 0)
  expect_equal(cl$fill[[1]], cl$fill[[2]])

  # Bounds that agree to 2 digits get more digits instead of duplicate labels.
  close <- count_classes(c(0.1201, 0.1202, 0.1203, 0.1204, 0.1205))
  expect_false(anyDuplicated(close$labels) > 0)

  expect_equal(count_classes(c(0, 0.25, 0.25))$labels, c("0", "0.25"))
})

test_that("fmt_values keeps counts exact and rounds fractions", {
  expect_equal(fmt_values(c(12345, 0.037152, 1234.5678, NA)),
               c("12,345", "0.0372", "1,230", "n/a"))
  pop <- places_popups("a", data.frame(access_grocery = 0.123456), empty = "no access")
  expect_match(pop, "0.123")
  expect_match(places_popups("a", data.frame(access_grocery = 0), empty = "no access"),
               "no access")
})
