test_that("the bundled taxonomy loads", {
  tx <- read_taxonomy()
  expect_true(all(c("grocery", "pharmacy", "childcare", "industrial") %in% names(tx)))
  expect_equal(tx$convenience_store$geometry_types, c("node", "way"))
  expect_equal(tx$grocery$tag_specs[[1]]$exclude[[1]]$value, "yes")

  tbl <- places_taxonomy()
  expect_s3_class(tbl, "data.frame")
  expect_equal(tbl$label, names(tx))
  expect_true(all(nzchar(tbl$ohsome_filter)))
})

test_that("resolve_tags handles all, aliases, case and duplicates", {
  tx <- read_taxonomy()
  expect_equal(resolve_tags("all", tx), names(tx))
  expect_equal(resolve_tags("ALL", tx), names(tx))
  expect_equal(resolve_tags("bank", tx), "financial_mainstream")
  expect_equal(resolve_tags(c("Pharmacy", "pharmacy", "gym"), tx), c("pharmacy", "fitness"))
  expect_error(resolve_tags("nope", tx), "Unknown tag")
  expect_error(resolve_tags(character(0), tx), "at least one")
})

test_that("resolve_domains resolves domain names to labels", {
  tx <- read_taxonomy()

  # Single domain
  labels <- resolve_domains("food_environment", tx)
  expect_true(all(c("grocery", "convenience_store", "fast_food", "restaurant_cafe") %in% labels))
  expect_false("pharmacy" %in% labels)

  # Multiple domains
  labels <- resolve_domains(c("food_environment", "health_services"), tx)
  expect_true(all(c("grocery", "pharmacy", "healthcare") %in% labels))
  expect_false("fitness" %in% labels)

  # Case insensitive
  labels <- resolve_domains("Food_Environment", tx)
  expect_true("grocery" %in% labels)

  # With underscores and spaces (normalized)
  labels <- resolve_domains("food environment", tx)
  expect_true("grocery" %in% labels)

  # Invalid domain
  expect_error(resolve_domains("invalid_domain", tx), "Unknown domain")

  # NULL domains returns NULL
  expect_null(resolve_domains(NULL, tx))
})

test_that("build_ohsome_filter produces ohsome filter expressions", {
  tx <- read_taxonomy()
  expect_equal(
    build_ohsome_filter(tx$convenience_store),
    "(type:node or type:way) and ((shop=convenience and not (disused=yes)))"
  )
  expect_equal(
    build_ohsome_filter(tx$grocery),
    paste0(
      "(type:node or type:way or type:relation) and ",
      "((shop in (supermarket, greengrocer, health_food, organic) and not (disused=yes)) ",
      "or amenity=marketplace)"
    )
  )
  expect_equal(
    build_ohsome_filter(tx$industrial),
    "(type:way or type:relation) and (landuse=industrial or man_made=works)"
  )
})

test_that("build_ohsome_filter quotes unusual tokens", {
  pt <- list(
    geometry_types = "node",
    tag_specs = list(list(key = "social_facility:for", value = "senior citizens", exclude = list()))
  )
  expect_equal(
    build_ohsome_filter(pt),
    "(type:node) and (\"social_facility:for\"=\"senior citizens\")"
  )
})
