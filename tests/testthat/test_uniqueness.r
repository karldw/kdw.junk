context("uniqueness functions")

test_that("is_id (SE version) works", {
  expect_false(is_id(Loblolly, "Seed"))
  expect_true(is_id(Loblolly, "Seed", "age"))
  expect_true(is_id(Loblolly, c("Seed", "age")))

  vars <- c("Seed", "age")
  # Avoid messages related to https://github.com/r-lib/tidyselect/issues/76
  # This error only comes up the first time the tests are run.
  expect_silent(is_id(Loblolly, !!vars))
  expect_false(is_id(Loblolly, !!vars[1]))
  expect_true(is_id(Loblolly, !!vars))

})

test_that("is_id (NSE version) works", {
  # Without .data$
  expect_false(is_id(Loblolly, Seed))
  expect_true(is_id(Loblolly, Seed, age))

  # With .data$
  expect_false(is_id(Loblolly, .data$Seed))
  expect_true(is_id(Loblolly, .data$Seed, .data$age))

  ## !! syntax
  Seed <- c("Seed", "age")
  expect_true(is_id(Loblolly, !!Seed))
  expect_true(is_id(Loblolly, !!!Seed))
})


test_that("is_id works for memdb", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("RSQLite")

  Loblolly <- dbplyr::memdb_frame(!!!Loblolly) # like tbl_memdb, but assign a unique name
  # SE tests
  expect_false(is_id(Loblolly, "Seed"))
  expect_true(is_id(Loblolly, "Seed", "age"))
  expect_true(is_id(Loblolly, c("Seed", "age")))
  vars <- c("Seed", "age")
  expect_false(is_id(Loblolly, vars[1]))
  expect_true(is_id(Loblolly, vars))

  # NSE tests
  # Without .data$
  expect_false(is_id(Loblolly, Seed))
  expect_true(is_id(Loblolly, Seed, age))

  # With .data$
  expect_false(is_id(Loblolly, .data$Seed))
  expect_true(is_id(Loblolly, .data$Seed, .data$age))

  ## !! syntax
  expect_true(is_id(Loblolly, !!vars))
  expect_true(is_id(Loblolly, !!!vars))
})

test_that("is_id works for sf objects", {
  skip_if_not_installed("sf")
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))
  expect_true(is_id(nc, CNTY_ID))
  expect_false(is_id(nc, SID74))
  CNTY_ID <- "SID74"
  expect_false(is_id(nc, !!CNTY_ID))
})
