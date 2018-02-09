context("uniqueness functions")

test_that("is_id (SE version) works", {
  expect_false(is_id(mtcars, "cyl"))
  expect_true(is_id(dplyr::distinct(mtcars, cyl), "cyl"))
})

test_that("is_id (NSE version) works", {
  # Without .data$
  expect_false(is_id(mtcars, cyl))
  expect_true(is_id(dplyr::distinct(mtcars, .data$cyl), cyl))

  # With .data$
  expect_false(is_id(mtcars, .data$cyl))
  expect_true(is_id(dplyr::distinct(mtcars, .data$cyl), .data$cyl))
})
