context("convenience functions")

test_that("make_monthly works as expected", {
  expect_equal(
    make_monthly(as.Date(c("2000-01-01", "2000-01-02"))),
    as.Date(c("2000-01-01", "2000-01-01"))
  )
})


test_that("make_better_names works as expected", {
  orig_names <- c("Country", "GDP $M", "Coast.Length")
  improved_names <- c("country", "gdp_mn", "coast_length")
  expect_equal(make_better_names(orig_names), improved_names)

  orig_names <- c("a and b", "a-and-b", "a.and.b", "a_and_b")
  improved_names <- c("a_and_b", "a_and_b_1", "a_and_b_2", "a_and_b_3")
  expect_equal(make_better_names(orig_names), improved_names)
  # Don't do this:
  orig_names <- c("", "x", "X", "x_1")
  improved_names <- c("x", "x_1", "x_2", "x_1_1")
  expect_equal(make_better_names(orig_names), improved_names)
})

test_that("memory_limit runs", {
  skip_on_os("mac")
  skip_if_not_installed("unix")
  expect_equal(memory_limit(NA), Inf)
  expect_equal(memory_limit(NULL), Inf)
  memory_limit(Inf)
})
