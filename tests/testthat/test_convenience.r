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

test_that("plotting code runs", {
  skip_if_not_installed("ggplot2")
  plt <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  temp_pdf <- tempfile(fileext = ".pdf")
  save_plot(plt, temp_pdf)
  expect_true(file.exists(temp_pdf))

  skip_if_not_installed("tikzDevice")
  temp_tikz <- tempfile(fileext=".tikz")
  save_plot(plt, temp_tikz)
  expect_true(file.exists(temp_tikz))
})


# plot_dev ---------------------------------------------------------------------
# copied from ggplot2's tests.
test_that("function is passed back unchanged", {
  expect_equal(plot_dev(png), png)
})

test_that("unknown device triggers error", {
  expect_error(plot_dev("xyz"), "Unknown graphics device")
  expect_error(plot_dev(NULL, "test.xyz"), "Unknown graphics device")
})

test_that("text converted to function", {
  expect_identical(body(plot_dev("png"))[[1]], quote(grDevices::png))
})

test_that("if device is NULL, guess from extension", {
  expect_identical(body(plot_dev(NULL, "test.png"))[[1]], quote(grDevices::png))
})
