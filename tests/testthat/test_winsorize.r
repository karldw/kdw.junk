context("winsorization")

test_that("winsorize works for normal sequence", {
  expect_equal(length(winsorize(1:20, 0.1)), 20)
  expect_equal(length(winsorize(20:1, 0.1)), 20)
  expect_equal(length(winsorize(20:1, 0)),   20)
  expect_equal(winsorize(1:100, 0.1), c(rep(10, 10), 11:90, rep(90, 10)))
  expect_true(setequal(winsorize(1:101, 0.1), winsorize(101:1, 0.1)))
})

test_that("winsorize doesn't change order", {
  expect_equal(winsorize(5:1, 0), 5:1)
  expect_equal(winsorize(10:1, 0.09), 10:1)
})

test_that("winsorize works with point masses", {
  x <- c(rep(0, 10), 1:10)
  expect_equal(winsorize(x, trim = 0.1), c(rep(0, 10), 1:8, 8, 8))
})

test_that("winsorize works with floats", {
  x <- c(rep(0, 10), 1:10) + 0.1
  expect_equal(winsorize(x, trim = 0.1), c(rep(0.1, 10), 1:7 + 0.1, 8.1, 8.1, 8.1))
})

test_that("winsorize works with dates", {
  x <- as.Date(1:1000, origin = "1970-01-01")
  res <- expect_silent(winsorize(x))
  expected <- as.Date(winsorize(1:1000), origin = "1970-01-01")
  expect_equal(res, expected)
})


test_that("winsorize works with different trim values", {
  x <- c(rep(0, 10), 1:10) + 0.1
  expect_equal(winsorize(x, trim = c(0.1, 0)), c(rep(0.1, 10), 1:10 + 0.1))
  expect_equal(winsorize(x, trim = c(0, 0.1)), c(rep(0.1, 10), 1:7 + 0.1, 8.1, 8.1, 8.1))
})

test_that("winsorize works with infinite values", {
  x <- c(-Inf, 2:100)
  y <- 1:100
  z <- c(1:99, Inf)
  expect_equal(winsorize(x, trim = 0.05), winsorize(y, trim = 0.05))
  expect_equal(winsorize(y, trim = 0.05), winsorize(z, trim = 0.05))
  expect_equal(winsorize(x, trim = 0.05), winsorize(z, trim = 0.05))
  expect_equal(winsorize(y, trim = c(0, 0.1)), winsorize(z, trim = c(0, 0.1)))
  expect_equal(winsorize(x, trim = c(0.1, 0)), winsorize(y, trim = c(0.1, 0)))
  expect_equal(winsorize(x, trim = 0), x)
  x <- c(1:50, rep(Inf, 50))
  expect_equal(winsorize(x, trim = c(0, 0.1)), x)
})


test_that("winsorize works with grouped dataframes", {
  skip_if_not_installed("dplyr")
  res <- dplyr::group_by(mtcars, cyl) |>
    dplyr::transmute(wt = winsorize(wt)) |>
    dplyr::ungroup() |>
    dplyr::arrange(cyl) |>
    as.data.frame() # Compat with dplyr 1.0's removal of all.equal.tbl_df
  cyl4_wins <- winsorize(mtcars[mtcars$cyl == 4, ]$wt)
  cyl6_wins <- winsorize(mtcars[mtcars$cyl == 6, ]$wt)
  cyl8_wins <- winsorize(mtcars[mtcars$cyl == 8, ]$wt)
  expected <- data.frame(cyl = sort(mtcars$cyl), wt = c(cyl4_wins, cyl6_wins, cyl8_wins))
  expect_equal(res, expected)
})
