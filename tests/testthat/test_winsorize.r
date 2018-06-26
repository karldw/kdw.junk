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
