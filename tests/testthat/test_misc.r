context("test other functions")

test_that("truncate_bytes works for ascii", {

  res <- truncate_bytes(c("ab", "cde"), c(1, 2))
  expect_equal(res, c("a", "cd"))


})

test_that("truncate_bytes works for non-ascii", {
  latin1_str <- "fa\xE7ile"
  Encoding(latin1_str) <- "latin1"
  latin1_str_truncated <- "fa\xE7"
  Encoding(latin1_str_truncated) <- "latin1"
  expect_equal(truncate_bytes(latin1_str, 3), latin1_str_truncated)
  expect_equal(truncate_bytes("ὯaὯa", 2), "")
  expect_equal(truncate_bytes("ὯaὯa", 6), "Ὧa")
})
