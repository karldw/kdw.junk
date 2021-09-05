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

test_that("rename_cols works for remote tables", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dplyr")
  remote_tbl <- dbplyr::memdb_frame(mpg = 1:5, cyl = 2:6)
  my_rename_remote <- rename_cols(remote_tbl,
    c("mpg_rename" = "mpg", "cyl_rename" = "cyl"))
  dplyr_rename_remote <- dplyr::rename(remote_tbl, mpg_rename = mpg, cyl_rename = cyl)
  expect_equal(my_rename_remote, dplyr_rename_remote)
  expect_error(rename_cols(remote_tbl, c(x = "not a column")))
  expect_error(rename_cols(remote_tbl, c(x = "cyl", y = "cyl")))
})

test_that("rename_cols works for local tables", {
  skip_if_not_installed("dplyr")
  my_rename <- rename_cols(mtcars,
    c("mpg_rename" = "mpg", "cyl_rename" = "cyl"))
  dplyr_rename <- dplyr::rename(mtcars, mpg_rename = mpg, cyl_rename = cyl)
  expect_equal(my_rename, dplyr_rename)
  expect_error(rename_cols(mtcars, c(x = "not a column")))
  expect_error(rename_cols(mtcars, c(x = "cyl", y = "cyl")))
})

test_that("make_better_names works for a small example", {
  res <- make_better_names(c("Country", "GDP $M", "Coast.Length"))
  expect_equal(res, c("country", "gdp_mn", "coast_length"))
})

test_that("rename_cols works for single-col renames", {
  skip_if_not_installed("dplyr")
  res <- rename_cols(mtcars, c("cyl2" = "cyl"))
  expected <- dplyr::rename(mtcars, cyl2 = cyl)
  expect_equal(expected, res)
})

test_that("rename_cols works for multi-col renames", {
  skip_if_not_installed("dplyr")
  res <- rename_cols(mtcars, c("cyl2" = "cyl", "mpg2" = "mpg"))
  expected <- dplyr::rename(mtcars, cyl2 = cyl, mpg2 = mpg)
  expect_equal(expected, res)
})
# TODO: should test renaming remote tables.
