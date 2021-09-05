context("testing join functions")
#
# test_that("make_join_safer works for inner_join", {
#   skip_if_not_installed("dplyr")
#   my_join <- make_join_safer(dplyr::inner_join)
#   mtcars_cyl <- dplyr::select(mtcars, cyl)
#   mtcars_uniq <- dplyr::distinct(mtcars_cyl, cyl)
#
#   expect_equal(
#     dplyr::inner_join(mtcars_cyl, mtcars_uniq, by = "cyl"),
#     my_join(mtcars_cyl, mtcars_uniq, by = "cyl")
#     )
#   expect_error(my_join(mtcars_cyl, mtcars_cyl, by = "cyl"))
# })
#
# test_that("make_join_safer works for full_join", {
#   skip_if_not_installed("dplyr")
#   my_join <- make_join_safer(dplyr::full_join)
#   mtcars_cyl <- dplyr::select(mtcars, cyl)
#   mtcars_uniq <- dplyr::distinct(mtcars_cyl, cyl)
#
#   expect_equal(
#     dplyr::full_join(mtcars_cyl, mtcars_uniq, by = "cyl"),
#     my_join(mtcars_cyl, mtcars_uniq, by = "cyl")
#     )
#   expect_error(my_join(mtcars_cyl, mtcars_cyl, by = "cyl"))
# })


set_up_db <- function() {
  # set up mtcars_db
  con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
  dplyr::copy_to(con, mtcars, "mtcars")
  con
}

test_that("nrow(x, force = TRUE) works", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dplyr")
  con <- set_up_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  mtcars_db <- dplyr::tbl(con, "mtcars")
  expect_equal(base::nrow(mtcars), nrow(mtcars))
  expect_equal(base::nrow(mtcars), nrow(mtcars, force = TRUE))
  expect_equal(NA_real_, nrow(mtcars_db, force = FALSE))
  expect_equal(base::nrow(mtcars), nrow(mtcars_db, force = TRUE))
})
