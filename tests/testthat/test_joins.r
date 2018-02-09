context("testing join functions")

test_that("make_join_safer works for inner_join", {
    my_join <- make_join_safer(dplyr::inner_join)
    mtcars_cyl <- dplyr::select(mtcars, cyl)
    mtcars_uniq <- dplyr::distinct(mtcars_cyl, cyl)

    expect_equal(
      dplyr::inner_join(mtcars_cyl, mtcars_uniq, by = "cyl"),
      my_join(mtcars_cyl, mtcars_uniq, by = "cyl")
      )
    expect_error(my_join(mtcars_cyl, mtcars_cyl, by = "cyl"))
})

test_that("make_join_safer works for full_join", {
    my_join <- make_join_safer(dplyr::full_join)
    mtcars_cyl <- dplyr::select(mtcars, cyl)
    mtcars_uniq <- dplyr::distinct(mtcars_cyl, cyl)

    expect_equal(
      dplyr::full_join(mtcars_cyl, mtcars_uniq, by = "cyl"),
      my_join(mtcars_cyl, mtcars_uniq, by = "cyl")
      )
    expect_error(my_join(mtcars_cyl, mtcars_cyl, by = "cyl"))
})


# set up mtcars_db
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
dplyr::copy_to(con, mtcars, "mtcars")
mtcars_db <- dplyr::tbl(con, "mtcars")

test_that("force_nrow works", {
  expect_equal(nrow(mtcars), force_nrow(mtcars))
  expect_equal(nrow(mtcars), force_nrow(mtcars_db))
})

test_that("force_names works", {
  expect_equal(names(mtcars), force_names(mtcars))
  expect_equal(names(mtcars), force_names(mtcars_db))
})
