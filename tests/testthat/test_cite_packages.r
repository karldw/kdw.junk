context("Cite packages")

test_that("cite_one works", {
  # Glue:
  expected_cite <- structure(c(
    "@Manual{glue,",
    title = "  title = {glue: Interpreted String Literals},",
    author = "  author = {Jim Hester},",
    year = "  year = {2020},",
    note = "  note = {1.4.2},",
    url = "  url = {https://cran.r-project.org/package=glue},",
  "}"), class = "Bibtex")
  actual_cite <- .cite_one("glue")
  expect_equal(expected_cite["url"],   actual_cite["url"])
  expect_equal(expected_cite["title"], actual_cite["title"])

  skip_if_not_installed("data.table")
  # data.table:
  expected_cite <- structure(c(
    "@Manual{data.table,",
    title = "  title = {data.table: Extension of `data.frame'},",
    author = "  author = {Matt Dowle and Arun Srinivasan},",
    year = "  year = {2021},",
    note = "  note = {1.14.0},",
    url = "  url = {https://cran.r-project.org/package=data.table},",
    "}"), class = "Bibtex")
  actual_cite <- .cite_one("data.table")
  expect_equal(expected_cite["url"],   actual_cite["url"])
  expect_equal(expected_cite["title"], actual_cite["title"])
})
