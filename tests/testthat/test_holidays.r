context("holidays")

test_that("hol_martin_luther_king_day works", {
  expect_identical(hol_martin_luther_king_day(1980:1981),
    structure(numeric(0L), class = "Date"))
  some_mlk_days <- hol_martin_luther_king_day(1986:2050)

  expect_equal(as.character(lubridate::wday(some_mlk_days, label=TRUE)), rep("Mon", length(some_mlk_days)))
  days_of_month <- lubridate::day(some_mlk_days)
  expect_true(all(days_of_month >= 15))
  expect_true(all(days_of_month <= 21))
})

test_that("hol_veterans_day works", {
  expect_equal(sort(unname(hol_veterans_day(1970:1978))),
    as.Date(c("1970-11-11",
    "1971-10-25", "1972-10-23", "1973-10-22", "1974-10-28", "1975-10-27",
    "1976-10-25", "1977-10-24", "1978-11-11")))
})

test_that("hol_inauguration_day works", {
  expect_equal(unname(hol_inauguration_day(2016:2018)), as.Date("2017-01-20"))
})


test_that("hol_good_friday works", {
  good_fridays <- hol_good_friday(1001:3000)
  expect_equal(as.character(lubridate::wday(good_fridays, label=TRUE)),
    rep("Fri", 2000))
})

test_that("hol_us_federal_holidays seems okay", {
  expect_equal(hol_us_federal_holidays(1780), structure(numeric(0), class="Date"))
  expect_equal(unname(hol_us_federal_holidays(2018)),
    as.Date(c("2018-01-01", "2018-01-15", "2018-02-19", "2018-05-28", "2018-07-04", "2018-09-03",  "2018-10-08", "2018-11-11", "2018-11-22", "2018-12-25")))
})

test_that(".round_down_weekday works", {
  lots_of_days <- seq(as.Date("2016-01-01"), as.Date("2018-12-31"), by = 1)
  thursday_dates <- .round_down_weekday(lots_of_days, "Thu")
  expect_equal(as.character(lubridate::wday(thursday_dates, label=TRUE)),
    rep("Thu", length(lots_of_days)))
  # test the rounding down part
  expect_true(all(thursday_dates <= lots_of_days))
})
