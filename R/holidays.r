#' US holidays
#'
#' Available holiday functions are:
#'
#' * `hol_us_federal_holidays()`: All US federal holidays.
#'   These are: New Years day, Martin Luther King Jr. day,
#'   Presidents day (George Washington's birthday), Memorial day,  Independence
#'   day, Labor day, Columbus day, Veterans day, Thanksgiving day, and Christmas
#'   day.
#'
#' * `hol_new_years_day()`: New Years day
#'
#' * `hol_martin_luther_king_day()`: Martin Luther King Jr. day
#'
#' * `hol_inauguration_day()`: Presidential Inauguration day (every four years)
#'
#' * `hol_george_washington_birthday()`: George Washington's birthday
#'
#' * `hol_presidents_day()`: Presidents day; same as `hol_george_washington_birthday()`
#'
#' * `hol_good_friday()`: Good Friday (not a federal holiday, but some markets are closed)
#'
#' * `hol_easter():` Easter (not a federal holiday)
#'
#' * `hol_memorial_day()`: Memorial day
#'
#' * `hol_independence_day()`: Independence day (Fourth of July)
#'
#' * `hol_labor_day()`: Labor day
#'
#' * `hol_columbus_day()`: Columbus day
#'
#' * `hol_veterans_day()`: Veterans day
#'
#' * `hol_thanksgiving_day()`: Thanksgiving day
#'
#' * `hol_christmas_day()`: Christmas day
#'
#'
#' Important note: these holidays only return values when they were celebrated
#' at a federal level. For example, Veterans day is only 1938 onward.
#'
#' These functions require the `lubridate` package.
#'
#' A lot of this code comes from the tis package, but returns dates
#'
#' @name holidays
#' @param years Vector of years
#' @return Holidays for those years, as a named vector of dates.
NULL


#' @rdname holidays
#' @export
hol_us_federal_holidays <- function(years) {
  z <- c(hol_new_years_day(years),
         hol_martin_luther_king_day(years),
         hol_george_washington_birthday(years),
         hol_memorial_day(years),
         hol_independence_day(years),
         hol_labor_day(years),
         hol_columbus_day(years),
         hol_veterans_day(years),
         hol_thanksgiving_day(years),
         hol_christmas_day(years))
  hols <- sort(z)
  hols
}

#' @rdname holidays
#' @export
hol_new_years_day <- function(years){
  years <- years[years > 1870]
  if(length(years) == 0) {
    return(structure(numeric(0L), class = "Date"))
  }
  ans <- lubridate::make_date(years, month=1L, day=1L)
  names(ans) <- rep("NewYears", length(ans))
  ans
}

#' @rdname holidays
#' @export
hol_martin_luther_king_day <- function(years){
  years <- years[years > 1985]
  if(length(years) == 0) {
    return(structure(numeric(0L), class = "Date"))
  }
  # start with January 21, the latest possible day
  jan21 <- lubridate::make_date(years, month=1L, day=21L)
  ans <- .round_down_weekday(jan21, "Mon")
  names(ans) <- rep("MLKing", length(ans))
  ans
}

.round_down_weekday <- function(dates, weekday = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) {
  weekday <- match.arg(weekday)
  num_day <- which(weekday == c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  ans <- lubridate::floor_date(dates, unit="week", week_start=num_day)
  ans
}

#' @rdname holidays
#' @export
hol_george_washington_birthday <- function(years){
  pre1971 <- years >= 1880 & years <= 1970
  recent  <- years > 1970
  if(any(pre1971)) {
    ans <- lubridate::make_date(years[pre1971], month=2L, day=22L)
  } else {
    ans <- structure(numeric(0), class="Date")
  }
  if(any(recent)) {
    feb21 <- lubridate::make_date(years[recent], month=2L, day=21L)
    ans <- c(ans, .round_down_weekday(feb21, "Mon"))
  }
  names(ans) <- rep("GWBirthday", length(ans))
  ans
}

#' @rdname holidays
#' @export
presidents_day <- hol_george_washington_birthday

#' @rdname holidays
#' @export
hol_memorial_day <- function(years){
  pre1971 <- years >= 1888 & years <= 1970
  recent  <- years > 1970
  if (any(pre1971)) {
    ans <- lubridate::make_date(years[pre1971], month=5L, day=30L)
  } else {
    ans <- structure(numeric(0), class="Date")
  }
  if(any(recent)) {
    may31 <- lubridate::make_date(years[recent], month=5L, day=31L)
    ans <- c(ans, .round_down_weekday(may31, "Mon"))
  }
  names(ans) <- rep("Memorial", length(ans))
  ans
}

#' @rdname holidays
#' @export
hol_independence_day <- function(years){
  years <- years[years >= 1870]
  if (length(years) == 0) {
    return(structure(numeric(0), class="Date"))
  }
  ans <- lubridate::make_date(years, month=7L, day=4L)
  names(ans) <- rep("Independence", length(ans))
  ans
}

#' @rdname holidays
#' @export
hol_labor_day <- function(years){
  years <- years[years >= 1894]
  if (length(years) == 0) {
    return(structure(numeric(0), class="Date"))
  }
  sep7 <- lubridate::make_date(years, month=9L, day=7L)
  ans <- .round_down_weekday(sep7, "Mon")
  names(ans) <- rep("Labor", length(ans))
  ans
}

#' @rdname holidays
#' @export
hol_columbus_day <- function(years){
  pre1971 <- years >= 1934 & years <= 1970
  recent  <- years > 1970
  if (any(pre1971)) {
    ans <- lubridate::make_date(years[pre1971], month=10L, day=12L)
  } else {
    ans <- structure(numeric(0), class="Date")
  }
  if (any(recent)) {
    oct14 <- lubridate::make_date(years[recent], month=10L, day=14L)
    ans <- c(ans, .round_down_weekday(oct14, "Mon"))
  }
  names(ans) <- rep("Columbus", length(ans))
  ans
}

#' @rdname holidays
#' @export
hol_veterans_day <- function(years){
  mondayYears <- years >= 1971 & years <= 1977
  nov11Years <- (years >= 1938 & years <= 1970) | years > 1977
  if (any(mondayYears)) {
    # 4th monday in *october*
    oct28 <- lubridate::make_date(years[mondayYears], month=10L, day=28L)
    ans <- .round_down_weekday(oct28, "Mon")
  } else {
    ans <- structure(numeric(0), class="Date")
  }
  if (any(nov11Years)) {
    ans <- c(ans, lubridate::make_date(years[nov11Years], month=11L, day=11L))
  }
  names(ans) <- rep("Veterans", length(ans))
  ans
}

#' @rdname holidays
#' @export
hol_thanksgiving_day <- function(years){
  pre1939 <- years >= 1863 & years <= 1938
  is1939 <- years == 1939
  is1940 <- years == 1940
  recent <- years > 1940
  if(any(pre1939)){
    # last Thursday of November
    nov30 <- lubridate::make_date(years[pre1939], month=11L, day=30L)
    ans <- .round_down_weekday(nov30, "Thu")
  } else {
    ans <- structure(numeric(0), class="Date")
  }
  if(any(is1939)) {
    ans <- c(ans, rep(lubridate::make_date(1939L, 11L, 23L), sum(is1939)))
  }
  if(any(is1940)) {
    ans <- c(ans, rep(lubridate::make_date(1940L, 11L, 21L), sum(is1940)))
  }
  if (any(recent)) {
    # 4th thursday in November
    nov28 <- lubridate::make_date(years[recent], month=11L, day=28L)
    ans <- c(ans, .round_down_weekday(nov28, "Thu"))
  }
  names(ans) <- rep("Thanksgiving", length(ans))
  ans
}

#' @rdname holidays
#' @export
hol_christmas_day <- function(years){
  years <- years[years >= 1870]
  if (length(years) == 0) {
    return(structure(numeric(0), class="Date"))
  }
  ans <- lubridate::make_date(years, month=12L, day=25L)
  names(ans) <- rep("Christmas", length(ans))
  ans
}


#' @rdname holidays
#' @export
hol_good_friday <- function(years){
  z <- hol_easter(years) - 2
  names(z) <- rep("GoodFriday", length(z))
  z
}

#' @rdname holidays
#' @export
hol_inauguration_day <- function(years){
  ## Inauguration Day for given years
  inaugDates <- c(17890430,
                  10000*(1793 + 4*(0:35))  + 304,
                  10000*(1937 + 4*(0:115)) + 120)
  inaug <- as.Date(as.character(inaugDates), format = "%Y%m%d")
  sunday <- lubridate::wday(inaug, week_start=7L) == 1L
  inaug[sunday] <- inaug[sunday] + 1L

  inaug <- inaug[lubridate::year(inaug) %in% years]
  names(inaug) <- rep("Inauguration", length(inaug))
  inaug
}

#' @rdname holidays
#' @export
hol_easter <- function(years){
  G <- years %% 19
  C <- years %/% 100
  H <- (C - (C %/% 4) - ((8*C + 13) %/% 25) + 19*G + 15) %% 30
  I <- H - (H %/% 28) * (1 - (H %/% 28)*(29 %/% (H + 1))*((21 - G) %/% 11))
  J <- (years + (years %/% 4) + I + 2 - C + (C %/% 4)) %% 7
  L <-  I - J
  month <- 3 + (L + 40) %/% 44
  day <- L + 28 - 31*(month %/% 4)
  ans <- lubridate::make_date(years, month, day)
  names(ans) <- rep("Easter", length(ans))
  ans
}
