#' Winsorize one vector
#'
#' @param x Vector to winsorize (numeric)
#' @param trim Fraction of data to trim (if length == 1, same at top and bottom,
#'   if length == 2, trim different fractions)
#' @param na.rm Remove NAs? Fails if NAs are present and this argument is FALSE
#' @return x, but winsorized
#'
#' (this version deals with point masses!)
#' @export
winsorize <- function(x, trim = 0.01, na.rm = FALSE) {
  stopifnot(purrr::is_atomic(x), purrr::is_atomic(trim), length(trim) %in% c(1, 2))
  if (any(trim < 0) || any(trim > 0.5)) {
    stop("trimming must be reasonable")
  }
  if (length(trim) == 1) {
    trim <- c(trim, trim)
  }
  # Things like date can be winsorized, but dplyr::between is unhappy about it.
  # set to a basic class first
  if (typeof(x) %in% c("integer", "double") & !inherits(x, "double") & !inherits(x, "integer")) {
    orig_class <- class(x)
    x <- unclass(x)
    must_reclass <- TRUE
  } else {
    must_reclass <- FALSE
  }
  # quantile approach borrowed from https://www.r-bloggers.com/winsorization/
  # but improved by using type = 1
  lim <- stats::quantile(x, probs = c(trim[1], 1 - trim[2]),
    type = 1, names = FALSE, na.rm = na.rm)
  x[x < lim[1]] <- lim[1]
  x[x > lim[2]] <- lim[2]

  if (must_reclass) {
    class(x) <- orig_class
  }
  x
}
#
#
#   # Get the indexes of the sorted vector that are the winsorization cut points
#   min_to_keep_idx <- as.integer(floor(length(x) * trim))
#   if (min_to_keep_idx > 0L) {
#     max_to_keep_idx <- length(x) - min_to_keep_idx
#   } else {
#     min_to_keep_idx <- 1L
#     max_to_keep_idx <- length(x)
#   }
#
#
#   x_sorted <- sort(x)  # this is inefficient
#   min_to_keep <- x_sorted[min_to_keep_idx]
#   max_to_keep <- x_sorted[max_to_keep_idx]
#
#   out <- dplyr::case_when(
#     dplyr::between(x, min_to_keep, max_to_keep) ~ x,
#       x < min_to_keep ~ min_to_keep,
#       TRUE ~ max_to_keep
#   )
#
#   return(out)
# }
