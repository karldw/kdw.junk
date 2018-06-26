#' Winsorize one vector
#'
#' @param x Vector to winsorize (numeric)
#' @param trim Fraction of data to trim at top and bottom
#' @return x, but winsorized
#'
#' (this version deals with point masses!)
#' @export
winsorize <- function(x, trim = 0.01) {
  stopifnot(purrr::is_atomic(x), purrr::is_scalar_atomic(trim))
  if ((trim < 0) || (trim > 0.5)) {
    stop("trimming must be reasonable")
  }
  # Get the indexes of the sorted vector that are the winsorization cut points

  min_to_keep_idx <- as.integer(floor(length(x) * trim))
  if (min_to_keep_idx > 0L) {
    max_to_keep_idx <- length(x) - min_to_keep_idx
  } else {
    min_to_keep_idx <- 1L
    max_to_keep_idx <- length(x)
  }
  x_sorted <- sort(x)  # this is inefficient
  min_to_keep <- x_sorted[min_to_keep_idx]
  max_to_keep <- x_sorted[max_to_keep_idx]
  out <- dplyr::case_when(
    dplyr::between(x, min_to_keep, max_to_keep) ~ x,
      x < min_to_keep ~ min_to_keep,
      TRUE ~ max_to_keep
  )
  return(out)
}
