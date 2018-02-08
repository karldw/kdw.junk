

#' Do the claimed variables identify rows?
#'
#' Just like Stata's isid.
#'
#' @param df A dataframe to test
#' @param ... Variable names, following [dplyr::select] rules
#' @param notifier A function to report conditions you wouldn't want in an ID variable
#'  (Defaults to [base::warning]. Other reasonable options might be [base::stop] to
#'   escalate issues or [base::force] to not report them.)
#' @return TRUE/FALSE for ID-ness
#' @export
is_id <- function(df, ..., notifier = base::warning) {
  UseMethod("is_id")
}

is_id.data.frame <- function(df, ..., notifier = base::warning) {
  # TODO: write a version of this that works for databases
  claimed_id_vars <- tidyselect::vars_select(names(df), ...)

  stopifnot(is.character(claimed_id_vars), length(claimed_id_vars) > 0,
            is.function(notifier))

  not_found_vars <- base::setdiff(claimed_id_vars, names(df))
  if (length(not_found_vars) > 0) {
    err_msg <- sprintf("Claimed ID vars not in dataset: %s",
                       paste(not_found_vars, collapse = ", "))
      notifier(err_msg)
    return(FALSE)
  }

  df_id_cols_only <- dplyr::select_(df, .dots = claimed_id_vars)
  id_cols_with_na <- purrr::map_lgl(df_id_cols_only, anyNA)
  if (any(id_cols_with_na)) {
    err_msg <- paste("ID variables cannot be NA. Problem variables:",
      paste(names(id_cols_with_na)[id_cols_with_na], collapse = ", "), sep = "\n")
    notifier(err_msg)
    return(FALSE)
  }
  total_row_count <- nrow(df_id_cols_only)
  if (total_row_count == 0) {
    notifier("No rows!")
    return(FALSE)
  }

  # anyDuplicated is faster than calling "distinct" then counting rows
  ids_are_unique <- anyDuplicated(df_id_cols_only) == 0
  return(ids_are_unique)
}

is_id.tbl_lazy <- function(df, ..., notifier = base::warning) {
  stop("Not yet implemented.")
}


#' Raise an error if the claimed variables don't uniquely identify rows.
#'
#' Calls [is_id] (with warnings as errors), then returns the original data if [is_id]
#' returns `TRUE`.
#'
#' @param df A dataframe to test
#' @param ... Passed to [is_id]
#' @return Original `df`
#' @export
ensure_id_vars <- function(df, ...) {
  if (! isTRUE(is_id(df, ..., notifier = base::stop))) {
    claimed_id_vars <- tidyselect::vars_select(names(df), ...)
    stop("Variables don't uniquely identify rows: ",
         paste(claimed_id_vars, collapse = ", "))
  }
  df
}
