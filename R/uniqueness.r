

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

#' @export
is_id.data.frame <- function(df, ..., notifier = base::warning) {
  claimed_id_vars <- tidyselect::vars_select(colnames(df), ...)

  stopifnot(is.character(claimed_id_vars), length(claimed_id_vars) > 0,
            is.function(notifier))

  not_found_vars <- base::setdiff(claimed_id_vars, colnames(df))
  if (length(not_found_vars) > 0) {
    err_msg <- sprintf("Claimed ID vars not in dataset: %s",
                       paste(not_found_vars, collapse = ", "))
    notifier(err_msg)
    return(FALSE)
  }

  df_id_cols_only <- dplyr::ungroup(dplyr::select(df, claimed_id_vars))
  id_cols_with_na <- purrr::map_lgl(df_id_cols_only, anyNA)
  if (any(id_cols_with_na)) {
    err_msg <- paste("ID variables cannot be NA. Problem variables:",
      paste(colnames(id_cols_with_na)[id_cols_with_na], collapse = ", "), sep = "\n")
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

#' @export
is_id.tbl_lazy <- function(df, ..., notifier = base::warning) {
  `.` <- NULL # make R CMD CHECK happy.
  df_names <- colnames(df)
  claimed_id_vars <- tidyselect::vars_select(df_names, ...)

  stopifnot(is.character(claimed_id_vars), length(claimed_id_vars) > 0,
            is.function(notifier))

  not_found_vars <- base::setdiff(claimed_id_vars, df_names)
  if (length(not_found_vars) > 0) {
    err_msg <- sprintf("Claimed ID vars not in dataset: %s",
                       paste(not_found_vars, collapse = ", "))
    notifier(err_msg)
    return(FALSE)
  }
  any_vars <- dplyr::any_vars  # does nothing except satisfy R CMD CHECK
  df_id_cols_only <- dplyr::ungroup(dplyr::select(df, claimed_id_vars))
  df_nas <- dplyr::filter_all(df_id_cols_only, any_vars(is.na(.)))
  df_nas_nrow <- nrow(utils::head(df_nas, 1), force = TRUE)
  # If the df_nas table has any rows, at least one ID variable contains NAs
  if (df_nas_nrow > 0) {
    # TODO: it would be nice to say which variables contain NA
    notifier("ID variables cannot be NA.")
    return(FALSE)
  }

  total_row_count <- nrow(df_id_cols_only, force = TRUE)
  if (total_row_count == 0) {
    notifier("No rows!")
    return(FALSE)
  }
  nrow_distinct <- nrow(dplyr::distinct(df_id_cols_only), force = TRUE)

  nrow_distinct == total_row_count
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
    claimed_id_vars <- tidyselect::vars_select(colnames(df), ...)
    stop("Variables don't uniquely identify rows: ",
         paste(claimed_id_vars, collapse = ", "))
  }
  df
}
