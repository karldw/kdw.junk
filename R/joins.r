
#' Make a join function safer.
#'
#' @param join_fn Original join function to wrap
#' @param fast Boolean, should the check be fast or extra-thorough (defaults to TRUE)
#' @return A new join function
#'
#' The new join function will have arguments `x`, `y`, `by`, `...` and
#' `allow_cartesian = FALSE`. The goal here is to emulate [merge()], which will raise an
#' error if rows aren't uniquely identified. Instead, dplyr follows SQL and does a
#' cartesian join if rows aren't unique.
#'
#' If fast == TRUE, do the join, then check that the number of rows is not greater than
#' the sum of the row counts of the input tables.
#' If fast == FALSE, make sure that the by variables uniquely identify rows in at least
#' one of the tables before doing the join.
#'
#' @seealso [merge()], [dplyr::inner_join()]
#' @examples
#' nrow(mtcars)  # 32
#' nrow(dplyr::inner_join(mtcars, mtcars, by = 'cyl'))  # 366
#' inner_join <- make_join_safer(dplyr::inner_join)
#' \dontrun{
#' inner_join(mtcars, mtcars, by = 'cyl')  # error
#' }
#' @export
make_join_safer <- function(join_fn, fast = TRUE) {
  fast_join_fn <- function(x, y, by = NULL, ...,
      allow_cartesian = FALSE, allow_name_clash = FALSE) {
    stopifnot(purrr::is_scalar_logical(allow_cartesian),
      purrr::is_scalar_logical(allow_name_clash))

    # Always require an explicit 'by'
    if (is.null(by)) {
      stop("Please specify your 'by' variables explicitly.")
    }

    # Check that names are appropriate
    if (is.null(names(by))) {
      names_x_by <- by
      names_y_by <- by
    } else {
      names_x_by <- names(by)
      names_y_by <- unname(by)
    }
    names_x_notby <- base::setdiff(names(x), names_x_by)
    names_y_notby <- base::setdiff(names(y), names_y_by)
    clashing_names <- base::intersect(names_x_notby, names_y_notby)
    if (length(clashing_names) > 0 && ! allow_name_clash) {
      stop("Overlapping names are not allowed. (Rerun with allow_name_clash = TRUE ",
        "if you want R's normal var.x and var.y approach.)\n",
        "Problem variables: ", paste(clashing_names, collapse = ", "))
    }

    # Now do the actual join.
    join_results <- join_fn(x = x, y = y, by = by, ...)

    # Check that we didn't just do a cartesian join.
    # A slower, more complete way is actually check the claimed by-variables for
    # uniqueness in the two datasets.
    # Here, we're just going to count rows and throw an error if the number of results
    # was larger than the join type implies as possible.
    nrow_x <- nrow(x)
    nrow_y <- nrow(y)
    if (identical(join_fn, dplyr::inner_join)) {
      max_rows <- max(nrow_x, nrow_y)
    } else if (identical(join_fn, dplyr::full_join) ||
               identical(join_fn, dplyr::left_join) ||
               identical(join_fn, dplyr::right_join)) {
      max_rows <- nrow_x + nrow_y
    } else if (identical(join_fn, dplyr::anti_join)) {
      max_rows <- nrow_x
    } else if (identical(join_fn, dplyr::semi_join)) {
      max_rows <- nrow_x
    } else {
      stop("Sorry, I don't know what to do with this join function.")
    }
    nrow_join_results <- nrow(join_results)
    if (nrow_join_results > max_rows && ! allow_cartesian) {
      err_msg <- paste("Join results in",
        sprintf("%s rows; more than the expected max for unique by columns, %s.",
                nrow_join_results, max_rows),
        "Check for duplicate key values your by-variables in each table, each of ",
        "which join to the same values over and over again. If you are sure you wish ",
        "to proceed, rerun with allow_cartesian = TRUE. Also see the help for ",
        "data.table::merge.")
      stop(err_msg)
    }

    return(join_results)
  }

  # You can also do it by actually checking uniqueness, but that's usually not
  # necessary (and not as throughly tested)
  thorough_join_fn <- function(x, y, by, ..., allow_cartesian = FALSE,
    allow_name_clash = FALSE) {
    if (missing(by) || is.null(by) || is.na(by)) {
      stop("Please specify your 'by' variables explicitly.")
    }
    # Check that names are appropriate
    if (is.null(names(by))) {
      names_x_by <- by
      names_y_by <- by
    } else {
      names_x_by <- names(by)
      names_y_by <- unname(by)
    }
    names_x_notby <- base::setdiff(names(x), names_x_by)
    names_y_notby <- base::setdiff(names(y), names_y_by)
    clashing_names <- base::intersect(names_x_notby, names_y_notby)
    if (length(clashing_names) > 0 && ! allow_name_clash) {
      stop("Overlapping names are not allowed. (Rerun with allow_name_clash = TRUE ",
        "if you want R's normal var.x and var.y approach.)\n",
        "Problem variables: ", paste(clashing_names, collapse = ", "))
    }
    if (! allow_cartesian) {
      by_y <- unname(by)
      if (! is.null(names(by))) {
        by_x <- names(by)
      } else {
        by_x <- by_y
      }

      if (! is_id(x, by_x)) {
        # iff x isn't IDed by the by_x variables, then turn to y
        if (! is_id(y, by_y)) {
          err_msg <- paste("Neither table is uniquely identified by",
                           "their 'by' variables!")
          stop(err_msg)
        }
      }
    }
    join_results <- join_fn(x = x, y = y, by = by, ...)
    return(join_results)
  }

  if (isTRUE(fast)) {
    # Note: still slower than the dplyr versions, but faster than the extra
    # thorough_join_fn, and probably good enough.
    return(fast_join_fn)
  } else {
    return(thorough_join_fn)
  }
}
