
#' Really get the number of rows of the table
#'
#' @param x Table to count rows of
#' @param force Force the count of a `tbl_lazy`? (not used otherwise)
#' @return Number of rows in .tbl
#' @seealso [nrow()]
#'
#' Works even for dplyr's lazy tables.
#' @export
nrow <- function(x, force = FALSE) {
  UseMethod("nrow")
}

#' @export
nrow.default <- function(x, force = FALSE) {
  base::nrow(x)
}

#' @export
nrow.tbl_lazy <- function(x, force = FALSE) {
  if (!force) {
    res <- base::nrow(x)
  } else {
    n <- dplyr::n  # doesn't do anything except satisfy R CMD CHECK
    x_nrow <- dplyr::summarize(dplyr::ungroup(x), n = n())
    res <- dplyr::collect(x_nrow)$n
  }
}


#' Make a join function safer.
#'
#' @param join_fn Original join function to wrap
#' @param fast Boolean, should the check be fast or thorough (defaults to fast)
#' @return A new join function
#'
#' The new join function will have arguments `x`, `y`, `by`, `...` and
#' `allow_cartesian = FALSE`. The goal here is to emulate [merge()], which will
#' raise an error if rows aren't uniquely identified. Instead, dplyr follows SQL
#' and does a cartesian join if rows aren't unique.
#'
#' Additionally, the `by` variables are required. They cannot be implicit.
#' The `...` arguments are passed to the dplyr join function.
#'
#' If fast == TRUE, do the join, then check that the number of rows is not
#' greater than the sum of the row counts of the input tables.
#' If fast == FALSE, make sure that the by variables uniquely identify rows in
#' at least one of the tables before doing the join.
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

  check_name_clash <- function(x, y, by) {
    if (is.null(names(by))) {
      names_x_by <- by
      names_y_by <- by
    } else {
      names_x_by <- names(by)
      names_y_by <- unname(by)
    }
    names_x_notby <- base::setdiff(colnames(x), names_x_by)
    names_y_notby <- base::setdiff(colnames(y), names_y_by)
    clashing_names <- base::intersect(names_x_notby, names_y_notby)
    if (length(clashing_names) > 0) {
      stop("Overlapping names are not allowed. (Rerun with allow_name_clash = ",
        "TRUE if you want R's normal var.x and var.y approach.)\n",
        "Problem variables: ", paste(clashing_names, collapse = ", "))
    }
    invisible()
  }

  fast_join_fn <- function(x, y, by = NULL, ...,
      allow_cartesian = FALSE, allow_name_clash = FALSE) {
    stopifnot(purrr::is_scalar_logical(allow_cartesian),
      purrr::is_scalar_logical(allow_name_clash))

    # Always require an explicit 'by'
    if (is.null(by)) {
      stop("Please specify your 'by' variables explicitly.")
    }

    # Check that names are appropriate
    if (! allow_name_clash) {
      check_name_clash(x, y, by)
    }

    # Now do the actual join.
    join_results <- join_fn(x = x, y = y, by = by, ...)

    if (! allow_cartesian) {
      # Check that we didn't just do a cartesian join.
      # A slower, more complete way is actually check the claimed by-variables
      # for uniqueness in the two datasets.
      # Here, we're just going to count rows and throw an error if the number of
      # results was larger than the join type implies as possible.
      nrow_x <- nrow(x, force = TRUE)
      if (identical(join_fn, dplyr::inner_join)) {
        nrow_y <- nrow(y, force = TRUE)
        max_rows <- max(nrow_x, nrow_y)
      } else if (identical(join_fn, dplyr::full_join) ||
                 identical(join_fn, dplyr::left_join) ||
                 identical(join_fn, dplyr::right_join)) {
        nrow_y <- nrow(y, force = TRUE)
        max_rows <- nrow_x + nrow_y
      } else if (identical(join_fn, dplyr::anti_join)) {
        max_rows <- nrow_x
      } else if (identical(join_fn, dplyr::semi_join)) {
        max_rows <- nrow_x
      } else {
        stop("Sorry, I don't know what to do with this join function.")
      }
      nrow_join_results <- nrow(join_results, force = TRUE)
      if (nrow_join_results > max_rows) {
        stop(glue::glue(
          "Join results in {joined_rows} rows; more than {max_rows}, the ",
          "maximum that would be expected if either of your tables were ",
          "uniquely identified by their `by` variables. Check for duplicate ",
          "values in your by-variables in each table.\n",
          "If you are sure you wish to proceed, rerun with allow_cartesian = ",
          "TRUE to join the same values over and over again. Also see the ",
          "help for data.table::merge.",
          joined_rows = nrow_join_results, max_rows = max_rows))
      }
    }
    join_results
  }

  # You can also do it by actually checking uniqueness, but that's usually not
  # necessary.
  thorough_join_fn <- function(x, y, by, ..., allow_cartesian = FALSE,
    allow_name_clash = FALSE) {
    if (missing(by) || is.null(by) || is.na(by)) {
      stop("Please specify your 'by' variables explicitly.")
    }
    # Check that names are appropriate
    if (! allow_name_clash) {
      check_name_clash(x, y, by)
    }
    if (! allow_cartesian) {
      by_y <- unname(by)
      if (! is.null(names(by))) {
        by_x <- names(by)
      } else {
        by_x <- by_y
      }
      if (! is_id(x, by_x) && ! is_id(y, by_y)) {
        # iff x isn't IDed by the by_x variables, then turn to y
        stop("Neither table is uniquely identified by their 'by' variables!")
      }
    }
    join_fn(x = x, y = y, by = by, ...)
  }

  if (isTRUE(fast)) {
    return(fast_join_fn)
  } else {
    return(thorough_join_fn)
  }
}


#' Use dplyr to emulate Stata merges.
#'
#' @param x A dataframe to merge ('master' in Stata)
#' @param y A dataframe to merge ('using' in Stata)
#' @param multi What multiple matches are allowed? One of "1:1", "1:m", "m:1",
#'    or "1:1 _n". Note that m:m matches aren't allowed because they're
#'    ill-defined. You can use standard dplyr joins if you instead want the
#'    cartesian product (equivalent to Stata's joinby).
#' @param by Variables to join by (as strings)
#' @param keep Observations to keep. Can be specified with Stata words
#'   (1, 2, 3, "master", "using", "match") or SQL words
#'   ("left", "right", "full", "semi").
#' @param keepusing Variables from the `y` ('using') dataset to keep.
#' @param ... Other arguments passed onto the dplyr *_join function.
#' @export
merge_stata <- function(x, y, multi = c("1:1", "1:m", "m:1", "1:1 _n"),
    by = NULL, keep = "inner", keepusing = NULL, ...) {
  mutli <- match.arg(multi)

  select_keepusing <- function(y, by, keepusing) {
    if (! is.null(keepusing)) {
      keepusing <- tidyselect::vars_select(colnames(y), keepusing)
      by_to_add <- setdiff(by, keepusing)
      keepusing <- c(keepusing, by_to_add)
      y <- dplyr::select(y, keepusing)
    }
    y
  }
  if (setequal(keep, 3) || setequal(keep, "match")) {
    keep <- "inner"
  } else if (setequal(keep, c(1, 3)) || setequal(keep, c("master", "match"))) {
    keep <- "left"
  } else if (setequal(keep, c(2, 3)) || setequal(keep, c("using", "match"))) {
    keep <- "right"
  } else if (setequal(keep, c(1, 2, 3)) || setequal(keep, c("master", "using", "match"))) {
    keep <- "full"
  } else if (setequal(keep, 1) || setequal(keep, "master")) {
    keep <- "semi"
  } else if (setequal(keep, 2) || setequal(keep, "using")) {
    if (! is.null(names(by))) {
      # interchange the names and values if we're going to switch around.
      new_by <- names(by)
      names(new_by) <- unname(by)
      by <- new_by
    }
    y <- select_keepusing(y, by = by, keepusing = keepusing)
    multi <- stringi::stri_reverse(multi)
    return(merge_stata(x = y, y = x, multi = multi, by = by, keep = "semi"))
  }
  if (length(keep) != 1 || ! keep %in% c("left", "right", "inner", "full", "semi")) {
    stop("keep should be one of \"left\", \"right\", \"inner\", \"full\", ",
         "\"semi\", or their Stata equivalents")
  }

  if (multi == "1:1 _n") {
    return(dplyr::bind_rows(x, y))
  }
  if (is.null(by)) {
    stop("Please specify your 'by' variables explicitly.")
  }
  if (multi %in% c("1:1", "1:m")) {
    ensure_id_vars(x, by)
  }
  if (multi %in% c("1:1", "m:1")) {
    ensure_id_vars(y, by)
  }

  join_fn <- getExportedValue("dplyr", paste0(keep, "_join"))
  join_fn(x, y, by = by, ...)
}
