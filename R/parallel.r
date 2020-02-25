

#' Get a reasonable number for mc.cores
#'
#' @param max_cores Max number of cores this function will return (default 4)
#' @return An integer number of cores to use
#' @export
get_cores <- function(max_cores = 4L) {
  # Find how many cores the machine has, counting only physical (rather than
  # logical) cores. That is, ignore hyperthreading. Don't automatically use > 4.
  # (You can still specify mc.cores > 4 in the function call.)
  cores <- min(parallel::detectCores(logical = FALSE), max_cores, na.rm = TRUE)
  if (is.na(cores)) {
    cores <- 1L
  }
  cores
}


#' DEPRECATED: Run lapply on multiple cores with mclapply
#'
#' @param X Argument to FUN to iterate over
#' @param FUN Function to apply
#' @param ... Other arguments passed to `FUN`
#' @param mc.cores Number of cores to use. (Default of NULL tries to guess.)
#' @return The result, as if passed to [lapply()]
#'
#' This is now just a thin wrapper around furrr::future_map. You shou
#'
#' @export
lapply_parallel <- function(X, FUN, ..., mc.cores = NULL) {
  stop("lapply_parallel() is deprecated. Use the more powerful furrr::future_map() instead.")
}


#' DEPRECATED: Run lapply then bind_rows, with multicore
#'
#' @param X Argument to FUN to iterate over
#' @param FUN Function to apply
#' @param ... Other arguments passed to `FUN`
#' @param lapply_id If not NULL, create a column with the name provided by lapply_id to
#'   identify the `X` value used.
#' @param mc.cores Number of cores to use. (Default of NULL tries to guess; see [get_cores])
#' @return The result, as if passed to [lapply()]
#' @seealso [purrr::map_df()], [dplyr::bind_rows()]
#' @importFrom rlang ":="
#' @export
lapply_bind_rows <- function(X, FUN, ..., lapply_id = NULL, mc.cores = NULL) {
  stop("lapply_bind_rows() is deorecated. Use the more powerful furrr::future_dfr() instead.")
}


.bind_rows_add_id <- function(lst, X, lapply_id = NULL) {
  # Dispatch based on the class of the first element of the list
  if (!rlang::is_list(lst)) {
    stop("This function is meant to be called on lists.")
  }
  UseMethod(".bind_rows_add_id", lst[[1]])
}


.bind_rows_add_id.data.frame <- function(lst, X, lapply_id = NULL) {
  if (is.null(lapply_id)) {
    out <- dplyr::bind_rows(lst)
  } else {
    row_counts <- purrr::map_int(lst, nrow)
    id_col <- rep(X, row_counts)
    # It is possible to do this all in one step with proper use of .data$ and .env,
    # but it gets really messy.
    out <- dplyr::mutate(dplyr::bind_rows(lst), !! lapply_id := !! id_col)
  }
  out
}


.bind_rows_add_id.sf <- function(lst, X, lapply_id = NULL) {
  # bind_rows doesn't work well for sf
  out <- rbind(lst)
  if (! is.null(lapply_id)) {
    row_counts <- purrr::map_int(lst, nrow)
    id_col <- rep(X, row_counts)

    out <- dplyr::mutate(out, !! lapply_id := !! id_col)
  }
  out
}


.bind_rows_add_id.tbl_sql <- function(lst, X, lapply_id = NULL) {
  # As with data.frames, we want to add an ID column.  The name of the column is provided
  # by lapply_id.  Unlike bind_rows, we have to add it manually, even if X is not
  # atomic.  So, the add_src_id function looks at X and picks either the value
  # of X or the value of the index to add as the column name. The mutate_ call
  # is relatively simple because I'm just adding a constant.
  # Then we use union_all, which translates to SQL's UNION ALL, to bind the tables
  # into one. union_all only takes two tables, so use Reduce to bring them all
  # together.
  .add_src_id <- function(idx) {
    if (is.atomic(X)) {
      # In this case, add the value of X for the index
      X_val <- X[idx]
    } else {
      # In this case, just add the sequential index (like bind_rows' .id argument)
      X_val <- idx
    }
    dplyr::mutate(lst[[idx]], !! lapply_id := !! X_val)
  }

  if (! is.null(lapply_id)) {
    # It's helpful to add the ID to each element individually, as opposed to doing it at
    # the end, because this way I can defer computation. If I did it at the end, I think
    # I would have to know how many rows were in each result of lst.
    lst <- lapply(seq_along(lst), .add_src_id)
  }
  Reduce(dplyr::union_all, lst)
}
