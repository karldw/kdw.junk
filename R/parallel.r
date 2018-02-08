

#' Get a reasonable number for mc.cores
#'
#' @param max_cores Max number of cores this function will return
#' @return An integer number of cores to use
#' @export
get_cores <- function(max_cores = 10L) {
  # Find how many cores the machine has, counting only physical (rather than
  # logical) cores. That is, ignore hyperthreading. Don't automatically use > 10.
  # (You can still specify mc.cores > 10 in the function call.)
  cores <- min(parallel::detectCores(logical = FALSE), max_cores, na.rm = TRUE)
  if (is.na(cores)) {
    cores <- 1L
  }
  cores
}


#' Run lapply on multiple cores with mclapply
#'
#' @param X Argument to FUN to iterate over
#' @param FUN Function to apply
#' @param ... Other arguments passed to `FUN`
#' @param mc.cores Number of cores to use. (Default of NULL tries to guess.)
#' @return The result, as if passed to [lapply()]
#'
#' Like parallel::mclapply, but with easier core detection (see [get_cores] and
#' mostly works on Windows. See https://github.com/nathanvan/parallelsugar
#'
#' @export
lapply_parallel <- function(X, FUN, ..., mc.cores = NULL) {
  # First, figure out how many cores to use.
  if (is.null(mc.cores)) {
    mc.cores <- get_cores()
  } else {
    mc.cores <- as.integer(mc.cores)
  }

  stopifnot(is.integer(mc.cores), length(mc.cores) == 1L, ! is.na(mc.cores))
  # mclapply only gives a warning if scheduled cores experience errors.
  # set warn = 2 so warnings become errors

  if (get_os() == "win") {
    mclapply <- mclapply_socket  # defined in parallelsugar.r
  } else {
    mclapply <- parallel::mclapply
  }

  orig_warn <- getOption("warn")
  on.exit(options(warn = orig_warn), add = TRUE)
  options(warn = 2)
  mclapply(X = X, FUN = FUN, ..., mc.cores = mc.cores)
}


#' Run lapply then bind_rows, with multicore
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
  stopifnot(length(X) >= 1)
  # just like lapply, but bind the results together at the end (plus parallelization)
  list_results <- lapply_parallel(X = X, FUN = FUN, ..., mc.cores = mc.cores)
  stopifnot(length(X) == length(list_results))
  .bind_rows_add_id(list_results, X = X, lapply_id = lapply_id)
}


.bind_rows_add_id <- function(lst, X, lapply_id = NULL) {
  # Dispatch based on the class of the first element of the list
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
