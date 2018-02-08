# The code in this file is from github:nathanvan/parallelsugar
# Copyright (c) 2015 Nathan VanHoudnos
# Licensed under the MIT License
# Minor changes by Karl

#' Define a sockets version of mclapply
#'
#' An implementation of \code{\link[parallel]{mclapply}} using \code{parallel::parLapply}
#'
#' Windows does not support forking. This makes it impossible to use mclapply on Windows
#' to farm out work to additional cores.
#'
#' NOTE: This calls a load-balancing function, which doesn't play well with random number
#' generation. Everything I'm doing is deterministic, but be careful.
#' If you need to change it, switch out the `clusterApplyLB` for `clusterApply`.
#'
#' @param X A vector/list to iterate over
#' @param FUN A function to apply to `X`
#' @param ... What you pass to mclapply
#' @param mc.preschedule Not used, provided for consistency with [parallel::mclapply]
#' @param mc.set.seed Not used, provided for consistency with [parallel::mclapply]
#' @param mc.silent Not used, provided for consistency with [parallel::mclapply]
#' @param mc.cores Number of cores (default 1)
#' @param mc.cleanup Not used, provided for consistency with [parallel::mclapply]
#' @param mc.allow.recursive Not used, provided for consistency with [parallel::mclapply]
#' @return lapply-like list
mclapply_socket <- function(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
    mc.silent = FALSE, mc.cores = 1L, mc.cleanup = TRUE, mc.allow.recursive = TRUE) {
  stopifnot(purrr::is_scalar_atomic(mc.cores), mc.cores == as.integer(mc.cores),
    purrr::is_function(FUN))
  if (length(X) == 0) {
    return(list())
  }
  ## Create a cluster
  mc.cores <- min(length(X), mc.cores)
  cl <- parallel::makeCluster(mc.cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Base packages and additional packages
  # loaded.package.names <- c(sessionInfo()$basePkgs, names(sessionInfo()$otherPkgs))
  loaded.package.names <- names(utils::sessionInfo()$otherPkgs)

  # Ship it to the clusters
  parallel::clusterExport(cl, 'loaded.package.names', envir = environment())

  # Load the libraries on all the clusters
  parallel::parLapply(cl, seq_along(cl),
    function(xx){
      lapply(loaded.package.names,
        function(yy) {
          library(yy, character.only = TRUE)
        }
      )
    }
  )

  clusterExport_function <- function(cl, FUN) {
    ## We want the enclosing environment, not the calling environment
    ## (I had tried parent.frame, which was not what we needed)
    ##
    ## Written by Hadley Wickham, off the top of his head, when I asked him
    ##   for help at one of his Advanced R workshops.
    env <- environment(FUN)
    while(!identical(env, globalenv())) {
      env <- parent.env(env)
      parallel::clusterExport(cl, ls(all.names = TRUE, envir = env), envir = env)
    }
    parallel::clusterExport(cl, ls(all.names = TRUE, envir = env), envir = env)
    ## // End Hadley Wickham
  }


  clusterExport_function(cl, FUN)

  ## Run the lapply in parallel, with a special case for the ... arguments
  if(length(list(...)) == 0) {
    return(parallel::clusterApplyLB(cl = cl, x = X, fun = FUN))
  } else {
    return(parallel::clusterApplyLB(cl = cl, x = X, fun = FUN, ...))
  }
}
