
#' Update installed packages
#'
#' @param repos CRAN repo to use (defaults to Rstudio's)
#' @param checkBuilt If TRUE, a package built under an earlier major.minor version of R
#'  (e.g., 3.4) is considered to be ‘old’. (default TRUE)
#' @param verbose Narrate progress? (default TRUE)
#' @return Character vector of packages updated (possibly of length 0)
#'
#' This version of update_packages goes the extra step of reinstalling all packages that
#' depend on Rcpp whenever Rcpp is updated.
update_packages <- function(repos = getOption("repos"), checkBuilt = TRUE, verbose = TRUE) {
  if (length(repos) > 1) {
    repos <- repos[1]
  }
  if (repos == "@CRAN@") {
    repos <- "https://cran.rstudio.com"
  }
  outdated <- utils::old.packages(checkBuilt = checkBuilt, repos = repos)
  if (! is.null(outdated)) {
    outdated <- outdated[! grepl("^/usr/l", as.character(outdated[, "LibPath"])), , drop = FALSE]
    if (NROW(outdated) == 0) {
      outdated <- NULL
    }
  }

  if (! is.null(outdated)) {
    pkg_to_update <- as.character(outdated[, "Package"])

    narrate("  Updating: ", paste(pkg_to_update, collapse = ", "), "\n", verbose = verbose)
    if ("Rcpp" %in% pkg_to_update) {
      rcpp_deps <- find_rcpp_deps(repos = repos, verbose = verbose)
      additional_rcpp_updates <- setdiff(rcpp_deps, pkg_to_update)
      if (length(additional_rcpp_updates) > 0) {
        narrate("  Also updating Rcpp-depending packages: ",
          paste(additional_rcpp_updates, collapse = ", "), "\n", verbose = verbose)
      }
      pkg_to_update <- union(pkg_to_update, rcpp_deps)
    }
    utils::install.packages(pkg_to_update, repos = repos)
    ret <- pkg_to_update
  } else {
    narrate("  R packages are up to date.", verbose = verbose)
    ret <- character(0)
  }
  invisible(ret)
}


#' Find installed packages that depend on Rcpp
#'
#' @param repos CRAN repo to use (defaults to Rstudio's)
#' @param verbose Narrate progress? (default TRUE)
#' @return Vector of installed pacakges that depend on Rcpp and are available in `repos`
#'
#' Borrowed from https://github.com/RcppCore/rcpp-logs/blob/master/scripts/showReverseRcppDepends.r
find_rcpp_deps <- function(repos = getOption("repos", default = "https://cran.rstudio.com"), verbose = TRUE) {
  if (length(repos) > 1) {
    repos <- repos[1]
  }
  if (repos == "@CRAN@") {
    repos <- "https://cran.rstudio.com"
  }

  IP <- utils::installed.packages()
  AP <- utils::available.packages(utils::contrib.url(repos), filters = list())[, "Package", drop = FALSE]

  depend_on_rcpp <- unique(c(
    grep("Rcpp", as.character(IP[, "Depends",   drop = TRUE]), fixed = TRUE),
    grep("Rcpp", as.character(IP[, "LinkingTo", drop = TRUE]), fixed = TRUE),
    grep("Rcpp", as.character(IP[, "Imports",   drop = TRUE]), fixed = TRUE)
    ))
  are_compiled <- grep("yes", as.character(IP[, "NeedsCompilation", drop = TRUE]))
  depend_on_rcpp_and_compiled <- intersect(depend_on_rcpp, are_compiled)

  rcppset <- sort(unique(unname(IP[depend_on_rcpp_and_compiled, "Package", drop = TRUE])))

  # TODO: check version numbers of the CRAN offering vs what's installed. If installed is newer, add it to `other`
  onCRAN <- intersect(rcppset, AP)

  other <- setdiff(rcppset, AP)
  if (length(other) > 0) {
    narrate("\n\n  There are possibly local / github packages that depend on Rcpp.\n",
      "Please update them manually.\n ",
      dput(other), verbose = verbose)
  }
  return(onCRAN)
}
