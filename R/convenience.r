
#' Get the current operating system
#'
#' @return "win", "mac", or "linux".
#' Only works for Windows, Mac, and Linux. (Non-Mac unix systems are reported as Linux.)
#' @export
get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if (.Platform$OS.type == "unix") {
    "linux"  # sorry BSD
  } else {
    stop("Unknown OS")
  }
}

#' Install packages if not already installed
#'
#' @param pkg_list Vector of packages to install
#' @param verbose Narrate progress?
#' @return NULL
#'
#' @export
install_lazy <- function(pkg_list, verbose = TRUE) {
  installed_packages <- utils::installed.packages()[, 1]
  need_to_install <- setdiff(pkg_list, installed_packages)
  already_installed <- pkg_list[pkg_list %in% installed_packages]
  for (pkg in need_to_install) {
    try(utils::install.packages(pkg), silent=TRUE)
  }
  if (verbose) {
    message("Already installed:")
    print(already_installed)
    newly_installed <- need_to_install[need_to_install %in% utils::installed.packages()]
    if (length(newly_installed) > 0) {
      message("Newly installed:")
      print(newly_installed)
    }
  }
  failed_to_install <- setdiff(need_to_install, utils::installed.packages())
  if (length(failed_to_install) > 0) {
    warning("Failed to install these packages:\n  ", paste(failed_to_install))
  }
  invisible()
}

#' Clear everything possible
#'
#' @param to_clear Vector of things to clear.
#'   "graphics" means close all graphics devices.
#'   "objects" means remove all objects from global environment.
#'   "packages" means unload all loaded namespaces
#'   The default is to do all three.
#' @return NULL
#'
#' @export
clear_all <- function(to_clear = c("graphics", "objects", "packages")) {
  to_clear <- match.arg(to_clear, several.ok = TRUE)
  if ("graphics" %in% to_clear) {
    # clear and close any open grapics devices, then delete everything.
    while (! is.null(grDevices::dev.list())) {
      while(grDevices::dev.flush() > 0) {
        # do nothing.
      }
      try(grDevices::dev.off(), silent = TRUE)
    }
  }

  if ("objects" %in% to_clear) {
    rm(list = ls(envir = .GlobalEnv, all.names = TRUE, sorted = FALSE),
       envir = .GlobalEnv)
  }
  if ("packages" %in% to_clear) {
    to_detatch <- setdiff(loadedNamespaces(),
      c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base"))
    lapply(to_detatch, unloadNamespace)
  }
  invisible()
}



#' Stop if a condition is TRUE
#'
#' @param ... Conditions that will be evaluated
#' @seealso [stopifnot()]
#' @export
stopif <- function (...) {
  # just a negation of stopifnot, but Negate(stopifnot) doesn't work because of NSE
  n <- length(ll <- list(...))
  if (n == 0L)
    return(invisible())
  mc <- match.call()
  # !any instead of all below:
  for (i in 1L:n) if (!(is.logical(r <- ll[[i]]) && !anyNA(r) && !any(r))) {
    ch <- deparse(mc[[i + 1]], width.cutoff = 60L)
    if (length(ch) > 1L)
      ch <- paste(ch[1L], "....")
    stop(sprintf(ngettext(length(r), "%s is not FALSE", "%s are not all FALSE"),
                 ch), call. = FALSE, domain = NA)
  }
  invisible()
}



#' Get quarter from month
#'
#' @param mon Month number vector (1-12)
#' @param make_factor Make the result into a factor with appropriate levels? (default FALSE)
#' @return Quarters of the months (integer or factor)
#' @export
month_to_quarter <- function(mon, make_factor = FALSE) {
  qtr <- ((mon - 1) %/% 3) + 1
  if (make_factor) {
    qtr <- factor(qtr, levels = c(1, 2, 3, 4), labels = c("q1", "q2", "q3", "q4"))
  } else {
    qtr <- as.integer(qtr)
  }
  return(qtr)
}



#' Get file's extension.
#'
#' @param path The file or connection's name
#' @return The file or connection's extension
#'
#' In most cases, just apply [file_ext()], but include a special case for connections.
#' It's not required that the file exists.
get_ext <- function(path) {
  stopifnot(length(path) == 1)
  if (is.character(path)) {
    file_ext <- tolower(tools::file_ext(path))
  } else if (is.connection(path)) {
    # Pull out the connection underlying file and get its extension.
    # http://stackoverflow.com/a/12158664
    file_ext <- get_ext(summary(path)$description)
  } else {
    stop("Can't process ", path)
  }
  return(file_ext)
}


#' Return the filename without extension.
#'
#' @param path The file(s) or connection's name(s) (works with vectors)
#' @return The file or connection's name without extension
#'
#' In most cases, just apply [tools::file_path_sans_ext()], but include a special case for connections.
#' It's not required that the file exists.
#' @export
remove_ext <- function(path) {
  # The summary bit only works on single connections, so be recursive if necessary.
  if (length(path) > 1) {
    return(purrr::map_chr(path, remove_ext))
  } else if (length(path) == 1 && purrr::is_bare_list(path)) {
    path <- path[[1]]
  }
  if (is.connection(path)) {
    path <- summary(path)$description
  }
  return(tools::file_path_sans_ext(basename(path)))
}


#' Test if connection (to a file/website/zipfile etc)
#'
#' @param x Object to test
#' @return Logical: is it a connection?
#' @export
is.connection <- function(x) {
  inherits(x, "connection")
}


#' Save a ggplot plot
#'
#' @param plt The plot created by [ggplot2::ggplot()]
#' @param filename The filename to save (save type depends on extension)
#' @param scale_mult A scale multiplier on the size. Defaults to 1; bigger numbers use a larger canvas.
#' @return The plot (invisibly)
#' @seealso [ggplot2::ggsave()]
#' @export
save_plot <- function(plt, filename, scale_mult = 1) {
  stopifnot(dir.exists(dirname(filename)))
  # Save in the ratio of a beamer slide.
  # This aspect ratio works pretty well for normal latex too
  ggplot2::ggsave(filename = filename, plot = plt,
    width = 6.3 * scale_mult, height = 3.54 * scale_mult, units = "in",
    device = grDevices::cairo_pdf)
  invisible(plt)
}



#' Read and reconstitute Stata file.
#'
#' @param ... Arguments passed to [haven::read_dta]
#' @return The data, as read by [haven::read_dta], but with less attribute nonsense.
#'
#' @export
read_dta <- function(...) {
  has_labels <- function(x) {
    "labels" %in% names(attributes(x))
  }
  label_to_factor <- function(x) {
    x_labels <- attr(x, "labels", exact = TRUE)
    factor(x, levels = unname(x_labels), labels = names(x_labels))
  }
  remove_attributes <- function(x, to_remove = c("format.stata", "label")) {
    stopifnot(length(to_remove) >= 1, purrr::is_bare_character(to_remove))
    for (one_attr in to_remove) {
      attr(x, one_attr) <- NULL
    }
    x
  }
  df <- haven::read_dta(...)
  df <- dplyr::mutate_if(df, has_labels, label_to_factor)
  df <- dplyr::mutate_all(df, remove_attributes)
  df
}



#' Is this code running in Rstudio?
#'
#' @return Logical -- is this code running in Rstudio?
is_rstudio <- function() {
  identical(Sys.getenv("RSTUDIO"), "1")
}
