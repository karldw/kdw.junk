
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
#' @param scale_mult A scale multiplier on the size. Defaults to 1; bigger
#' numbers use a larger canvas.
#' @param bg The background color, passed to the cairo_pdf device. The default
#' is "white". If set to "transparent", the plot will be modified to make the
#' `panel.background`, `plot.background`, `legend.background`, and
#' `legend.box.background` transparent as well.
#' @return The plot (invisibly)
#' @seealso [ggplot2::ggsave()]
#' @export
save_plot <- function(plt, filename, scale_mult = 1, bg = "white") {
  stopifnot(dir.exists(dirname(filename)))
  if (identical(bg, "transparent")) {
    plt <- plt + ggplot2::theme(
      # Borrowed from https://stackoverflow.com/a/41878833
      panel.background      = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.background       = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.background     = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )
  }
  # Save in the ratio of a beamer slide.
  # This aspect ratio works pretty well for normal latex too
  ggplot2::ggsave(filename = filename, plot = plt,
    width = 6.3 * scale_mult, height = 3.54 * scale_mult, units = "in",
    device = grDevices::cairo_pdf, bg = bg)
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


#' Read a dataset into memory, using extension to figure out file type
#'
#' @param file Filename to read
#' @param ... Further arguments passed to the reader function
#' @param guess_max Number of rows to scan to check types in readxl
#'   (defaults to 1,000,000, which will be slower than readxl's default of 1000)
#' @return The read data
#'
#' This function is useful to read a range of datasets where you want to be able
#' to use a single reading function. It's a convenience function, and obviously
#' can't read all data types.
#'
#' Notes on the reading functions:
#' - xls and xlsx call [readxl::read_xls()] and [readxl::read_xlsx()]
#' - dbf uses [foreign::read.dbf()], with `as.is = TRUE`
#' - fst uses [fst::read_fst()]
#' - feather uses [feather::read_feather]
#' - csv uses [data.table::fread()] with `data.table = FALSE`
#' - dta uses [kdw.junk::read_dta()]
#' - rds uses [readRDS()]
#'
#' The `gues_max` parameter is set so high because computers are fast and I've
#' had a lot of issues with datasets that have blank cells for several thousand
#' rows (which read_excel reads as logical).
#' @export
read_data <- function(file, ..., guess_max = 1e6) {
  ext <- tolower(tools::file_ext(file))
  # Figure out what function to read.
  # Use partial to fill in the filename argument. (This is mostly useful for
  # fread, which takes both an input and a file argument; I want file.)
  read_fn <- switch(ext,
    xls     = purrr::partial(readxl::read_xls,  path = file, guess_max = guess_max),
    xlsx    = purrr::partial(readxl::read_xlsx, path = file, guess_max = guess_max),
    dbf     = purrr::partial(foreign::read.dbf, file = file, as.is = TRUE),
    fst     = purrr::partial(fst::read_fst, path = file),
    feather = purrr::partial(feather::read_feather, path = file),
    csv     = purrr::partial(data.table::fread, file = file, data.table = FALSE),
    dta     = purrr::partial(kdw.junk::read_dta, file = file),
    rds     = purrr::partial(readRDS, file = file))
  if (is.null(read_fn)) {
    stop("No reader function defined for extension '", ext, "'.\n",
         "Check the filename or add a new reader to read_data.")
  }
  read_fn(...)
}


#' Message to the user
#'
#' @param ... Arguments to `message`
#' @param verbose Actually use `message`? (default TRUE)
#' @return NULL
#' Just a thin wrapper around [message()]
#' @export
narrate <- function(..., verbose = TRUE) {
  if (verbose) {
    message(...)
  }
  invisible()
}


#' Turn a vector into one long string, used to generate messages
#'
#' @param x Vector to convert
#' @param quoted Should the individual elements have quotes around them?
#' @param add_and Should we add an 'and' before the last element
#' @return A string (length 1 character vector)
#' @example
#' vec2string(c(1,2,3)) == "'1', '2', and '3'"
#' @export
vec2string <- function(x, quoted = TRUE, add_and = TRUE) {
  # Like the toString function, but better
  stopifnot(length(x) > 0, is.logical(quoted), is.logical(add_and))
  if (quoted) {
    collapse_pattern <- "', '"
    sprintf_pattern <- "'%s'"
  } else {
    collapse_pattern <- ", "
    sprintf_pattern <- "%s"
  }
  end <- length(x)
  if (end == 1) {
    return(sprintf(sprintf_pattern, x))
  }
  first_elements <- x[-end]
  last_element <- x[[end]]
  if (add_and && end > 2) {
    last_element <- paste(' and', sprintf(sprintf_pattern, last_element))
  } else {
    last_element <- paste(',', sprintf(sprintf_pattern, last_element))
  }
  first_elements <- sprintf(sprintf_pattern, paste(first_elements,
                                                   collapse = collapse_pattern))
  out <- paste0(first_elements, last_element)
  out
}

#' Make felm slightly easier to use for someone who forgets to check things
#'
#' @param formula A formula to estimate
#' @param data Data to use
#' @param ... Passed to [lfe::felm()]
#' @param dates_as_factor Should dates be converted to factors before running felm? (Default true)
#' @param strict Should felm's warnings be treated as errors? Default true.
#' @return The result from `felm()`
#' @example
#' felm_strict(cyl ~ wt, mtcars)
#' @export
felm_strict <- function(formula, data, ..., dates_as_factor = TRUE, strict = TRUE) {
  if (strict) {
    # Just like normal felm, but stricter about warnings.
    # (these are almost always a serious problem and should be treated as errors)
    orig_warn <- getOption('warn')
    on.exit(options(warn = orig_warn), add = TRUE)
    options(warn = 2)
  }
  is_date <- function(x) {
    methods::is(x, "Date")
  }
  model_data <- stats::model.frame(formula, data)
  if (dates_as_factor) {
    # Force date variables to be factor if they're Dates and in the formula
    data <- dplyr::mutate_if(data, is_date, as.factor)
  }
  return(lfe::felm(formula = formula, data = model_data, ...))
}


#' Truncate a string to be no longer than a specific number of bytes
#'
#' @param x Vector of strings to truncate
#' @param max_len Max length (in bytes). Can be one number or have the same lenth as `x`
#' @return x, truncated to `max_len` bytes each. The function won't chop
#'   multi-byte characters in half, so the result might be shorter than `max_len`
#' @note Requires package `stringi`. This implementation is really inefficient.
#' @seealso [stringi::stri_sub()] and [base::substr()]
#' @examples
#' truncate_bytes(c("ab", "cde"), c(1, 2))
#' latin1_str <- "fa\xE7ile"
#' Encoding(latin1_str) <- "latin1"
#' truncate_bytes("latin1_str, 3)
#' truncate_bytes("ὯaὯa", 2) # empty string
#' truncate_bytes("ὯaὯa", 6) # Ὧa only
#' @export
truncate_bytes <- function(x, max_len = Inf) {
  stopifnot(length(max_len) == 1L || length(max_len) == length(x), !anyNA(max_len), all(max_len >= 0), !anyNA(x))
  if (all(stringi::stri_numbytes(x) < max_len)) {
    return(x)
  }
  if (all(stringi::stri_numbytes(x) == stringi::stri_length(x))) {
    # Special-case for anything that fits in one byte (ascii+)
    return(stringi::stri_sub(x, length = max_len))
  }
  truncate_the_hard_way <- function(one_x, one_max_len) {
    while(stringi::stri_numbytes(one_x) > one_max_len) {
      one_x <- stringi::stri_sub(one_x, from = 1L, length = stringi::stri_length(one_x) - 1L)
    }
    one_x
  }
  out <- purrr::map2_chr(x, max_len, truncate_the_hard_way)
  return(out)
}
