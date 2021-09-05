
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
#' In most cases, just apply `tolower(tools::file_ext()), but include a special
#' case for connections.
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


#' Read and reconstitute Stata file.
#'
#' @param ... Arguments passed to [haven::read_dta]
#' @return The data, as read by [haven::read_dta], but with less attribute nonsense.
#'
#' @export
read_dta <- function(...) {
  stop_if_not_installed("haven", "read_dta requires the package haven")
  stop_if_not_installed("dplyr", "read_dta requires the package dplyr")
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


#' DEPRECATED: Message to the user
#'
#' @param ... Arguments to `message`
#' @param verbose Actually use `message`? (default TRUE)
#' @return NULL
#' Just a thin wrapper around [message()]
#' @export
narrate <- function(..., verbose = TRUE) {
  stop("narrate is deprecated. Use rlang::inform")
  if (verbose) {
    message(...)
  }
  invisible()
}


#' DEPRECATED: Turn a vector into one long string, used to generate messages
#'
#' @param x Vector to convert
#' @param quoted Should the individual elements have quotes around them?
#' @param add_and Should we add an 'and' before the last element
#' @return A string (length 1 character vector)
#' @examples
#' \dontrun{
#' vec2string(c(1,2,3))  # "'1', '2', and '3'"
#' }
#' @export
vec2string <- function(x, quoted = TRUE, add_and = TRUE) {
  stop("vec2string is deprecated - use glue::glue instead")
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
#' @examples
#' if (requireNamespace("lfe", quietly=TRUE)) {
#'   felm_strict(cyl ~ wt, mtcars)
#' }
#'
#' NOTE: this function is soft-deprecated and will be removed sometime in the
#' future. Use the `fixest` package instead.
#'
#' @export
felm_strict <- function(formula, data, ..., dates_as_factor = TRUE, strict = TRUE) {
  stop_if_not_installed("lfe", "felm_strict requires lfe")

  if (strict) {
    # Just like normal felm, but stricter about warnings.
    # (these are almost always a serious problem and should be treated as errors)
    orig_warn <- getOption('warn')
    on.exit(options(warn = orig_warn), add = TRUE)
    options(warn = 2)
  }
  is_date <- function(x) {
    inherits(x, "Date")
  }
  model_data <- stats::model.frame(formula, data)
  if (dates_as_factor) {
    stop_if_not_installed("dplyr", "felm_strict requires dplyr when dates_as_factor is true")
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
#' truncate_bytes(latin1_str, 3)
#' truncate_bytes("ὯaὯa", 2) # empty string
#' truncate_bytes("ὯaὯa", 6) # Ὧa only
#' @export
truncate_bytes <- function(x, max_len = Inf) {
  stopifnot(length(max_len) == 1L || length(max_len) == length(x), !anyNA(max_len), all(max_len >= 0), !anyNA(x))
  stop_if_not_installed("stringi")
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

#' Set some tikzDevice options
#'
#' @param font Name of a font, passed to `\\setmainfont`
#' @param cache_dir Directory to store `tikzDevice_cache` file
#' @return Nothing
#' @export
configure_tikzDevice <- function(font = "Libertinus Serif", cache_dir = "~/.R") {

  # Note that this requireNamespace calls tikzDevice:::.onLoad, which looks for
  # a working version of latex/lualatex/xelatex. If it's in a non-standard place,
  # you can provide it as an environment variable or an option. For convenience,
  # the arguments of this function are also passed to options().
  has_tikz_device <- requireNamespace("tikzDevice", quietly = TRUE)
  if (!has_tikz_device) {
    warning("Package tikzDevice not installed. You need to install it before ",
            "you can use tikzDevice.")
    return()
  }
  if (utils::packageVersion("tikzDevice") <= "0.12") {
    warning("tikzDevice is installed, but will not work well with ggplot2.\n",
            "Please call:\n  devtools::install_github('daqana/tikzDevice')")
    return()
  }
  if (dir.exists(cache_dir)) {
    options(tikzMetricsDictionary = file.path(cache_dir, "tikzDevice_cache"))
  } else {
    message("Cache directory '", cache_dir, "' doesn't exist. Not creating cache file.")
  }
  options(
    tikzDefaultEngine = "luatex",
    tikzLualatexPackages = c(
      "\\usepackage{tikz}\n",
      # "\\IfFileExists{luatex85.sty}{\\usepackage{luatex85}}{}\n",
      "\\usepackage[active,tightpage,psfixbb]{preview}\n",
      "\\usepackage{fontspec}\n",
      "\\PreviewEnvironment{pgfpicture}\n",
      "\\setlength\\PreviewBorder{0pt}\n",
      "\\usepackage{microtype}",
      paste0("\\setmainfont{", font, "}\n")
    ),
    tikzUnicodeMetricPackages = c(
      "\\usetikzlibrary{calc}\n"
    )
  )
}


#' Rename columns in a table
#'
#' @param .tbl Table to rename
#' @param .vars A named character vector, where the names are the new column
#'   names and the values are existing column names.
#' @param strict Should the function raise an error if existing column names
#'   can't be found? (Default TRUE)
#' @return The same .tbl, with some renamed columns
#'
#' Note that this function is the same as `colnames()<-` for in-memory
#' data.frames, but also works for remote tbls. It's similar to pandas'
#' `.rename` method.
#'
#' @examples
#' cols_to_rename <- c(cyl2 = "cyl")
#' rename_cols(mtcars, cols_to_rename)
#' @export
rename_cols <- function(.tbl, .vars, strict = TRUE) {
  stop_if_not_installed("dplyr")
  tbl_names <- colnames(.tbl)
  old_names <- unname(.vars)
  if ((! purrr::is_bare_character(.vars)) ||
    (length(.vars) == 0) ||
    length(names(.vars)) != length(.vars)) {
    stop("Must provide a named character vector of variables to rename. The form should be c(\"new_name\" = \"old_name\")")
  }
  if (anyDuplicated(unname(.vars))) {
    stop("The original names should not be duplicated")
  }
  # Get the index in tbl_names that we're going to rename
  # Will be NA if missing
  rename_idx <- match(old_names, tbl_names)
  if (anyNA(rename_idx)) {
    if (strict) {
      stop("Variables not present to rename:\n  ", paste(old_names[is.na(rename_idx)], collapse = ", "))
    }
    rename_idx <- rename_idx[!is.na(rename_idx)]
  }
  .renaming_fn <- function(x) {
    # new name is stored in the names attribute
    names(.vars[old_names == x])
  }
  out <- dplyr::rename_at(.tbl,
     .vars = dplyr::vars(rename_idx),
     .funs = list(.renaming_fn))
  out
}

#' Get or set memory limits
#'
#' For Linux (or BSD), this function calls [unix::rlimit_as()]
#' For Windows, this function calls [utils::memory.limit()]
#' For Mac OS X, no limiting is available.
#'
#' @param size numeric. Request a new limit, in MiB.
#' @return A vector with the (new) limit, in MiB.
#' @seealso \link[base]{Memory-limits} for other limits.
#' @export
memory_limit <- function(size = NA) {
  os <- get_os()
  if (os == "win") {
    if (is.null(size)) {
      size <- NA
    }
    limit <- utils::memory.limit(size)
  } else if (os == "linux") {
    if (!requireNamespace("unix", quietly=TRUE)) {
      stop("Limiting memory on linux requires the 'unix' package")
    }
    if (is.null(size) || is.na(size)) {
      size <- NULL
    } else {
      size <- size * (1024^2) # rlimit_as expects bytes
    }
    limit <- unix::rlimit_as(size)$cur
  } else {
    warning("Sorry, memory limits on OS X are not supported")
    limit <- NULL
  }
  invisible(limit)
}

.tidy_gsub <- function(x, pattern, replacement, fixed=FALSE) {
  gsub(pattern, replacement, x, fixed=fixed, perl=!fixed)
}

#' Make names nicer to work with
#'
#' @param x A character vector of names
#' @return A transformed version of those names
#' @seealso [make.names()] and [tibble::tibble()]'s `.name_repair` argument
#'
#' Resulting names are guaranteed to be unique, and will almost certainly be
#' syntactic.
#'
#' @examples
#' make_better_names(c("Country", "GDP $M", "Coast.Length"))
#' #> [1] "country" "gdp_mn"  "coast_length"
#' # Note that the guarantee to make the names unique can lead to some surprises
#' # for example, "a_and_b" becomes "a_and_b_3" in this case:
#' make_better_names(c("a and b", "a-and-b", "a.and.b", "a_and_b"))
#' #> c("a_and_b", "a_and_b_1", "a_and_b_2", "a_and_b_3")
#' # Here's a way to have a bad time:
#' make_better_names(c("", "x", "X", "x_1"))
#' @export
make_better_names <- function(x) {
  better_names <- x |>
    .tidy_gsub("%", "pct", fixed=TRUE) |>
    .tidy_gsub("$M", "mn", fixed=TRUE) |>
    .tidy_gsub("$B", "bn", fixed=TRUE) |>
    .tidy_gsub("$T", "tn", fixed=TRUE) |>
    make.names() |>
    tolower() |>
    .tidy_gsub(".", "_", fixed=TRUE) |>
    .tidy_gsub("_+", "_") |>
    .tidy_gsub("^_|_$", "")
  loop_count <- 0L
  while (anyDuplicated(better_names) != 0) {
    loop_count <- loop_count + 1L
    if (loop_count > 100L) {
      stop("Failed to make names unique!")
    }
    better_names <- make.names(better_names, unique=TRUE) |>
      .tidy_gsub(".", "_", fixed=TRUE)
  }
  better_names
}

#' Coarsen a date to monthly resolution
#'
#' @param x A date vector
#' @return A date vector of the same length, with the day-of-month always 1
#'
#' @examples
#' make_monthly(as.Date(c("2000-01-01", "2000-01-02")))
#' #> [1] "2000-01-01" "2000-01-01"
#' @export
make_monthly <- function(x) {
  lubridate::make_date(
    lubridate::year(x),
    lubridate::month(x),
    1L
  )
}

#' Re-render a document as it changes
#'
#' @param input Filename of rmarkdown file
#' @param ... Arguments passed on to [rmarkdown::render()]
#' @param renderer A function to call to render the file. Defaults to
#'   [rmarkdown::render()] for `.md` or `.rmd` files and [tinytex::lualatex()]
#'   for `.tex` file. Provide a function for any other function.
#'
#' Use Ctrl + break (windows), Esc (mac gui) or Ctrl + C (command  line) to stop
#' the watcher.
#' @export
auto_render <- function(input, ..., renderer=NULL) {
  if (!requireNamespace("testthat", quietly=TRUE)) {
    rlang::abort("testthat package is required to watch for changes")
  }
  if (!file.exists(input)) {
    rlang::abort(glue::glue("File {input} not found"))
  }
  if (is.null(renderer)) {
    renderer <- switch(get_ext(input),
      "rmd" = rmarkdown::render,
      "md" = rmarkdown::render,
      "tex" = tinytex::lualatex,
      rlang::abort(glue::glue("No default renderer for {input}"))
    )
  }
  watcher <- function(added, deleted, modified) {
    changed <- normalizePath(c(added, modified))
    if (length(changed) > 0) {
      renderer(changed[1], ...)
    }
    TRUE
  }
  renderer(input, ...)
  # Easier to use testthat::watch than write our own watcher.
  # This will be slightly inexact if the filename matches multiple files when
  # interpreted as a pattern in dir(). (Only a problem if those files are also
  # changing)
  indir <- dirname(normalizePath(input, mustWork=TRUE, winslash="/"))
  pattern <- basename(input)
  testthat::watch(indir, callback=watcher, pattern=pattern, hash=TRUE)
}


#' Detect function conflicts between packages
#'
#' @return A list of character vectors of conflicting functions (invisibly)
#'
#' Mostly useful for the side-effect of printing conflicting functions.
#'
#' This function draws heavily from [tidyverse::tidyverse_conflicts()], except
#' it finds any conflicts (not just with tidyverse packages), and it's got no
#' dependencies beyond base R (>= 3.0ish)
#'
#' Note that this function detects conflicts between *attached* packages, not
#' packages and user-defined functions, or packages that haven't been attached.
detect_function_conflicts <- function() {
  invert <- function (x) {
    if (length(x) == 0) {
      return()
    }
    stacked <- utils::stack(x)
    tapply(as.character(stacked$ind), stacked$values, list)
  }
  ls_env <- function (env) {
    x <- ls(pos = env)
    if (identical(env, "package:dplyr")) {
      x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
    }
    x
  }
  confirm_conflict <- function (packages, name) {
    objs <- lapply(packages, function(x) get(name, pos = x))
    objs <- Filter(is.function, objs)
    if (length(objs) <= 1) {
      return()
    }
    objs <- objs[!duplicated(objs)]
    packages <- packages[!duplicated(packages)]
    if (length(objs) == 1) {
      return()
    }
    # Ignore "kronecker" and "body<-" conflicts between base and methods.
    if (setequal(packages, c("package:base", "package:methods"))) {
      return()
    }
    packages
  }
  length_gt_1 <- function(x) {
    length(x) > 1
  }
  compact <- function(x) {
    # purrr::compact, implemented in base R
    Filter(Negate(is.null), x)
  }
  conflict_message <- function(conflict_funs) {
    pkgs <- lapply(conflict_funs, function(x) .tidy_gsub(x, "^package:", ""))
    winner <- lapply(pkgs, function(x) x[1])
    others <- lapply(pkgs, function(x) x[-1])
    msg <- vector(length=length(winner), mode="character")
    for (i in seq_along(winner)) {
      # Produce a line like "dplyr::filter() masks stats::filter()"
      fn_name <- names(winner[i])
      # others[i] might have length > 1.
      # paste() duplicates fn_name before collapsing
      msg[i] <- paste0(winner[i], "::", fn_name, "() masks ",
        paste0(others[i], "::", fn_name, "()", collapse=", ")
      )
    }
    paste0(msg, collapse="\n")
  }

  envs <- grep("^package:", search(), value = TRUE)
  names(envs) <- envs
  objs <- invert(lapply(envs, ls_env))
  conflicts <- Filter(length_gt_1, objs)
  conflict_funs <- mapply(confirm_conflict, conflicts, names(conflicts))
  conflict_funs <- compact(conflict_funs)
  msg <- conflict_message(conflict_funs)
  message(msg)
  invisible(conflict_funs)
}


stop_if_not_installed <- function(pkg_name, err_msg = paste(pkg_name, "is required")) {
  if (!requireNamespace(pkg_name, quietly=TRUE)) {
    stop(err_msg, .call=FALSE)
  }
}
