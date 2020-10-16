

#' Save a ggplot plot
#'
#' @param plot The plot created by [ggplot2::ggplot()]
#' @param filename The filename(s) to save (save type depends on extension)
#' @param scale_mult A scale multiplier on the size. Defaults to 1; bigger
#' numbers use a larger canvas.
#' @param bg The background color, passed to the output device. The default
#' is "transparent". If set to "transparent", the plot will be modified to make
#' the `panel.background`, `plot.background`, `legend.background`, and
#' `legend.box.background` transparent as well. Set it to "white" to retain
#' the normal ggplot behavior.
#' @param device The device to use. Default depends on filename extension. Uses
#' cairo_pdf devices when available. Use "tex" or "tikz" to save with [tikzDevice::tikz()].
#' @param reproducible Logical. Should we try to make the plot reproducible by
#' resetting the embedded timestamp? Defaults to false unless the `SOURCE_DATE_EPOCH`
#' environment variable or `SOURCE_DATE_EPOCH` R option is set. If `reproducible`
#' is `TRUE` and `SOURCE_DATE_EPOCH` isn't set, the timestamp we reset to is
#' 1970-01-01 00:00:00 UTC. Other sources of non-reproducibility aren't handled.
#' Requires system `sed` command.
#'
#' @return The plot (invisibly)
#' @seealso [ggplot2::ggsave()] https://reproducible-builds.org/docs/source-date-epoch/
#'
#' Note that creating reproducible outputs currently depends on the system
#' command `sed`, which isn't installed by default on Windows.
#' This implementation of reproducible outputs is incompatible with multi-file
#' outputs, like tikz.
#'
#' @export
save_plot <- function(plot, filename, scale_mult = 1, bg = "transparent", device=NULL, reproducible=NULL) {
  force(plot)
  if (length(filename) > 1) {
    for (fl in filename) {
      save_plot(
        plot=plot, filename=fl,
        scale_mult=scale_mult, bg=bg, device=device, reproducible=reproducible
      )
    }
    return(invisible(plot))
  }
  stopifnot(dir.exists(dirname(filename)))
  if (identical(bg, "transparent")) {
    plot <- plot + ggplot2::theme(
      # Borrowed from https://stackoverflow.com/a/41878833
      panel.background      = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.background       = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.background     = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )
  }
  if (is.null(device)) {
    dev_list <- list(
      pdf = function(filename, ...) grDevices::cairo_pdf(filename=filename, ...),
      tex = function(filename, ...) tikzDevice::tikz(filename=filename, ...),
      tikz = function(filename, ...) tikzDevice::tikz(filename=filename, ...)
    )
    device <- dev_list[[tolower(tools::file_ext(filename))]]
  }
  # By default, check if R option or system variable SOURCE_DATE_EPOCH is set
  # and can be parsed as a POSIXct. If so, we'll try to reset the file timestamp
  # to that datetime. If `reproducible` is just `TRUE`, then we'll reset to
  # 1970-01-01 00:00:00 UTC
  if (is.null(reproducible)) {
    reproducible <- !is.null(read_source_date_epoch())
  }
  if (reproducible && (!is_sed_available())) {
    warning("Reproducible graphics (currently) depend on having a system sed command")
    reproducible <- FALSE
  }
  if (reproducible) {
    # Make a tempfile with the same extension as filename so type can be parsed
    ggsave_file <- tempfile(fileext=paste0(".", tools::file_ext(filename)))
    dev_category <- get_dev_category(filename, device)
    if (dev_category == "tikz") {
      rlang::abort(glue::glue("Creating reproducible multi-part files, like {dev_category}, is not supported"))
    }
  } else {
    ggsave_file <- filename
  }
  # Save in the ratio of a beamer slide.
  # This aspect ratio works pretty well for normal latex too
  ggplot2::ggsave(filename = ggsave_file, plot = plot,
    width = 6.3 * scale_mult, height = 3.54 * scale_mult, units = "in",
    device = device, bg = bg)
  if (reproducible) {
    reset_datestamp(infile=ggsave_file, outfile=filename, category=dev_category)
  }
  invisible(plot)
}


read_source_date_epoch <- function() {
  source_data_epoch <- getOption("SOURCE_DATE_EPOCH") %||% # First choice
    Sys.getenv("SOURCE_DATE_EPOCH")
  datetime <- as.POSIXct(as.numeric(source_data_epoch), origin="1970-01-01", tz="UTC")
  if (is.na(datetime)) {
    datetime <- NULL
  }
  datetime
}


get_dev_category <- function(filename, device) {
  if (is.function(device)) {
    if (identical(device, grDevices::pdf) || identical(device, grDevices::cairo_pdf)) {
      category <- "pdf"
    } else if (identical(device, grDevices::cairo_ps)) {
      category <- "cairo_ps"
    } else if (identical(device, grDevices::jpeg)) {
      category <- "jpeg"
    } else if (requireNamespace("tikzDevice", quietly=TRUE) && identical(device, tikzDevice::tikz)) {
      category <- "tikz"
    } else {
      category <- "uncorrected"
    }
    if (category != "uncorrected") {
      return(category)
    }
  }
  ext <- tolower(tools::file_ext(filename))
  if (any(c(device, ext) %in% c("pdf", "cairo_pdf"))) {
    category <- "pdf"
  } else if (any(c(device, ext) == "cairo_ps")) {
    category <- "cairo_ps"
  } else if (any(c(device, ext) %in% c("jpg", "jpeg"))) {
    category <- "jpeg"
  } else if (any(c(device, ext) %in% c("tex", "tikz"))) {
    category <- "tikz"
  } else {
    category <- "uncorrected"
  }
  category
}


#' @importFrom rlang "%||%"
reset_datestamp <- function(infile, outfile, category) {
  if (category == "uncorrected") {
    file.rename(infile, outfile)
    return(outfile)
  }
  datestring <- switch(category,
    cairo_ps = "%%CreationDate: ",
    jpeg = "%%CreationDate: ",
    pdf = "  /CreationDate ",
    tikz = "% Created by tikzDevice "
  )
  timeformat <- switch(category,
    cairo_ps = "%a %b %d %H:%M:%S %Y",
    jpeg = "%a %b %d %H:%M:%S %Y",
    pdf = "(D:%Y%m%d%H%M%S-00'00)",
    tikz = "%Y-%m-%d %H-%M-%S"
  )
  desired_datetime <- read_source_date_epoch() %||% # First choice
    as.POSIXct(0, origin="1970-01-01", tz="UTC") # third choice
  inregex <- paste0(datestring, ".*")
  outregex <- paste0(
    datestring,
    strftime(desired_datetime, format=timeformat, tz="UTC")
  )
  substitute_text(infile, outfile, inregex, outregex)
}

is_sed_available <- function() {
  suppressWarnings(rc <- system2("sed", "--version", stderr=FALSE, stdout=FALSE))
  rc == 0
}

substitute_text <- function(infile, outfile, inregex, outregex) {
  # This could definitely be done in R, but it seems like a pain.
  stopifnot(is.character(infile), length(infile) == 1, is.character(outfile),
    length(outfile) == 1, infile != outfile
  )
  if (any(grepl('"', c(inregex, outregex), fixed=TRUE))) {
    stop("This substitution function isn't designed to handle regex that include double quotes. Sorry.")
  }
  if (!is_sed_available()) {
    stop("Text substitution depends on having the system `sed` command available")
  }
  sed_regex <- paste0(
    '"s/',
    gsub("/", "\\/", inregex, fixed=TRUE),
    "/",
    gsub("/", "\\/", outregex, fixed=TRUE),
    '/"'
  )
  # cat(sed_regex)
  # cat("\n")
  # For reasons that aren't clear to me, stdout=outfile doesn't work.
  rc <- system2("sed", c(sed_regex, infile, " > ", outfile))
  if (rc != 0) {
    stop("sed command failed!\n",
      "  Regex:  ", sed_regex, "\n",
      "  Input:  ", infile, "\n",
      "  Output: ", outfile, "\n"
    )
  }
  if ((!file.exists(outfile)) || (file.size(outfile) == 0 && file.size(infile) > 0)) {
    stop("Output file ", outfile, " was not created successfully")
  }
  invisible(outfile)
}
