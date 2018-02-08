

#' Cite currently loaded pacakges
#'
#' @param outfile Write results to a file. The default, NULL, doesn't write a file.
#' @param include Extra packages to include, beyond those that have been loaded.
#'  The default, "base", includes a citation for R itself.
#' @return A list of citations
#'
#' Note that some packages provide multiple citations. This function will pick only the
#' first one they provided.
cite_everything <- function(outfile = NULL, include = "base") {
  prev_enc <- getOption("encoding")
  on.exit(options(encoding = prev_enc), add = TRUE)
  options(encoding = "utf8")
  cite_one <- function(pkg, ...) {
    cite <- utils::toBibtex(utils::citation(pkg, ...))
    if (identical(pkg, "base")) {
      pkg <- "r_core"
    }

    # Handle cases where authors list multiple citations (only pick the first)
    cite_lines <- paste(cite)
    empty_lines <- which(cite_lines == "")
    bracket_lines <- which(cite_lines == "}")  # lone bracket on a line
    if (length(empty_lines) + 1 != length(bracket_lines)) {
      stop("Multi-citation detection failed for ", pkg)
    }
    if (length(empty_lines) > 0) {
      if (! all((empty_lines - 1) %in% bracket_lines)) {
        stop("Multi-citation detection failed for ", pkg)
      }
      # Take only the first cite by taking all the lines up until the first
      # empty line (which we just confirmed is after a closing bracket)
      cite <- cite[seq_len(bracket_lines[1])]
      class(cite) <- "Bibtex"
    }
    cite[1] <- gsub("(@[a-zA-Z]+{),", paste0("\\1", pkg, ","), cite[1], perl = TRUE)

    # Also adjust to the biblatex "@software" and set a version
    # cite[1] <- gsub("@Manual{", "@software{", cite[1], fixed = TRUE)
    version_row <- which(names(cite) == "note")
    version_text <- gsub("R package version ", "",
                         cite[version_row], fixed = TRUE)
    cite[version_row] <- version_text
    # names(cite)[version_row] <- "version"

    return(cite)
  }

  # Don't auto-include RevoUtilsMath (can still be manually included with the include param)
  pkg_names <- setdiff(names(utils::sessionInfo()$otherPkgs), "RevoUtilsMath")

  include <- include[! is.na(include)]
  pkg_names <- c(pkg_names, include)

  cites <- lapply(pkg_names, cite_one)

  if (!is.null(outfile)) {
    cites_text <- paste(lapply(cites, paste, collapse = "\n"), collapse = "\n\n")
    writeLines(cites_text, outfile, useBytes = TRUE)
  }
  return(cites)
}
