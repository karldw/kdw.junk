#' Make a binscatter plot, allowing for formulas and fixed effects
#'
#' This is a convenience wrapper around [lfe::felm()] and [binsreg::binsreg()],
#' because I want the ease of Stata's binscatter.
#'
#' *Important note:* The standard errors here will not account for the fixed
#' effects. This function is useful for plots, and is not recommended for inference.
#'
#' @param formula A formula to estimate, of the form `y ~ x1 + x2 | fe1 + fe2`
#' @param data Data to use
#' @param x Independent variable of interest. May be a vector (as in `binsreg()`)
#'   or the name of a varaible in `data`. If omitted, the first x variable in
#'   the formula is used.
#' @param ... Other arguments passed to `binsreg`
#' @param weights Optional vector of weights. Interpretation is the same as [lm()].
#'
#' @seealso [binsreg::binsreg()], [lfe::felm()]
#'
#' @examples
#' res <- binscatter(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris)
#' print(res$bins_plot)
#' res <- binscatter(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris, x="Petal.Length")
#' @export
binscatter <- function(formula, data, x = NULL, ..., weights = NULL) {
  if (!requireNamespace("binsreg", quietly=TRUE)) {
    stop("binscatter() relies on the binsreg package. Please install it.")
  }
  if (!requireNamespace("lfe", quietly=TRUE)) {
    stop("binscatter() relies on the lfe package. Please install it.")
  }
  res <- lfe::felm(formula, data, weights=weights, keepCX=TRUE, nostats=TRUE)
  resid_y <- res$resid
  y_name <- colnames(res$cY)
  if (NCOL(res$cX) == 1) {
    resid_x <- res$cX
    other_x <- NULL
    x_name <- colnames(res$cX)[1]
  } else if (!is.null(x)) {
    # x here must be a variable name
    if (! is.character(x) || length(x) != 1 || ! x %in% colnames(res$cX)) {
      stop("Could not find specified x in the centered predictor matrix")
    }
    resid_x <- res$cX[, x, drop=TRUE]
    other_x_names <- setdiff(colnames(res$cX), x)
    other_x <- res$cX[, other_x_names, drop=FALSE]
    x_name <- x
  } else {
    resid_x <- res$cX[, 1, drop=TRUE]
    other_x <- res$cX[, -1, drop=TRUE]
    x_name <- colnames(res$cX)[1]
  }
  # Need to capture output here because the binsreg code insists on printing the
  # plot as soon as it's created, but we want to change the labs.
  out <- capture_plot_output(
    binsreg::binsreg(y=resid_y, x=resid_x, w=other_x, ..., weights=weights)
  )
  out$bins_plot <- out$bins_plot + ggplot2::labs(x=x_name, y=y_name)
  out
}

#' Capture graphical output
#'
#' @param expr An expression to evaluate without printing output
#' @return Whatever expr returned
#'
#' This function takes advantage of R's delayed evaluation. `expr` isn't
#' valuated until it's forced in this function.
#' The output is printed to a temporary png, which is then deleted.
#'
#' @seealso [utils::capture.output()]
#' @export
capture_plot_output <- function(expr) {
  tmp <- tempfile(fileext=".png")
  grDevices::png(tmp)
  ret_value <- force(expr)
  grDevices::dev.off()
  unlink(tmp)
  return(ret_value)
}
