context("testing reproducible graphics functions")

# Note: these tests, particularly on PDFs, are a little flaky. That is,
# sometimes the generated PDFs are actually different in ways that aren't just
# the timestamp, but actually in the contents of the deflate block.
# I don't know.

if (requireNamespace("ggplot2", quietly=TRUE)) {
  PLOT <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color=disp)) +
    ggplot2::geom_point()
}
SED_AVAILABLE <- system2("sed", "--version", stderr=FALSE, stdout=FALSE) == 0

check_files_exist <- function(files, allow_empty=FALSE) {
  # Waits for a while, returns TRUE/FALSE for whether files eventually exist
  rate <- purrr::rate_backoff(pause_base=0.01, pause_min=0.005, pause_cap=10, max_times=Inf)
  files_exist <- FALSE
  for (i in 1:10) {
    file_sizes <- file.size(files)
    if ((!anyNA(file_sizes)) && (allow_empty || all(file_sizes > 0))) {
      files_exist <- TRUE
      break
    }
    purrr::rate_sleep(rate, quiet=FALSE)
  }
  files_exist
}

expect_files_equal <- function(f1, f2, allow_empty=FALSE) {
  files_exist <- check_files_exist(c(f1, f2), allow_empty=allow_empty)
  res <- files_exist && (length(unique(tools::md5sum(c(f1, f2)))) == 1L)

  # Note: I do still want to run all the code above to give the files a chance
  err_msg <- ifelse(
    files_exist,
    paste0("Files {f1} and {f2} are not the same\n",
    "(sizes are {file.size('f1')} and {file.size('f2')})"),
    "Files {f1} or {f2} don't exist"
  )
  testthat::expect(res, glue::glue(err_msg))
  invisible(f1)
}

test_that("plotting code runs", {
  skip_on_ci()
  skip_if_not_installed("ggplot2")
  temp_pdf <- tempfile(fileext = ".pdf")
  save_plot(PLOT, temp_pdf, reproducible=FALSE)
  expect_true(check_files_exist(temp_pdf))

  skip_if_not_installed("tikzDevice")
  temp_tikz <- tempfile(fileext=".tikz")
  save_plot(PLOT, temp_tikz, reproducible=FALSE)
  expect_true(check_files_exist(temp_tikz))
})

test_that("device-as-function plots are made reproducible", {
  skip_if_not_installed("ggplot2")
  skip_if_not(SED_AVAILABLE)
  skip_on_ci()
  devices <- list(
    # function(filename, ...) grDevices::pdf(file=filename, ...),
    grDevices::cairo_ps,
    grDevices::cairo_pdf,
    grDevices::png,
    function(filename, ...) grDevices::postscript(file=filename, ...),
    grDevices::jpeg
  )
  save_plot_return_tempfile <- function(dev) {
    tf <- tempfile()
    save_plot(PLOT, filename=tf, device=dev, reproducible=TRUE)
    tf
  }

  tf1 <- purrr::map_chr(devices, save_plot_return_tempfile)
  Sys.sleep(1) # sleep so timestamps differ
  tf2 <- purrr::map_chr(devices, save_plot_return_tempfile)
  purrr::walk2(tf1, tf2, expect_files_equal)
  unlink(c(tf1, tf2))
})

test_that("implicit device is reproducible", {
  skip_if_not_installed("ggplot2")
  skip_if_not(SED_AVAILABLE)
  skip_on_ci()
  tf1 <- tempfile(fileext=".pdf")
  tf2 <- tempfile(fileext=".pdf")
  tf3 <- tempfile(fileext=".jpg")
  tf4 <- tempfile(fileext=".jpg")

  save_plot(PLOT, tf1, reproducible=TRUE)
  save_plot(PLOT, tf2, reproducible=TRUE)
  Sys.sleep(1) # sleep so timestamps differ
  save_plot(PLOT, tf3, reproducible=TRUE)
  save_plot(PLOT, tf4, reproducible=TRUE)
  expect_files_equal(tf1, tf2)
  expect_files_equal(tf3, tf4)
  unlink(c(tf1, tf2, tf3, tf4))
})

test_that("non-reproducible remain non-reproducible", {
  skip_on_ci()
  skip_if_not_installed("ggplot2")
  tf1 <- tempfile(fileext=".pdf")
  tf2 <- tempfile(fileext=".pdf")

  save_plot(PLOT, tf1, reproducible=FALSE)
  Sys.sleep(1) # sleep so timestamps differ
  save_plot(PLOT, tf2, reproducible=FALSE)
  files_exist <- check_files_exist(c(tf1, tf2))

  checksums <- unname(tools::md5sum(c(tf1, tf2)))
  expect_true(!anyNA(checksums))
  expect_true(checksums[1] != checksums[2])
  unlink(c(tf1, tf2))
})

test_that("read_source_date_epoch works", {
  expect_null(read_source_date_epoch())
  on.exit(options(SOURCE_DATE_EPOCH=NULL))
  options(SOURCE_DATE_EPOCH=1586480730)
  expect_true(as.Date(read_source_date_epoch()) == as.Date("2020-04-10"))
})

test_that("get device categories works", {
  expect_equal(get_dev_category("x.pdf", NULL), "pdf")
  expect_equal(get_dev_category("x.pdf", grDevices::pdf), "pdf")
  expect_equal(get_dev_category("x.jpeg", grDevices::cairo_ps), "cairo_ps")
  expect_equal(get_dev_category("x.jpg", NULL), "jpeg")
  expect_equal(get_dev_category("x", "tikz"), "tikz")
})

test_that("writing multiple files works", {
  skip_if_not_installed("ggplot2")
  skip_on_ci()
  tf1 <- tempfile(fileext=".pdf")
  tf2 <- tempfile(fileext=".png")
  save_plot(PLOT, c(tf1, tf2), reproducible=FALSE)
  files_exist <- check_files_exist(c(tf1, tf2))
  expect_true(files_exist)
  unlink(c(tf1, tf2))
})
