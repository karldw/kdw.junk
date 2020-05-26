context("testing reproducible graphics functions")

# Note: these tests, particularly on PDFs, are a little flaky. That is,
# sometimes the generated PDFs are actually different in ways that aren't just
# the timestamp, but actually in the contents of the deflate block.
# I don't know.

plt <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color=disp)) +
  ggplot2::geom_point()

SED_AVAILABLE <- system2("sed", "--version", stderr=FALSE, stdout=FALSE) == 0

expect_files_equal <- function(f1, f2, allow_empty=FALSE) {

  are_files_equal <- function(files, allow_empty) {
    if (!allow_empty && any(file.size(files) == 0L)) {
      return(FALSE)
    }
    checksums <- tools::md5sum(files)
    (!anyNA(checksums)) & (length(unique(checksums)) == 1L)
  }
  rate <- purrr::rate_backoff(pause_base=0.1, pause_min=0.005, pause_cap=10, max_times=Inf)
  for (i in 1:7) {
    res <- are_files_equal(c(f1, f2), allow_empty)
    if (isTRUE(res)) {
      break
    }
    purrr::rate_sleep(rate, quiet=FALSE)
  }
  # Note: I do still want to run all the code above to give the files a chance
  err_msg <- ifelse(
    all(file.exists(c(f1, f2))),
    paste0("Files {f1} and {f2} are not the same\n",
    "(sizes are {file.size('f1')} and {file.size('f2')})"),
    "Files {f1} or {f2} don't exist"
  )
  testthat::expect(res, glue::glue(err_msg))
  invisible(f1)
}

test_that("plotting code runs", {
  skip_if_not_installed("ggplot2")
  plt <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  temp_pdf <- tempfile(fileext = ".pdf")
  save_plot(plt, temp_pdf)
  expect_true(file.exists(temp_pdf))

  skip_if_not_installed("tikzDevice")
  temp_tikz <- tempfile(fileext=".tikz")
  save_plot(plt, temp_tikz, reproducible=FALSE)
  expect_true(file.exists(temp_tikz))
})

test_that("device-as-function plots are made reproducible", {
  skip_if_not_installed("ggplot2")
  skip_if_not(SED_AVAILABLE)
  devices <- list(
    # function(filename, ...) grDevices::pdf(file=filename, ...),
    grDevices::cairo_ps,
    grDevices::cairo_pdf,
    grDevices::png,
    function(filename, ...) grDevices::postscript(file=filename, ...),
    grDevices::jpeg
  )
  save_plot_return_tempfile <- function(dev, plt) {
    tf <- tempfile()
    save_plot(plt, filename=tf, device=dev, reproducible=TRUE)
    tf
  }

  tf1 <- purrr::map_chr(devices, save_plot_return_tempfile, plt=plt)
  Sys.sleep(1) # sleep so timestamps differ
  tf2 <- purrr::map_chr(devices, save_plot_return_tempfile, plt=plt)
  purrr::walk2(tf1, tf2, expect_files_equal)
  unlink(c(tf1, tf2))
})

test_that("implicit device is reproducible", {
  skip_if_not(SED_AVAILABLE)
  tf1 <- tempfile(fileext=".pdf")
  tf2 <- tempfile(fileext=".pdf")
  tf3 <- tempfile(fileext=".jpg")
  tf4 <- tempfile(fileext=".jpg")

  save_plot(plt, tf1, reproducible=TRUE)
  save_plot(plt, tf2, reproducible=TRUE)
  Sys.sleep(1) # sleep so timestamps differ
  save_plot(plt, tf3, reproducible=TRUE)
  save_plot(plt, tf4, reproducible=TRUE)
  expect_files_equal(tf1, tf2)
  expect_files_equal(tf3, tf4)
  unlink(c(tf1, tf2, tf3, tf4))
})

test_that("non-reproducible remain non-reproducible", {
  tf1 <- tempfile(fileext=".pdf")
  tf2 <- tempfile(fileext=".pdf")

  save_plot(plt, tf1, reproducible=FALSE)
  Sys.sleep(1) # sleep so timestamps differ
  save_plot(plt, tf2, reproducible=FALSE)
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
  tf1 <- tempfile(fileext=".pdf")
  tf2 <- tempfile(fileext=".png")
  save_plot(plt, c(tf1, tf2), reproducible=FALSE)
  expect_true(all(file.exists(c(tf1, tf2))))
})
