context("testing reproducible graphics functions")

plt <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color=disp)) +
  ggplot2::geom_point()


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
  devices <- list(
    # grDevices::pdf,
    grDevices::cairo_ps,
    grDevices::cairo_pdf,
    grDevices::png,
    # grDevices::postscript, pain to test because argument is called file
    grDevices::jpeg
  )
  for (d in devices) {
    tf1 <- tempfile()
    tf2 <- tempfile()
    save_plot(plt, tf1, device=d, reproducible=TRUE)
    Sys.sleep(1) # make sure timestamps would be different
    save_plot(plt, tf2, device=d, reproducible=TRUE)
    expect_true(all(file.exists(c(tf1, tf2))))
    expect_gt(file.size(tf2), 0)
    checksums <- unname(tools::md5sum(c(tf1, tf2)))
    expect_equal(checksums[1], checksums[2])
    unlink(c(tf1, tf2))
  }
})

test_that("implicit device is reproducible", {
  tf1 <- tempfile(fileext=".pdf")
  tf2 <- tempfile(fileext=".pdf")
  tf3 <- tempfile(fileext=".jpg")
  tf4 <- tempfile(fileext=".jpg")

  save_plot(plt, tf1, reproducible=TRUE)
  save_plot(plt, tf3, reproducible=TRUE)
  Sys.sleep(1)
  save_plot(plt, tf2, reproducible=TRUE)
  save_plot(plt, tf4, reproducible=TRUE)
  Sys.sleep(0.5)
  checksums <- unname(tools::md5sum(c(tf1, tf2, tf3, tf4)))
  expect_equal(checksums[1], checksums[2])
  expect_equal(checksums[3], checksums[4])
  unlink(c(tf1, tf2, tf3, tf4))
})

test_that("non-reproducible remain non-reproducible", {
  tf1 <- tempfile(fileext=".pdf")
  tf2 <- tempfile(fileext=".pdf")

  save_plot(plt, tf1, reproducible=FALSE)
  Sys.sleep(1)
  save_plot(plt, tf2, reproducible=FALSE)
  checksums <- unname(tools::md5sum(c(tf1, tf2)))
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
