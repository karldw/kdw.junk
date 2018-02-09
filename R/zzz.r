
.onAttach <- function(libname, pkgname) {
  rule <- function (pad = "-", gap = 2L) {
    paste0(rep(pad, getOption("width") - gap), collapse = "")
  }

  # Borrow this method from dplyr's treatment of plyr.
  load_order_message <- "NOTE: You have loaded {pkg_name} after kdw.junk. Since kdw.junk overwrites some {pkg_name} functions, this is likely not what you want."

  setHook(packageEvent("haven", "attach"), function(...) {
    packageStartupMessage(rule())
    packageStartupMessage(glue::glue(load_order_message, pkg_name = "haven"))
    packageStartupMessage(rule())
  })
}
