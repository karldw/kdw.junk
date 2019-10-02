
#' Find the user's Dropbox folder
#'
#' @param dropbox_path_override Override looking for the Dropbox path with this
#'   (defaults to envionment variable DROPBOX_PATH, which is not an official Dropbox setting)
#' @return A vector of the user's dropbox folder(s)
#' @export
dropbox_home <- function(dropbox_path_override = Sys.getenv("DROPBOX_PATH")) {
  if (!requireNamespace("jsonlite", quietly=TRUE)) {
    stop("dropbox_home requires the jsonlite package")
  }
  if (dropbox_path_override != "") {
    if (dir.exists(dropbox_path_override)) {
      return(dropbox_path_override)
    } else {
      warning("Environment variable DROPBOX_PATH is set to a non-directory. Ignoring.")
    }
  }
  .system <- get_os()

  if (.system == "win") {
    appdata_paths <- Sys.getenv(c("APPDATA", "LOCALAPPDATA"))
    info_path <- file.path(appdata_paths[1], "Dropbox", "info.json")
    if (! file.exists(info_path)) {
      info_path <- file.path(appdata_paths[2], "Dropbox", "info.json")
    }
  } else if (.system %in% c("linux", "mac")) {
    info_path <- path.expand("~/.dropbox/info.json")
  } else {
    stop(paste0("Unknown system = ", .system))
  }

  if (! file.exists(info_path)) {
    err_msg <- paste0("Could not find the Dropbox info.json file! (Should be here: '",
                      info_path, "')")
    stop(err_msg)
  }

  dropbox_settings <- jsonlite::fromJSON(info_path)
  paths <- vapply(dropbox_settings, function(account) account$path, FUN.VALUE = "")
  paths <- gsub("\\", "/", paths, fixed = TRUE)
  return(paths)
}
