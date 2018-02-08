
#' Get the user's Box folder
#'
#' @return Path to the Box folder, as reported by Box
#'
#' @export
box_home <- function() {
    os <- get_os()
    if (os == 'win') {
        appdata_paths <- Sys.getenv(c('APPDATA', 'LOCALAPPDATA'))

        info_path <- file.path(appdata_paths[1], 'Box Sync', 'sync_root_folder.txt')
        if (! file.exists(info_path)) {
            info_path <- file.path(appdata_paths[2], 'Box Sync', 'sync_root_folder.txt')
        }
    } else if (os == 'mac') {
        mac_path <- '~/Library/Application Support/Box/Box Sync/sync_root_folder.txt'
        info_path <- path.expand(mac_path)
    } else if (os == 'linux') {
        stop("Box doesn't support Linux/unix.  What are you doing?")
    }
    if (! file.exists(info_path)) {
        err_msg <- paste0("Could not find the Box sync_root_folder.txt file! ",
                         "(Should be here: '", info_path, "')")
        stop(err_msg)
    }

    box_dir <- readLines(info_path, warn = FALSE)
    if (! dir.exists(box_dir)) {
        err_msg <- paste0("Box configuration indicated the Box directory was '", box_dir,
                          "', but that doesn't exist.")
        stop(err_msg)
    }
    return(box_dir)
}
