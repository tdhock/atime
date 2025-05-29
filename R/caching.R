setup_cache <- function(cache_root, verbose) {
  if (is.null(cache_root)) {
    if (!requireNamespace("rappdirs", quietly = TRUE)) {
      install.packages("rappdirs")
    }
    cache_root <- rappdirs::user_cache_dir("atime_installed_pkgs", "atime")
  }
  dir.create(cache_root, showWarnings = FALSE, recursive = TRUE)
  if (verbose) message("Using cache directory: ", cache_root)
  cache_root
}

try_cache_restore <- function(target, cache, verbose) {
  marker <- file.path(cache, "_ATIME_INSTALL_SUCCESSFUL")
  if (!file.exists(marker)) return(FALSE)
  
  if (verbose) message("Restoring from cache: ", basename(cache))
  unlink(target, recursive = TRUE, force = TRUE)
  dir.create(target, recursive = TRUE)
  
  files <- list.files(cache, all.files = TRUE, no.. = TRUE)
  if (length(files) == 0) return(FALSE)
  
  success <- all(file.copy(file.path(cache, files), target, recursive = TRUE))
  success && file.exists(file.path(target, "DESCRIPTION"))
}

update_cache <- function(source, cache, verbose) {
  if (verbose) message("Updating cache: ", basename(cache))
  unlink(cache, recursive = TRUE, force = TRUE)
  dir.create(cache, recursive = TRUE)
  files <- list.files(source, all.files = TRUE, no.. = TRUE)
  if (length(files) == 0) return()
  file.copy(file.path(source, files), cache, recursive = TRUE)
  # this marker is to ensure the previous steps succeeded
  writeLines(as.character(Sys.time()), file.path(cache, "_ATIME_INSTALL_SUCCESSFUL"))
}