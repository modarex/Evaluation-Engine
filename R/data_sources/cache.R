suppressPackageStartupMessages({
  library(here)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

cache_dir <- function() {
  dir <- here("data", "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

cache_path <- function(key) {
  fn <- paste0(gsub("[^A-Za-z0-9_-]", "_", key), ".rds")
  file.path(cache_dir(), fn)
}

cache_read <- function(key, max_age_hours = 24) {
  path <- cache_path(key)
  if (!file.exists(path)) return(NULL)

  if (is.finite(max_age_hours) && max_age_hours > 0) {
    age <- difftime(Sys.time(), file.info(path)$mtime, units = "hours")
    if (!is.finite(age) || as.numeric(age) > max_age_hours) return(NULL)
  }

  tryCatch(readRDS(path), error = function(e) NULL)
}

cache_write <- function(key, value) {
  path <- cache_path(key)
  tryCatch(saveRDS(value, path), error = function(e) invisible(NULL))
  invisible(path)
}

cache_clear <- function(prefix = NULL) {
  files <- list.files(cache_dir(), pattern = "\\.rds$", full.names = TRUE)
  if (!is.null(prefix)) {
    prefix_clean <- gsub("[^A-Za-z0-9_-]", "_", prefix)
    files <- files[startsWith(basename(files), prefix_clean)]
  }
  unlink(files, recursive = FALSE, force = TRUE)
}

cache_read_safe <- function(key, max_age_hours = 24) {
  tryCatch(cache_read(key, max_age_hours = max_age_hours), error = function(e) NULL)
}

cache_write_safe <- function(key, value) {
  tryCatch(cache_write(key, value), error = function(e) invisible(NULL))
}
