
format_iso_8601 <- function (date) {
  format(as.POSIXlt(date, tz = "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
}

pkgname_from_filename <- function(files) {
  vapply(strsplit(basename(files), "_", fixed = TRUE), "[[", "", 1)
}

#' @importFrom utils getFromNamespace

`%:::%` <- function(pkg, fun) {
  getFromNamespace(fun, asNamespace(pkg))
}
