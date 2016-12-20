
#' @importFrom tools write_PACKAGES

get_old_packages <- function(dir, ...) {
  ## If there is a PACKAGES file, create the DB from that,
  ## Otherwise create a PACKAGES file first
  if (file.exists(rds <- file.path(dir, "PACKAGES.rds"))) {
    readRDS(rds)

  } else if (file.exists(gz <- file.path(dir, "PACKAGES.gz"))) {
    read.dcf(gzfile(gz))

  } else if (file.exists(plain <- file.path(dir, "PACKAGES"))) {
    read.dcf(plain)

  } else {
    write_PACKAGES(dir, ...)
    read.dcf(plain)
  }
}
list_package_files <- function(dir, type) {
  package_pattern <- switch(
    type,
    "source" = "_.*\\.tar\\..*$",
    "mac.binary" = "_.*\\.tgz$",
    "win.binary" = "_.*\\.zip$")
  list.files(dir, pattern = package_pattern, full.names = TRUE)
}

write_packages_files <- function(dir, db_file) {
  df <- db_all_packages(db_file)
  txt <- get_packages_txt(df)

  pkgs <- c(plain = "PACKAGES", gz = "PACKAGES.gz", rds = "PACKAGES.rds")
  pkgs_new <- structure(paste0(pkgs, ".new"), names = names(pkgs))

  ## PACKAGES
  cat(txt, file = file.path(dir, pkgs_new["plain"]))

  ## PACKAGES.gz
  pkggz <- gzfile(file.path(dir, pkgs_new["gz"]), open = "w+")
  cat(txt, file = pkggz)
  close (pkggz)

  ## PACKAGES.rds
  saveRDS(as.matrix(df), file.path(dir, pkgs_new["rds"]))

  ## Move them in place
  ## TODO: should we create .bak files, and roll back on error?
  file.rename(file.path(dir, pkgs_new), file.path(dir, pkgs))

  ## Number of packages added
  invisible(nrow(df))
}

#' @importFrom utils head

get_packages_txt <- function(df) {
  mat <- t(as.matrix(cbind(df, "")))
  nms <- rep(c(paste0(colnames(df), ": "), ""), nrow(df))
  nms <- nms[!is.na(mat)]
  mat <- mat[!is.na(mat)]
  mat <- gsub("\n", "\n        ", mat)
  txt <- head(paste0(nms, mat), -1)
  txt <- sub(": \n", ":\n", txt, fixed = TRUE)
  paste(txt, collapse = "\n")
}
