
#' Tools for CRAN-like Repositories
#'
#' A set of functions to manage CRAN-like repositories efficiently.
#'
#' @docType package
#' @name cranlike
NULL

#' Create or update PACKAGES* files for a CRAN-like repository
#'
#' This function is similar to [tools::write_PACKAGES()], with some
#' differences:
#' * It always uses the `MD5sum` field.
#' * It defaults to `addFiles = TRUE`, to allow following the packag
#'   files better.
#' * It does not support the `verbose`, `unpacked`, and `subdirs`
#'   arguments currently.
#' * It uses a database to speed up the indexing process, and only
#'   reindexes files that have added, removed or updated.
#'
#' `update_PACKAGES` uses a SQLite database to aid updating PACKAGES*
#' files quickly. It this database does not exist, then it will be created
#' based on the existing PACKAGES* files. If no PACKAGES* files exist,
#' either, then these will be created via `
#'
#' @inheritParams tools::write_PACKAGES
#' @param ... Other arguments are passed to [tools::write_PACKAGES()].
#'   Note that not all invokations will call [tools::write_PACKAGES()],
#'   so these arguments might not be used at all.
#'
#' @family PACKAGES manipulation
#' @export

update_PACKAGES <- function(
  dir = ".", fields = NULL, type = c("source", "mac.binary", "win.binary"),
  addFiles = TRUE) {

  if (is.null(fields)) {
    fields <- unique(c(
      ("tools" %:::% ".get_standard_repository_db_fields")(),
      "MD5sum"
    ))
  }

  fields <- unique(c(fields, "File"))

  ## This is bad, but tools does it, so we just follow
  if (missing(type) && .Platform$OS.type == "windows") {
    type <- "win.binary"
  }
  type <- match.arg(type)

  db_file <- get_db_file(dir)

  ## Create DB if needed
  if (!file.exists(db_file)) create_db(db_file, fields = fields)

  ## Update DB
  update_db(dir, db_file, fields, type, addFiles)

  ## Write out new PACKAGES* files
  write_packages_files(dir, db_file)
}

#' Add R packages to the package database
#'
#' The files must exist in the directory.
#'
#' @param files Files to add, only the file names, without the path.
#'   You can use [base::basename()] to chop off the path.
#' @param dir Package directory.
#'
#' @family PACKAGES manipulation
#' @export

add_PACKAGES <- function(files, dir = ".") {

  full_files <- file.path(dir, files)
  check_existing_files(full_files)

  md5s <- md5sum(full_files)

  db_file <- get_db_file(dir)
  fields <- db_get_fields(db_file)
  pkgs <- parse_package_files(full_files, md5s, fields)
  with_db(db_file, {
    insert_packages(db, pkgs)
  })

  ## Write out new PACKAGES* files
  write_packages_files(dir, db_file)
}

#' Remove package from a package database
#'
#' The files will be first removed from the database, and then
#' from the directory.
#'
#' @param files Files to remove. They must still exist at the time this
#'   function is called.
#' @inheritParams add_PACKAGES
#'
#' @family PACKAGES manipulation
#' @export

remove_PACKAGES <- function(files, dir = ".") {

  full_files <- file.path(dir, files)
  check_existing_files(full_files)

  md5s <- md5sum(full_files)

  db_file <- get_db_file(dir)
  with_db(db_file, {
    sql <- "DELETE FROM packages WHERE MD5sum = ?md5"
    for (md5 in md5s) {
      dbGetQuery(db, sqlInterpolate(db, sql, md5 = md5))
    }
  })

  ## Remove files
  unlink(full_files)

  ## Write out new PACKAGES* files
  write_packages_files(dir, db_file)
}
