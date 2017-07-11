
#' Tools for CRAN-like Repositories
#'
#' A set of functions to manage CRAN-like repositories efficiently.
#'
#' @docType package
#' @name cranlike
NULL

#' Create an empty package database
#'
#' Create an empty package database if it does not exist.
#' It also updates the PACKAGES* files from the new (empty) database.
#'
#' @inheritParams tools::write_PACKAGES
#' @export

create_empty_PACKAGES <- function(dir = ".", fields = NULL) {

  "!DEBUG Creating empty package DB and PACKAGES* files"
  fields <- get_fields(fields)

  db_file <- get_db_file(dir)

  ## Create DB if needed
  if (!file.exists(db_file)) create_db(db_file, fields = fields)

  ## Write out new PACKAGES* files
  write_packages_files(dir, db_file)
}

#' Create or update PACKAGES* files for a CRAN-like repository
#'
#' This function is similar to [tools::write_PACKAGES()], with some
#' differences:
#' * It always uses the `MD5sum` field.
#' * It defaults to `addFiles = TRUE`, to allow following the package
#'   files better.
#' * It does not support the `verbose`, `unpacked`, and `subdirs`
#'   arguments currently.
#' * It uses a database to speed up the indexing process, and only
#'   reindexes files that have added, removed or updated.
#'
#' `update_PACKAGES` uses a SQLite database to aid updating PACKAGES*
#' files quickly. It this database does not exist, then it will be created
#' based on the existing PACKAGES* files. If no PACKAGES* files exist,
#' either, then these will be created via `tools::write_PACKAGES()`.
#'
#' @inheritParams create_empty_PACKAGES
#' @inheritParams tools::write_PACKAGES
#'
#' @family PACKAGES manipulation
#' @export

update_PACKAGES <- function(
  dir = ".", fields = NULL,
  type = c("source", "mac.binary", "win.binary")) {

  "!DEBUG Updating DB and PACKAGES* from directory content"
  fields <- get_fields(fields)

  type <- match.arg(type)

  db_file <- get_db_file(dir)

  ## Create DB if needed
  if (!file.exists(db_file)) create_db(db_file, fields = fields)

  ## Update DB
  update_db(dir, db_file, fields, type)

  ## Write out new PACKAGES* files
  write_packages_files(dir, db_file)
}

#' Add R packages to the package database
#'
#' The files must exist in the directory. If the package database does
#' not exist, then it will be created.
#'
#' @param files Files to add, only the file names, without the path.
#'   You can use [base::basename()] to chop off the path.
#' @param dir Package directory.
#' @param fields Fields to use in the database if the database is
#'   created.
#'
#' @family PACKAGES manipulation
#' @export

add_PACKAGES <- function(files, dir = ".", fields = NULL) {

  "!DEBUG Adding `length(files)` packages"

  full_files <- file.path(dir, files)
  check_existing_files(full_files)

  md5s <- md5sum(full_files)

  db_file <- get_db_file(dir)
  fields <- get_fields(fields)
  if (!file.exists(db_file)) create_db(db_file, fields = fields)

  pkgs <- parse_package_files(full_files, md5s, fields)
  sql <- "DELETE FROM packages WHERE file = ?file"
  with_db(db_file, {
    for (file in full_files) {
      dbExecute(db, sqlInterpolate(db, sql, file = basename(file)))
    }
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

  "!DEBUG Removing `length(files)` packages"

  full_files <- file.path(dir, files)
  check_existing_files(full_files)

  md5s <- md5sum(full_files)

  db_file <- get_db_file(dir)
  with_db(db_file, {
    sql <- "DELETE FROM packages WHERE MD5sum = ?md5"
    for (md5 in md5s) {
      "!DEBUG Removing `md5`"
      dbExecute(db, sqlInterpolate(db, sql, md5 = md5))
    }
  })

  ## Remove files
  unlink(full_files)

  ## Write out new PACKAGES* files
  write_packages_files(dir, db_file)
}
