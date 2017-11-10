
#' @importFrom utils globalVariables

globalVariables("db")

get_db_file <- function(dir) {
  file.path(dir, "PACKAGES.db")
}

get_fields <- function(fields) {
  if (is.null(fields)) {
    fields <- unique(c(
      ("tools" %:::% ".get_standard_repository_db_fields")(),
      "MD5sum"
    ))
  }

  unique(c(fields, "File"))
}

db_env <- new.env()

#' Perform a DB query, without explicit locking
#'
#' This is for read operations. They can also be called from
#' within a transaction. In this case the database handle will
#' be reused.
#'
#' @param db_file File of the DB.
#' @param expr Expression to evaluate, it can refer to the connection
#'   handle as `db`.
#'
#' @importFrom DBI dbConnect dbDisconnect dbWithTransaction dbIsValid
#' @importFrom RSQLite SQLite
#'
#' @keywords internal

with_db <- function(db_file, expr) {
  con <- db_env$con
  if (is.null(con) || ! dbIsValid(con)) {
    con <- dbConnect(SQLite(), db_file, synchronous = NULL)
    dbExecute(con, "PRAGMA busy_timeout = 60000")
    on.exit(dbDisconnect(con), add = TRUE)
  }
  eval(substitute(expr), envir = list(db = con), enclos = parent.frame())
}

#' Perform a DB query, with locking
#'
#' This creates a transaction, and an exclusive lock.
#' It always creates a new DB connection, and closes it on exit.
#'
#' @inheritParams with_db
#'
#' @keywords internal

with_db_lock <- function(db_file, expr) {
  on.exit(dbDisconnect(con), add = TRUE)
  if (is.null(db_env$con) || ! dbIsValid(db_env$con)) {
    db_env$con <- dbConnect(SQLite(), db_file, synchronous = NULL)
  }
  con <- db_env$con
  pnt <- parent.frame()
  dbExecute(con, "PRAGMA busy_timeout = 60000")
  dbExecute(con, "BEGIN EXCLUSIVE")
  withCallingHandlers(
    {
      res <- eval(substitute(expr), envir = list(db = con), enclos = pnt)
      dbExecute(con, "COMMIT")
      res
    },
    error = function(e) dbExecute(con, "ROLLBACK")
  )
}

#' @importFrom DBI dbGetQuery dbExecute

db_all_packages <- function(db_file) {
  with_db(db_file, {
    dbGetQuery(db, "SELECT * FROM packages ORDER BY Package, Version")
  })
}

db_get_fields <- function(db_file) {
  with_db(db_file, {
    names(dbGetQuery(db, "SELECT * FROM PACKAGES LIMIT 1"))
  })
}

#' @importFrom DBI dbWriteTable

create_db <- function(dir, db_file, fields) {
  "!DEBUG Creating DB in `basename(db_file)`"
  dir.create(dirname(db_file), showWarnings = FALSE, recursive = TRUE)
  with_db_lock(db_file, {
    db_create_text_table(db, "packages", fields, key = "MD5sum")
    write_packages_files(dir, db_file)
  })
}

db_create_text_table <- function(db, name, columns, key) {
  sql <- paste0(
    "CREATE TABLE ", name, "(\n",
    paste0('  "', columns, '" ', "TEXT", collapse = ",\n"),
    if (!is.null(key)) paste0(',\n  PRIMARY KEY ("', key, '")\n'),
    ");"
  )
  dbExecute(db, sql)
}

adjust_package_fields <- function(pkgs, fields) {
  if (length(add <- setdiff(fields, colnames(pkgs)))) {
    newcols <- structure(
      replicate(
        length(add),
        rep(NA_character_, nrow(pkgs)),
        simplify = FALSE
      ),
      names = add
    )
    pkgs <- as.data.frame(
      c(as.list(pkgs), newcols),
      stringsAsFactors = FALSE
    )
  }

  pkgs[, fields]
}

#' @importFrom tools md5sum
#' @importFrom DBI sqlInterpolate

update_db <- function(dir, db_file, fields, type) {

  "!DEBUG Updating DB in `basename(db_file)`"

  ## Current packages
  files <- list_package_files(dir, type)
  dir_md5 <- md5sum(files)

  with_db_lock(db_file, {

    ## Packages in the DB
    db_md5 <- dbGetQuery(db, "SELECT MD5sum FROM packages")$MD5sum

    ## Files removed?
    if (length(removed <- setdiff(db_md5, dir_md5)) > 0) {
      sql <- "DELETE FROM packages WHERE MD5sum = ?md5sum"
      for (rem in removed) {
        "!DEBUG Removing `rem`"
        dbExecute(db, sqlInterpolate(db, sql, md5sum = rem))
      }
    }

    ## Any files added?
    if (length(added <- setdiff(dir_md5, db_md5)) > 0) {
      added_files <- names(dir_md5)[match(added, dir_md5)]
      pkgs <- parse_package_files(added_files, added, fields)
      insert_packages(db, pkgs)
    }

    write_packages_files(dir, db_file)
  })
}

insert_packages <- function(db, pkgs) {
  dbWriteTable(db, "packages", pkgs, append = TRUE)
}
