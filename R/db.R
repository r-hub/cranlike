
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

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite

with_db <- function(db_file, expr) {
  on.exit(dbDisconnect(con))
  con <- dbConnect(SQLite(), db_file)
  eval(substitute(expr), envir = list(db = con), enclos = parent.frame())
}

#' @importFrom DBI dbGetQuery

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

create_db <- function(db_file, fields) {
  "!DEBUG Creating DB in `basename(db_file)`"
  dir.create(dirname(db_file), showWarnings = FALSE, recursive = TRUE)
  with_db(db_file, {
    db_create_text_table(db, "packages", fields, key = "MD5sum")
  })
}

db_create_text_table <- function(db, name, columns, key) {
  sql <- paste0(
    "CREATE TABLE ", name, "(\n",
    paste0('  "', columns, '" ', "TEXT", collapse = ",\n"),
    if (!is.null(key)) paste0(',\n  PRIMARY KEY ("', key, '")\n'),
    ");"
  )
  dbGetQuery(db, sql)
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

  with_db(db_file, {

    do_all <- function() {
      dbGetQuery(db, "BEGIN EXCLUSIVE")
      on.exit(try(dbGetQuery(db, "ROLLBACK"), silent = TRUE))

      ## Packages in the DB
      db_md5 <- dbGetQuery(db, "SELECT MD5sum FROM packages")$MD5sum

      ## Files removed?
      if (length(removed <- setdiff(db_md5, dir_md5)) > 0) {
        sql <- "DELETE FROM packages WHERE MD5sum = ?md5sum"
        for (rem in removed) {
          "!DEBUG Removing `rem`"
          dbGetQuery(db, sqlInterpolate(db, sql, md5sum = rem))
        }
      }

      ## Any files added?
      if (length(added <- setdiff(dir_md5, db_md5)) > 0) {
        added_files <- names(dir_md5)[match(added, dir_md5)]
        pkgs <- parse_package_files(added_files, added, fields)
        insert_packages(db, pkgs)
      }

      ## All good
      dbGetQuery(db, "COMMIT")
      on.exit(NULL)
    }

    do_all()
  })
}

insert_packages <- function(db, pkgs) {
  dbWriteTable(db, "packages", pkgs, append = TRUE)
}
