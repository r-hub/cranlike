
#' @importFrom utils globalVariables

globalVariables("db")

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

#' @importFrom DBI dbWriteTable

create_db <- function(dir, db_file, fields, ...) {

  pkgs <- as.data.frame(get_old_packages(dir, ...))
  if (is.null(pkgs)) stop("Cannot use or create PACKAGES files")

  pkgs <- adjust_package_fields(pkgs, fields)

  with_db(db_file, {
    dbWriteTable(db, "packages", pkgs, overwrite = TRUE)
  })
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
    pkgs <- as.data.frame(c(as.list(pkgs), newcols))
  }

  if (length(remove <- setdiff(colnames(pkgs), fields))) {

  }

  pkgs[, fields]
}

#' @importFrom tools md5sum
#' @importFrom DBI sqlInterpolate

update_db <- function(dir, db_file, fields, type, ...) {

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
