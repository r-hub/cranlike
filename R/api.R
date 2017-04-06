
#' List all packages and versions in a CRAN-like repository
#'
#' @param dir Path to the repository.
#' @return Data frame with three columns: `Package`, `Version`, `MD5sum`.
#'
#' @export

package_versions <- function(dir = ".") {
  db_file <- get_db_file(dir)

  if (!file.exists(db_file)) stop("Database does not exist")

  with_db(db_file, {
    dbGetQuery(db, "SELECT Package, Version, MD5sum FROM packages")
  })
}
