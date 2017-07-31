
#' List all packages and versions in a CRAN-like repository
#'
#' @param dir Path to the repository.
#' @param xcolumns Extra columns to include in the result.
#' @return Data frame with at least three columns:
#'   `Package`, `Version`, `MD5sum`.
#'
#' @export

package_versions <- function(dir = ".", xcolumns = character()) {
  db_file <- get_db_file(dir)

  if (!file.exists(db_file)) stop("Database does not exist")

  cols <- unique(c("Package", "Version", "MD5Sum", xcolumns))

  with_db(db_file, {
    q <- paste0("SELECT ", paste(cols, collapse = ","), " FROM packages")
    dbGetQuery(db, q)
  })
}
