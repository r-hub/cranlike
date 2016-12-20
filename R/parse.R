
#' @importFrom desc description
#' @importFrom utils untar

parse_package_files <- function(files, md5s, fields) {

  ## We work in temp dir
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ## Extract and parse DESCRIPTION
  pkgs <- vapply(seq_along(files), FUN.VALUE = fields, function(i) {
    file <- files[i]
    pkg <- pkgname_from_filename(file)
    untar(file, files = paste0(pkg, "/DESCRIPTION"), exdir = tmp)
    desc <- description$new(file.path(tmp, pkg, "DESCRIPTION"))
    desc$get(fields)
  })

  ## Stick in MD5
  df <- as.data.frame(t(pkgs))
  df$MD5sum <- md5s

  ## Add file names
  df$File <- basename(files)

  ## Standardize licenses, or NA, like in tools
  license_info <- analyze_licenses(df$License)
  df$License <- ifelse(license_info$is_standardizable,
                       license_info$standardization,
                       NA_character_)

  df
}
