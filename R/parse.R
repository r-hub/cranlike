
#' @importFrom desc description

parse_package_files <- function(files, md5s, fields) {

  ## We work in temp dir
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ## Extract and parse DESCRIPTION
  pkgs <- vapply(seq_along(files), FUN.VALUE = fields, function(i) {
    file <- files[i]
    "!DEBUG Parsing `basename(file)`"
    desc_file <- get_desc_file(files[i], exdir = tmp)
    desc <- description$new(desc_file)
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

get_desc_file <- function(file, exdir) {
  pkg <- pkgname_from_filename(file)
  uncompress <- choose_uncompress_function(file)
  uncompress(file, files = paste0(pkg, "/DESCRIPTION"), exdir = exdir)
  file.path(exdir, pkg, "DESCRIPTION")
}

#' @importFrom utils untar unzip

choose_uncompress_function <- function(file) {
  if (grepl("_.*\\.zip$", file)) {
    unzip

  } else if (grepl("_.*\\.tar\\..*$", file) || grepl("_.*\\.tgz$", file)) {
    untar

  } else {
    stop("Don't know how to handle file: ", file)
  }
}
