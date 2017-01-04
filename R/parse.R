
#' @importFrom desc description

parse_package_files <- function(files, md5s, fields) {

  ## We work in temp dir
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ## Extract and parse DESCRIPTION. If there is a warning, then
  ## this is probably because uncompressing the file has failed.
  ## If there is an error, then we could not extract DESCRIPTION,
  ## but even in this case there will be a warning as well...
  pkgs <- lapply(files, function(file) {
    "!DEBUG Parsing `basename(file)`"
    desc_file <- get_desc_file(file, exdir = tmp)
    if (is.null(desc_file)) return(NULL)
    desc <- tryCatch(
      desc <- description$new(desc_file),
      error = function(e) {
        warning("Invalid DESCRIPTION file")
        NULL
      }
    )
    if (is.null(desc)) return(NULL)
    row <- desc$get(fields)
    if (is.na(row["Package"])) warning("No package name in ", sQuote(file))
    if (is.na(row["Version"])) warning("No version number in ", sQuote(file))
    row
  })
  valid <- ! vapply(pkgs, is.null, TRUE)

  ## Make it into a DF
  pkgs <- drop_nulls(pkgs)
  pkgs <- vapply(pkgs, c, FUN.VALUE = fields)
  df <- as.data.frame(t(pkgs))
  names(df) <- fields

  ## Stick in MD5
  df$MD5sum <- md5s[valid]

  ## Add file names
  df$File <- basename(files[valid])

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
  tryCatch(
    {
      uncompress(file, files = paste0(pkg, "/DESCRIPTION"), exdir = exdir)
      file.path(exdir, pkg, "DESCRIPTION")
    },
    error = function(e) {
      warning(
        "Cannot extract valid DESCRIPTION, ", sQuote(file),
        " will be ignored ",
        conditionMessage(e)
      )
      NULL
    },
    warning = function(e) {
      warning(
        "Cannot extract valid DESCRIPTION, ", sQuote(file),
        " will be ignored ",
        conditionMessage(e)
        )
      NULL
    }
  )
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
