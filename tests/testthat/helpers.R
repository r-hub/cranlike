
make_tmp_pkg <- function(dir, name, type = c("source", "win.binary")) {

  type <- match.arg(type)

  ## Package root directory
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp, name))

  ## DESCRIPTION
  desc <- desc::description$new("!new")
  desc$set(
    Package = name,
    Version = "1.0.0",
    Title = "Foobar Package is the Best",
    Maintainer = "Jo Doe <jodoe@dom.ain>",
    Description = "I need a package for testing. And this section must
      also contain some complete sentences.",
    License = "GPL (>=2)",
    URL = "https://github.com/r-hub/cranlike",
    BugReports = "https://github.com/r-hub/cranlike/issues"
  )
  desc$write(file.path(tmp, name, "DESCRIPTION"))

  ## NAMESPACE
  cat("# nothing here, really\n", file = file.path(tmp, name, "NAMESPACE"))

  ## tar it up, we need to create this file, otherwise normalizePAth
  ## does not work :/
  pkgfile <- file.path(
    dir,
    paste0(name, if (type == "source") "_1.0.0.tar.gz" else "_1.0.0.zip")
  )
  cat("", file = pkgfile)
  pkgfile <- normalizePath(pkgfile)

  withr::with_dir(tmp, {
    unlink(pkgfile)
    if (type == "source") {
      utils::tar(pkgfile, name, compression = "gzip", tar = "internal")
    } else {
      zip::zip(pkgfile, name)
    }
  })

  pkgfile
}
