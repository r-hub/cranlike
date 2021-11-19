
context("Edge cases")

test_that("no packages", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  db_file <- get_db_file(dir)
  fields <- get_fields(NULL)
  create_db(dirname(db_file), db_file, fields = fields)

  df <- db_all_packages(db_file)
  expect_equal(get_packages_txt(df), "")
})

test_that("only invalid package files", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  cat("foobar", file = file.path(dir, "bad-package_1.0.0.tar.gz"))
  expect_warning(update_PACKAGES(dir), "Cannot extract valid DESCRIPTION")
  unlink(file.path(dir, "bad-package_1.0.0.tar.gz"))

  cat("foobar", file = file.path(dir, "bad-package_1.0.0.tgz"))
  expect_warning(
    update_PACKAGES(dir, type = "mac.binary"),
    "Cannot extract valid DESCRIPTION"
  )
  unlink(file.path(dir, "bad-package_1.0.0.tgz"))

  cat("foobar", file = file.path(dir, "bad-package_1.0.0.zip"))
  expect_warning(
    update_PACKAGES(dir, type = "win.binary"),
    "Cannot extract valid DESCRIPTION"
  )

  cat("foobar", file = file.path(dir, "bad-package_1.0.0.tar.gz"))
  cat("foobar", file = file.path(dir, "bad-package_1.0.0.tgz"))
  expect_warning(update_PACKAGES(dir), "Cannot extract valid DESCRIPTION")
})

test_that("valid and invalid package files", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  foo2 <- make_tmp_pkg(dir, "foobar2")
  foo  <- make_tmp_pkg(dir, "foobar")
  cat("foobar", file = file.path(dir, "bad-package_1.0.0.tar.gz"))
  expect_warning(update_PACKAGES(dir), "Cannot extract valid DESCRIPTION")

  db_file <- get_db_file(dir)
  fields <- get_fields(NULL)
  all_fields <- c(fields, extra_columns())
  tab <- db_all_packages(db_file)
  expect_equal(names(tab), all_fields)
  expect_equal(tab$Package, c("foobar", "foobar2"))
})

test_that("archive without DESCRIPTION", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir.create(pkgdir <- file.path(dir, "pkg"))
  dir.create(file.path(pkgdir, "R"))
  cat("print('hello world')", file = file.path(pkgdir, "R", "pkg.R"))

  dir.create(dir2 <- tempfile())
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)
  withr::with_dir(
    dir,
    tar(file.path(dir2, "pkg_1.0.0.tar.gz"), "pkg", compression = "gzip")
  )

  expect_warning(update_PACKAGES(dir2), "Cannot extract valid DESCRIPTION")

  db_file <- get_db_file(dir2)
  fields <- get_fields(NULL)
  tab <- db_all_packages(db_file)
  expect_equal(nrow(tab), 0)
})

test_that("invalid DESCRIPTION", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir.create(pkgdir <- file.path(dir, "pkg"))
  dir.create(file.path(pkgdir, "R"))
  cat("print('hello world')", file = file.path(pkgdir, "R", "pkg.R"))
  cat("invalid\n", file = file.path(pkgdir, "DESCRIPTION"))

  dir.create(dir2 <- tempfile())
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)
  withr::with_dir(
    dir,
    tar(file.path(dir2, "pkg_1.0.0.tar.gz"), "pkg", compression = "gzip")
  )

  expect_warning(update_PACKAGES(dir2), "Cannot extract valid DESCRIPTION")

  db_file <- get_db_file(dir2)
  fields <- get_fields(NULL)
  tab <- db_all_packages(db_file)
  expect_equal(nrow(tab), 0)
})

test_that("DESCRIPTION missing required fields", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir.create(pkgdir <- file.path(dir, "pkg"))
  dir.create(file.path(pkgdir, "R"))
  cat("print('hello world')", file = file.path(pkgdir, "R", "pkg.R"))
  desc::description$
    new("!new")$
    del("Version")$
    write(file.path(pkgdir, "DESCRIPTION"))

  dir.create(dir2 <- tempfile())
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)
  withr::with_dir(
    dir,
    tar(file.path(dir2, "pkg_1.0.0.tar.gz"), "pkg", compression = "gzip")
  )

  expect_warning(update_PACKAGES(dir2), "No version number")

  db_file <- get_db_file(dir2)
  fields <- get_fields(NULL)
  tab <- db_all_packages(db_file)
})
