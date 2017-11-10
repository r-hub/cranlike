
context("PACKAGES* files")

test_that("list_package_files", {

  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  files <- sort(normalizePath(c(
    make_tmp_pkg(dir, "foobar2"),
    make_tmp_pkg(dir, "foobar"),
    make_tmp_pkg(dir, "foobar3")
  )))

  file.copy(files, mac <- sub("\\.tar\\.gz$", ".tgz", files))
  file.copy(files, win <- sub("\\.tar\\.gz$", ".zip", files))

  expect_equal(sort(list_package_files(dir, "source")), files)
  expect_equal(sort(list_package_files(dir, "mac.binary")), mac)
  expect_equal(sort(list_package_files(dir, "win.binary")), win)
})

test_that("write_package_files", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  foo2 <- make_tmp_pkg(dir, "foobar2")
  foo  <- make_tmp_pkg(dir, "foobar")
  foo3 <- make_tmp_pkg(dir, "foobar3")

  db_file <- get_db_file(dir)
  fields <- get_fields(NULL)
  create_db(dir, db_file, fields)
  update_db(dir, db_file, fields, type = "source")

  write_packages_files(dir, db_file)
  expect_true(file.exists(file.path(dir, "PACKAGES")))
  expect_true(file.exists(file.path(dir, "PACKAGES.gz")))
  expect_true(file.exists(file.path(dir, "PACKAGES.rds")))

  tab <- db_all_packages(db_file)
  tab2 <- read.dcf(file.path(dir, "PACKAGES"))
  expect_equal(
    tab[, c("Package", "Version", "License", "MD5sum", "File")],
    as.data.frame(tab2, stringsAsFactors = FALSE)
  )

  tab3 <- read.dcf(gz <- gzfile(file.path(dir, "PACKAGES")))
  close(gz)
  expect_equal(tab2, tab3)

  tab4 <- readRDS(file.path(dir, "PACKAGES.rds"))
  expect_equal(
    tab2,
    tab4[, c("Package", "Version", "License", "MD5sum", "File")]
  )
})
