
context("utils")

test_that("format_iso_8601", {

  expect_identical(
    format_iso_8601(as.POSIXct("2012-06-30 13:58:23", tz = "GMT")),
    "2012-06-30T13:58:23+00:00"
  )
})

test_that("pkgname_from_filename", {

  expect_identical(
    pkgname_from_filename(character()),
    character()
  )

  expect_identical(
    pkgname_from_filename("foo/bar/pkg_1.0.0.tar.gz"),
    "pkg"
  )
  
  expect_identical(
    pkgname_from_filename(c("foo/bar/pkg_1.0.0.tar.gz", "x/y/pkg_2.0.tgz")),
    c("pkg", "pkg")
  )
})

test_that("%:::%", {

  expect_equal(
    "cranlike" %:::% "update_PACKAGES",
    update_PACKAGES
  )
})

test_that("check_existing_files", {

  expect_silent(check_existing_files(character()))

  expect_silent(check_existing_files(tempdir()))

  cat("foo\n", file = tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)
  expect_silent(check_existing_files(tmp))
  expect_silent(check_existing_files(c(tmp, tmp)))

  expect_error(
    check_existing_files(tempfile()),
    "File does not exist"
  )

  expect_error(
    check_existing_files(c(tmp, tempfile(), tempdir())),
    "File does not exist"
  )

  expect_error(
    check_existing_files(c(tempfile(), tempfile(), tempdir())),
    "Files do not exist"
  )
})
