
context("package_versions")

test_that("package_vesions", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  foo2 <- make_tmp_pkg(dir, "foobar2")
  foo  <- make_tmp_pkg(dir, "foobar")
  foo3 <- make_tmp_pkg(dir, "foobar3")

  update_PACKAGES(dir)

  tab <- package_versions(dir)
  expect_equal(
    colnames(tab),
    c("Package", "Version", "MD5sum")
  )
  expect_equal(
    sort(tab$Package),
    sort(c("foobar", "foobar2", "foobar3"))
  )
})
