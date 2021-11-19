
test_that("extra columns", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  foo2 <- make_tmp_pkg(dir, "foobar2")
  foo  <- make_tmp_pkg(dir, "foobar")
  foo3 <- make_tmp_pkg(dir, "foobar3")

  db_file <- get_db_file(dir)
  fields <- get_fields(NULL)
  update_PACKAGES(dir, xcolumns = list(Platform = "myplatform"))

  all_fields <- c(fields, extra_columns(), "Platform")
  tab <- db_all_packages(db_file)
  expect_equal(names(tab), all_fields)
  expect_equal(tab$Package, c("foobar", "foobar2", "foobar3"))
  expect_equal(tab$File, basename(c(foo, foo2, foo3)))
  expect_equal(tab$Platform, rep("myplatform", 3))

  foo4 <- make_tmp_pkg(dir, "foobar4")
  add_PACKAGES(basename(foo4), dir, xcolumns = list(Platform = "another"))
  tab <- db_all_packages(db_file)
  expect_equal(tab$Package, c("foobar", "foobar2", "foobar3", "foobar4"))
  expect_equal(tab$File, basename(c(foo, foo2, foo3, foo4)))
  expect_equal(tab$Platform, c(rep("myplatform", 3), "another"))
})
