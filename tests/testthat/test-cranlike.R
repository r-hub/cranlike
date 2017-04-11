
context("update")

test_that("create", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  create_empty_PACKAGES(dir)

  expect_equal(
    package_versions(dir),
    data.frame(
      stringsAsFactors = FALSE,
      Package = character(),
      Version = character(),
      MD5sum = character()
    )
  )
})

test_that("update", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  foo2 <- make_tmp_pkg(dir, "foobar2")
  foo  <- make_tmp_pkg(dir, "foobar")
  foo3 <- make_tmp_pkg(dir, "foobar3")

  db_file <- get_db_file(dir)
  fields <- get_fields(NULL)
  create_db(db_file, fields)
  update_PACKAGES(dir)

  tab <- db_all_packages(db_file)
  expect_equal(names(tab), fields)
  expect_equal(tab$Package, c("foobar", "foobar2", "foobar3"))
  expect_equal(tab$File, basename(c(foo, foo2, foo3)))
})

test_that("update, zip files", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  foo2 <- make_tmp_pkg(dir, "foobar2", type = "win.binary")
  foo  <- make_tmp_pkg(dir, "foobar", type = "win.binary")
  foo3 <- make_tmp_pkg(dir, "foobar3", type = "win.binary")

  db_file <- get_db_file(dir)
  fields <- get_fields(NULL)
  create_db(db_file, fields)
  update_PACKAGES(dir, type = "win.binary")

  tab <- db_all_packages(db_file)
  expect_equal(names(tab), fields)
  expect_equal(tab$Package, c("foobar", "foobar2", "foobar3"))
  expect_equal(tab$File, basename(c(foo, foo2, foo3)))
})

context("add")

test_that("add", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  foo2 <- make_tmp_pkg(dir, "foobar2")

  db_file <- get_db_file(dir)
  fields <- get_fields(NULL)
  create_db(db_file, fields)
  update_db(dir, db_file, fields, type = "source")
  tab <- db_all_packages(db_file)
  expect_equal(names(tab), fields)
  expect_equal(tab$Package, c("foobar2"))
  expect_equal(tab$File, basename(c(foo2)))

  foo  <- make_tmp_pkg(dir, "foobar")
  add_PACKAGES(basename(foo), dir)

  tab <- db_all_packages(db_file)
  expect_equal(names(tab), fields)
  expect_equal(tab$Package, c("foobar", "foobar2"))
  expect_equal(tab$File, basename(c(foo, foo2)))

  foo3 <- make_tmp_pkg(dir, "foobar3")
  add_PACKAGES(basename(foo3), dir)

  tab <- db_all_packages(db_file)
  expect_equal(names(tab), fields)
  expect_equal(tab$Package, c("foobar", "foobar2", "foobar3"))
  expect_equal(tab$File, basename(c(foo, foo2, foo3)))
})

test_that("add, create DB on demand", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  foo2 <- make_tmp_pkg(dir, "foobar2")
  foo  <- make_tmp_pkg(dir, "foobar")
  foo3 <- make_tmp_pkg(dir, "foobar3")

  add_PACKAGES(basename(foo2), dir)
  add_PACKAGES(basename(foo), dir)
  add_PACKAGES(basename(foo3), dir)

  db_file <- get_db_file(dir)
  tab <- db_all_packages(db_file)
  expect_equal(names(tab), get_fields(NULL))
  expect_equal(tab$Package, c("foobar", "foobar2", "foobar3"))
  expect_equal(tab$File, basename(c(foo, foo2, foo3)))
})

context("remove")

test_that("remove", {

  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  foo2 <- make_tmp_pkg(dir, "foobar2")
  foo  <- make_tmp_pkg(dir, "foobar")
  foo3 <- make_tmp_pkg(dir, "foobar3")

  db_file <- get_db_file(dir)
  fields <- get_fields(NULL)
  create_db(db_file, fields)
  update_PACKAGES(dir)

  tab <- db_all_packages(db_file)
  expect_equal(names(tab), fields)
  expect_equal(tab$Package, c("foobar", "foobar2", "foobar3"))
  expect_equal(tab$File, basename(c(foo, foo2, foo3)))

  remove_PACKAGES(basename(foo3), dir)

  tab <- db_all_packages(db_file)
  expect_equal(names(tab), fields)
  expect_equal(tab$Package, c("foobar", "foobar2"))
  expect_equal(tab$File, basename(c(foo, foo2)))
})
