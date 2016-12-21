
context("DB")

test_that("with_db", {
  expect_equal(
    with_db(":memory:", c(class(db))),
    "SQLiteConnection"
  )

  xxx <- NULL
  expect_null(with_db(":memory:", xxx))
})

test_that("db_all_packages", {
  db_file <- tempfile()
  on.exit(unlink(db_file))
  with_db(db_file, {
    dbGetQuery(db, "CREATE TABLE packages (Package TEXT, Version TEXT)")
    dbGetQuery(db, "INSERT INTO packages VALUES ('igraph', '1.0.0')")
    dbGetQuery(db, "INSERT INTO packages VALUES ('wegraph', '1.0.1')")
  })
  expect_equal(
    db_all_packages(db_file),
    data.frame(
      stringsAsFactors = FALSE,
      Package = c("igraph", "wegraph"),
      Version = c("1.0.0", "1.0.1")
    )
  )
})

test_that("db_get_fields", {
  db_file <- tempfile()
  on.exit(unlink(db_file))
  with_db(db_file, {
    dbGetQuery(db, "CREATE TABLE packages (Package TEXT, Version TEXT)")
    dbGetQuery(db, "INSERT INTO packages VALUES ('igraph', '1.0.0')")
    dbGetQuery(db, "INSERT INTO packages VALUES ('wegraph', '1.0.1')")
  })
  expect_equal(
    db_get_fields(db_file),
    c("Package", "Version")
  )
})

test_that("create_db", {
  create_db(db_file <- tempfile(), fields = c("foo", "bar", "MD5sum"))
  expect_silent(
    with_db(db_file, dbGetQuery(db, "SELECT * from packages"))
  )
})

test_that("db_create_text_table", {
  res <- NULL
  mockery::stub(db_create_text_table, 'dbGetQuery', function(x, y) res <<- y)
  db_create_text_table(NULL, "table", c("a", "b"), "b")
  expect_match(res, paste0(
    'CREATE TABLE table[(]\\s*"a" TEXT,\\s*"b" TEXT,',
    '\\s*PRIMARY KEY [(]"b")\\s*);'
  ))
})

test_that("adjust_package_fields", {

  df <- data.frame(
    stringsAsFactors = FALSE,
    col1 = c("a", "b"),
    col2 = c("x", "y"),
    col3 = c("z", "z")
  )

  expect_equal(adjust_package_fields(df, names(df)), df)

  expected <- data.frame(
    stringsAsFactors = FALSE,
    col1 = c("a", "b"),
    col3 = c("z", "z"),
    col4 = c(NA_character_, NA_character_),
    col5 = c(NA_character_, NA_character_)
  )

  expect_equal(
    adjust_package_fields(df, c("col1", "col3", "col4", "col5")),
    expected
  )
})

test_that("update_db", {
  dir.create(dir <- tempfile())
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  foo2 <- make_tmp_pkg(dir, "foobar2")
  foo  <- make_tmp_pkg(dir, "foobar")
  foo3 <- make_tmp_pkg(dir, "foobar3")

  db_file <- get_db_file(dir)
  fields <- get_fields(NULL)
  create_db(db_file, fields)
  update_db(dir, db_file, fields, type = "source")

  tab <- db_all_packages(db_file)
  expect_equal(names(tab), fields)
  expect_equal(tab$Package, c("foobar", "foobar2", "foobar3"))
  expect_equal(tab$File, basename(c(foo, foo2, foo3)))

  ## This file was removed
  unlink(file.path(dir, "foobar3_1.0.0.tar.gz"))
  update_db(dir, db_file, fields, type = "source")

  tab <- db_all_packages(db_file)
  expect_equal(names(tab), fields)
  expect_equal(tab$Package, c("foobar", "foobar2"))
  expect_equal(tab$File, basename(c(foo, foo2)))
})
