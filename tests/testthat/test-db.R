
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
  
})

test_that("db_create_text_table", {

})

test_that("adjust_package_fields", {

})

test_that("update_db", {

})
