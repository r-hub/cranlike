
context("parse")

test_that("choose_uncompress_function", {
  expect_output(
    print(choose_uncompress_function("foo_1.0.0.tar.gz")),
    "untar"
  )
  expect_output(
    print(choose_uncompress_function("foo_1.0.0.tgz")),
    "untar"
  )
  expect_output(
    print(choose_uncompress_function("foo_1.0.0.zip")),
    "unzip"
  )
  expect_error(
    choose_uncompress_function("foo"),
    "know how to handle"
  )
})
