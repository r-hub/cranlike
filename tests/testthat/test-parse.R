
context("parse")

test_that("choose_uncompress_function", {
  expect_equal(
    choose_uncompress_function("foo_1.0.0.tar.gz"),
    utils::untar
  )
  expect_equal(
    choose_uncompress_function("foo_1.0.0.tgz"),
    utils::untar
  )
  expect_equal(
    choose_uncompress_function("foo_1.0.0.zip"),
    utils::unzip
  )
  expect_error(
    choose_uncompress_function("foo"),
    "know how to handle"
  )
})
