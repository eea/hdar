library("testthat")
library("hdar")

TOKEN_REGEXP = "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"


test_that("Auth: incorrect credentials", {
  client <- Client$new("wrong_user", "wrong_pwd")
  expect_error(client$get_token())
  expect_equal(client$token(), NULL)
})

#test_that("Auth: correct credentials", {
#  client <- Client$new(USER, PWD)
#  expect_no_error(client$get_token())
#  expect_match(client$token(), TOKEN_REGEXP)
#})

test_that("Auth: read credentials from file", {
  client <- Client$new()
  expect_no_error(client$get_token())
  expect_match(client$token(), TOKEN_REGEXP)
})
