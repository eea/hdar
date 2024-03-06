library("testthat")
library("hdar")

test_that("Accept TaC", {
  client <- Client$new()
  expect_no_error(client$accept_tac())
})
