library("testthat")
library("hdar")

test_that("Search - Results", {
  client <- Client$new()
  found_datasets <- client$datasets()
  # print(found_datasets)

  expect_no_error(found_datasets)
  expect_gte(length(found_datasets), 0)
})

test_that("Search Dataset by Name - No Results", {
  client <- Client$new()

  found_datasets <- client$datasets("non existing name 123456")
  expect_no_error(found_datasets)
  expect_length(found_datasets, 0)
})

test_that("Search Dataset by Name - Results", {
  client <- Client$new()

  found_datasets <- client$datasets("EO:CRYO:DAT:HRSI:FSC")
  expect_no_error(found_datasets)
  expect_gt(length(found_datasets), 0)
})
