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

  found_datasets <- client$datasets("zzzqwerty123456789")
  expect_no_error(found_datasets)
  expect_length(found_datasets, 0)
})

test_that("Search Dataset by Name - Results", {
  client <- Client$new()

  found_datasets <- client$datasets("EO:EEA:DAT:CLMS_HRVPP_ST")
  expect_no_error(found_datasets)
  expect_gt(length(found_datasets), 0)
})
