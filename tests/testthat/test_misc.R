library("testthat")
library("hdar")

test_that("Search - Results", {
  client <- Client$new()
  found_datasets <- client$datasets()

  print(length(found_datasets))

  IDs <- sapply(found_datasets,FUN = function(x){x$dataset_id})
  print(IDs)

#  expect_no_error(found_datasets)
#  expect_gte(length(found_datasets), 0)
})


