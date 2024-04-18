library("testthat")
library("hdar")
library("httr2")
library("jsonlite")


test_that("Template - Generate Query Template", {
  client <- Client$new()

  query_template <- client$generate_query_template("EO:CRYO:DAT:HRSI:FSC")
  print(query_template)

})

