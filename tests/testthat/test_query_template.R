library("testthat")
library("hdar")
library("httr2")
library("jsonlite")


test_that("Template - Generate Query Template", {
  client <- Client$new()

  query_template <- client$generate_query_template("EO:CRYO:DAT:HRSI:FSC")
  print(query_template)

#  query_template <- client$generate_query_template("EO:EEA:DAT:CLMS_HRVPP_VPP")
#  cat("\n\n")
#  print(query_template)

})

