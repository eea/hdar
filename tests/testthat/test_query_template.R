library("testthat")
library("hdar")
library("httr2")
library("jsonlite")

check_if_placeholders_exist <- function(query_template) {
  template <- fromJSON(query_template)
  for (param in names(template))
  {
    value <- template[[param]]
    if (is_placeholder(value)) {
      return(TRUE)
    }
  }
  FALSE
}

test_that("Template - Generate Query Template", {
  client <- Client$new()

  query_template <- client$generate_query_template("EO:EEA:DAT:CLMS_HRVPP_ST")
  # print(query_template)

  usable_query_template <- strip_off_template_placeholders(query_template)
  # print(usable_query_template)

  expect_true(check_if_placeholders_exist(query_template))
  expect_false(check_if_placeholders_exist(usable_query_template))
})
