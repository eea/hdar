library("testthat")
library("hdar")
library("jsonlite")

# Queryables served for CDSE datasets can contain array parameters whose
# `items` have neither `oneOf` nor `enum`, and scalar parameters typed as
# "integer"/"boolean". These used to crash template generation, so cover the
# metadata extraction directly without hitting the network.

test_that("array without oneOf or enum yields a placeholder", {
  meta <- list(type = "array", items = list(type = "string"))

  expect_no_error(result <- extract_param_metadata(meta))
  expect_s3_class(result, "data.frame")
  expect_true(is_placeholder(result$value))
  expect_true(is.na(result$comment))
})

test_that("array without items yields a placeholder", {
  meta <- list(type = "array")

  expect_no_error(result <- extract_param_metadata(meta))
  expect_true(is_placeholder(result$value))
})

test_that("array oneOf and enum still yield their values", {
  one_of <- extract_param_metadata(list(
    type = "array",
    items = list(oneOf = list(list(const = "a"), list(const = "b")))
  ))
  expect_equal(unlist(one_of$value), c("a", "b"))

  enum <- extract_param_metadata(list(
    type = "array",
    items = list(type = "string", enum = list("x", "y"))
  ))
  expect_equal(unlist(enum$value), c("x", "y"))
})

test_that("integer, boolean and unknown types return a data frame", {
  int <- extract_param_metadata(list(type = "integer", default = 30L, minimum = 0L, maximum = 100L))
  expect_equal(int$value, 30L)
  expect_false(is.na(int$comment))

  bool <- extract_param_metadata(list(type = "boolean", default = TRUE))
  expect_equal(bool$value, TRUE)
  expect_false(is.na(bool$comment))

  unknown <- extract_param_metadata(list(type = "object"))
  expect_true(is_placeholder(unknown$value))

  # A missing type previously made switch() error.
  expect_no_error(missing_type <- extract_param_metadata(list(default = "v")))
  expect_equal(missing_type$value, "v")
})

test_that("template generation survives CDSE-style metadata end to end", {
  client <- Client$new("user", "password")
  map <- client$.__enclos_env__$private$map_metadata_to_query_template

  properties <- list(
    dataset_id = list(type = "string"),
    itemsPerPage = list(type = "number"),
    startIndex = list(type = "number"),
    productType = list(type = "array", items = list(type = "string")),
    cloudCover = list(type = "integer", default = 30L, minimum = 0L, maximum = 100L),
    latest = list(type = "boolean", default = TRUE)
  )

  expect_no_error(template <- map(list(properties = properties), "EO:CLMS:DAT:GRA_10M_YEARLY_2017_PRESENT"))

  parsed <- fromJSON(template)
  expect_equal(parsed$dataset_id, "EO:CLMS:DAT:GRA_10M_YEARLY_2017_PRESENT")
  expect_equal(parsed$cloudCover, 30L)
  expect_true(parsed$latest)

  # The unfilled array placeholder is dropped from the usable query.
  expect_false(grepl("__###", strip_off_template_placeholders(template), fixed = TRUE))
})
