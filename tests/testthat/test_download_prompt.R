library("testthat")
library("hdar")

test_that("small downloads do not prompt", {
  client <- Client$new("user", "password")
  sr <- SearchResults$new(client, list(), "dummy")

  expect_true(sr$.__enclos_env__$private$prompt_user_confirmation(1))
})

test_that("large downloads proceed without prompting in non-interactive sessions", {
  skip_if(interactive(), "requires a non-interactive R session")

  client <- Client$new("user", "password")
  sr <- SearchResults$new(client, list(), "dummy")

  # 200 MB > LARGE_DOWNLOAD_SIZE (100 MB); must not error without stdin.
  expect_true(sr$.__enclos_env__$private$prompt_user_confirmation(200 * 1024^2))
})
