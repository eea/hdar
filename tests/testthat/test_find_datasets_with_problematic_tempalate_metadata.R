library("testthat")
library("hdar")
library("httr2")
library("jsonlite")


test_that("Bug Hunter - find dataset with incorrect default query template", {
  client <- Client$new()

  found_datasets <- client$datasets()

  sink(file="~/failed_datasets_result01.txt")

  i <- 1
  for (dataset in found_datasets) {

    dataset_id <- dataset[["dataset_id"]]
    tryCatch(
      {
        #print(paste("Trying to build template for", dataset_id))
        query_template <- client$generate_query_template(dataset_id)
        #print(query_template)
        query = jsonlite::fromJSON(query_template)
        matches <- client$search(query,1)
      }, error = function(err) {
        #print(paste("Problem: ", err))
        print(dataset_id)
      }
    )

    Sys.sleep(1)
    i <- i + 1
    if (i %% 20 == 0) {
      Sys.sleep(9)
    }
  }

  sink()

})

