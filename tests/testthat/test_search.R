library("testthat")
library("hdar")

QUERY_CORRECT <- jsonlite::fromJSON('{
  "dataset_id": "EO:CRYO:DAT:HRSI:FSC",
  "observed_start": "2021-01-01T00:00:00.000Z"
}')

QUERY_CORRECT2 <- jsonlite::fromJSON('{
  "dataset_id": "EO:ECMWF:DAT:CAMS_GLOBAL_EMISSION_INVENTORIES",
  "variable": [
    "acetaldehyde"
  ],
  "source": [
    "anthropogenic"
  ],
  "version": [
    "latest"
  ],
  "year": [
    "2000"
  ],
  "format": "zip"
}')

QUERY_FAILED <- jsonlite::fromJSON('{
  "dataset_id": "EO:EEA:DAT:CLMS_HRVPP_VPP",
  "boundingBoxValues": [
    {
      "name": "bbox",
      "bbox": [
        9.436829259297419,
        46.43169593718112,
        10.533940098519743,
        47.08557742620989
      ]
    }
  ],
  "dateRangeSelectValues": [
    {
      "name": "Observed",
      "start": "2021-01-01T00:00:00.000Z",
      "end": "2021-01-15T00:00:00.000Z"
    }
  ]
}')

test_that("Search - Matches Found", {
  client <- Client$new()

  matches <- client$search(QUERY_CORRECT, 10)

  # Download files for *all* results
  temp_dir <- tempdir()
  matches$download(temp_dir)
  print(list.files(temp_dir, recursive = TRUE))
  # delete the folder and files
  unlink(temp_dir, recursive = TRUE, force = TRUE)
})

#test_that("Search - Failed Query", {
#  client <- Client$new()
#  expect_error(matches <- client$search(QUERY_FAILED), "Search query failed")
#})
