library("testthat")
library("hdar")

# "datasetId": "EO:ESA:DAT:EODC-SENTINEL-2:MSI1C",
# "datasetId": "EO:CRYO:DAT:HRSI:FSC",

QUERY_CORRECT <- jsonlite::fromJSON('{
  "dataset_id": "EO:CRYO:DAT:HRSI:FSC",
  "boundingBoxValues": [
    {
      "name": "bbox",
      "bbox": [
        10.40604182845371,
        46.070752904760845,
        10.62519817997369,
        46.231832161109644
        ]
    }
    ],
  "dateRangeSelectValues": [
    {
      "name": "time",
      "start": "2021-01-01T00:00:00.000Z",
      "end": "2021-02-05T00:00:00.000Z"
    }
    ]
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

  matches <- client$search(QUERY_CORRECT,2)

  # Download files for *all* results
  matches$download("~/deleteme")
  print(list.files("~/deleteme", recursive = TRUE))
  # delete the folder and files
  unlink("~/deleteme", recursive = TRUE, force = TRUE)
})

test_that("Search - Failed Query", {
  client <- Client$new()
  expect_error(matches <- client$search(QUERY_FAILED), "Search query failed")
})




