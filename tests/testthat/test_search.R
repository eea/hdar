library("testthat")
library("hdar")

# Small (~6 MB, single-file) query: CLMS Grassland 10m yearly, one tile and year.
QUERY_CORRECT <- '{
  "dataset_id": "EO:CLMS:DAT:GRA_10M_YEARLY_2017_PRESENT",
  "datasetIdentifier": [
    "clms_vlcc_grassland_europe_10m_yearly_v1"
  ],
  "startdate": "2020-01-01T00:00:00.000Z",
  "enddate": "2020-12-31T23:59:59.999Z",
  "bbox": [
    11.1,
    46.6,
    11.2,
    46.7
  ]
}'

QUERY_FAILED <- '{
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
}'

test_that("Search - Matches Found", {
  client <- Client$new()

  matches <- client$search(QUERY_CORRECT, 2)
  expect_gt(matches$total_count, 0)

  # Use a dedicated sub-directory so tempdir() itself is never deleted.
  temp_dir <- tempfile("hdar-test-search-")

  matches$download(temp_dir)
  downloaded <- list.files(temp_dir)
  expect_gt(length(downloaded), 0)

  # Downloading again without `force` silently skips existing files: no warning,
  # and the files on disk are unchanged.
  expect_no_warning(matches$download(temp_dir))
  expect_setequal(list.files(temp_dir), downloaded)

  # `force = TRUE` re-downloads, also without a warning.
  expect_no_warning(matches$download(temp_dir, force = TRUE))
  expect_setequal(list.files(temp_dir), downloaded)

  unlink(temp_dir, recursive = TRUE, force = TRUE)
})

test_that("Search - Failed Query", {
  client <- Client$new()
  expect_error(client$search(QUERY_FAILED), "Failed to search data")
})
