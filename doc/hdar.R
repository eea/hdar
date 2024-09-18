## ----eval = FALSE-------------------------------------------------------------
#  # Install hdar from CRAN (if available) or GitHub
#  # install.packages("hdar")
#  # or
#  # devtools::install_github("eea/hdar@develop")
#  
#  # Load the hdar package
#  library(hdar)

## ----eval=FALSE---------------------------------------------------------------
#  # Define your username and password
#  username <- "your_username"
#  password <- "your_password"
#  
#  # Create an instance of the Client class and save credentials to a config file
#  # The save_credentials parameter is optional and defaults to FALSE
#  client <- Client$new(username, password, save_credentials = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  # Create an instance of the Client class without passing credentials
#  client <- Client$new()

## ----eval=FALSE---------------------------------------------------------------
#  # Example method to check authentication by getting the auth token
#  client$get_token()

## ----eval=FALSE---------------------------------------------------------------
#  # Assuming 'client' is already created and authenticated
#  # client <- Client$new()
#  all_datasets <- client$datasets()

## ----eval=FALSE---------------------------------------------------------------
#  # Assuming 'client' is already created and authenticated
#  # client <- Client$new()
#  
#  pattern <- "Seasonal Trajectories"
#  # client <- Client$new()
#  
#  pattern <- "Seasonal Trajectories"
#  filtered_datasets <- client$datasets(pattern)
#  
#  # list dataset IDs
#  sapply(filtered_datasets,FUN = function(x){x$dataset_id})
#  [1] "EO:EEA:DAT:CLMS_HRVPP_VPP-LAEA" "EO:EEA:DAT:CLMS_HRVPP_ST"
#  [3] "EO:EEA:DAT:CLMS_HRVPP_ST-LAEA" "EO:EEA:DAT:CLMS_HRVPP_VPP"

## ----eval = FALSE-------------------------------------------------------------
#  # client <- Client$new()
#  query_template <- client$generate_query_template("EO:EEA:DAT:CLMS_HRVPP_ST")
#  query_template
#  {
#    "dataset_id": "EO:EEA:DAT:CLMS_HRVPP_ST",
#    "uid": "__### Value of string type with pattern: [\\w-]+",
#    "productType": "PPI",
#    "platformSerialIdentifier": "S2A, S2B",
#    "tileId": "__### Value of string type with pattern: [\\w-]+",
#    "productVersion": "__### Value of string type with pattern: [\\w-]+",
#    "resolution": "10",
#    "processingDate": "__### Value of string",
#    "start": "__### Value of string",
#    "end": "__### Value of string",
#    "bbox": [
#      -180,
#      -90,
#      180,
#      90
#    ]
#  }

## ----eval = FALSE-------------------------------------------------------------
#  # convert to list for easier manipulation in R
#  library(jsonlite)
#  query_template <- fromJSON(query_template)
#  query_template
#  $dataset_id
#  [1] "EO:EEA:DAT:CLMS_HRVPP_ST"
#  
#  $uid
#  [1] "__### Value of string type with pattern: [\\w-]+"
#  
#  $productType
#  [1] "PPI"
#  
#  $platformSerialIdentifier
#  [1] "S2A, S2B"
#  
#  $tileId
#  [1] "__### Value of string type with pattern: [\\w-]+"
#  
#  $productVersion
#  [1] "__### Value of string type with pattern: [\\w-]+"
#  
#  $resolution
#  [1] "10"
#  
#  $processingDate
#  [1] "__### Value of string"
#  
#  $start
#  [1] "__### Value of string"
#  
#  $end
#  [1] "__### Value of string"
#  
#  $bbox
#  [1] -180  -90  180   90

## ----eval = FALSE-------------------------------------------------------------
#  # set a new bbox
#  query_template$bbox <- c(11.1090, 46.6210, 11.2090, 46.7210)
#  
#  # limit the time range
#  query_template$start <- "2018-03-01T00:00:00.000Z"
#  query_template$end   <- "2018-05-31T00:00:00.000Z"
#  query_template
#  $dataset_id
#  [1] "EO:EEA:DAT:CLMS_HRVPP_ST"
#  
#  $uid
#  [1] "__### Value of string type with pattern: [\\w-]+"
#  
#  $productType
#  [1] "PPI"
#  
#  $platformSerialIdentifier
#  [1] "S2A, S2B"
#  
#  $tileId
#  [1] "__### Value of string type with pattern: [\\w-]+"
#  
#  $productVersion
#  [1] "__### Value of string type with pattern: [\\w-]+"
#  
#  $resolution
#  [1] "10"
#  
#  $processingDate
#  [1] "__### Value of string"
#  
#  $start
#  [1] "2018-03-01T00:00:00.000Z"
#  
#  $end
#  [1] "2018-05-31T00:00:00.000Z"
#  
#  $bbox
#  [1] 11.109 46.621 11.209 46.721

## ----eval = FALSE-------------------------------------------------------------
#  # convert to JSON format
#  query_template <- toJSON(query_template, auto_unbox = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  # Assuming 'client' is already created and authenticated, 'query' is defined
#  matches <- client$search(query_template)
#  [1] "Found 9 files"
#  [1] "Total Size 1.8 GB"
#  
#  sapply(matches$results,FUN = function(x){x$id})
#  [1] "ST_20180301T000000_S2_T32TPS-010m_V101_PPI" "ST_20180311T000000_S2_T32TPS-010m_V101_PPI"
#  [3] "ST_20180321T000000_S2_T32TPS-010m_V101_PPI" "ST_20180401T000000_S2_T32TPS-010m_V101_PPI"
#  [5] "ST_20180411T000000_S2_T32TPS-010m_V101_PPI" "ST_20180421T000000_S2_T32TPS-010m_V101_PPI"
#  [7] "ST_20180501T000000_S2_T32TPS-010m_V101_PPI" "ST_20180511T000000_S2_T32TPS-010m_V101_PPI"
#  [9] "ST_20180521T000000_S2_T32TPS-010m_V101_PPI"

## ----eval = FALSE-------------------------------------------------------------
#  # Assuming 'matches' is an instance of SearchResults obtained from the search
#  odir <- tempdir()
#  matches$download(odir)
#  The total size is 1.8 GB . Do you want to proceed? (Y/N):
#  y
#  [1] "[Download] Start"
#  [1] "[Download] Downloading file 1/9"
#  [1] "[Download] Downloading file 2/9"
#  [1] "[Download] Downloading file 3/9"
#  [1] "[Download] Downloading file 4/9"
#  [1] "[Download] Downloading file 5/9"
#  [1] "[Download] Downloading file 6/9"
#  [1] "[Download] Downloading file 7/9"
#  [1] "[Download] Downloading file 8/9"
#  [1] "[Download] Downloading file 9/9"
#  [1] "[Download] DONE"
#  
#  # Assuming 'matches' is an instance of SearchResults obtained from the search
#  list.files(odir)
#  [1] "ST_20180301T000000_S2_T32TPS-010m_V101_PPI.tif" "ST_20180311T000000_S2_T32TPS-010m_V101_PPI.tif"
#  [3] "ST_20180321T000000_S2_T32TPS-010m_V101_PPI.tif" "ST_20180401T000000_S2_T32TPS-010m_V101_PPI.tif"
#  [5] "ST_20180411T000000_S2_T32TPS-010m_V101_PPI.tif" "ST_20180421T000000_S2_T32TPS-010m_V101_PPI.tif"
#  [7] "ST_20180501T000000_S2_T32TPS-010m_V101_PPI.tif" "ST_20180511T000000_S2_T32TPS-010m_V101_PPI.tif"
#  [9] "ST_20180521T000000_S2_T32TPS-010m_V101_PPI.tif"
#  
#  unlink(odir,recursive = TRUE)

