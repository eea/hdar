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
#  # Create an instance of the Client class
#  client <- Client$new(username, password)

## ----eval=FALSE---------------------------------------------------------------
#  # Create an instance of the Client class
#  client <- Client$new()

## ----eval=FALSE---------------------------------------------------------------
#  # Example method to check authentication by getting the auth token
#  client$get_token()
#  print(client$token())

## ----eval=FALSE---------------------------------------------------------------
#  # Assuming 'client' is already created and authenticated
#  all_datasets <- client$datasets()
#  print(all_datasets)

## ----eval=FALSE---------------------------------------------------------------
#  # Assuming 'client' is already created and authenticated
#  pattern <- "climate"
#  filtered_datasets <- client$datasets(pattern)
#  print(filtered_datase

## ----eval = FALSE-------------------------------------------------------------
#  # Assuming 'client' is already created and authenticated
#  query_template <- client$generate_query_template("EO:CRYO:DAT:HRSI:FSC")
#  print(query_template)

## ----eval = FALSE-------------------------------------------------------------
#  # Modify the query template as needed
#  query_template$cloudCover <- "10"
#  
#  # Perform the search using the modified query template
#  search_results <- client$search(query_template)
#  print(search_results)

## ----eval = FALSE-------------------------------------------------------------
#  # Assuming 'client' is already created and authenticated, 'query' is defined
#  matches <- client$search(query, 5)
#  print(matches$results)

## ----eval = FALSE-------------------------------------------------------------
#  # Assuming 'matches' is an instance of SearchResults obtained from the search
#  matches$download("~/downloads")

## ----eval = FALSE-------------------------------------------------------------
#  # Assuming 'matches' is an instance of SearchResults obtained from the search
#  selected_indexes <- c(1, 3, 5)  # Download only the 1st, 3rd, and 5th results
#  matches$download("~/downloads", selected_indexes)

