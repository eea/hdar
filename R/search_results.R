#' SearchResults Class
#'
#' This R6 class handles the storage and manipulation of search results including downloading resources based on a search query.
#'
#' @name SearchResults
#' @importFrom R6 R6Class
#' @export
SearchResults <- R6::R6Class("SearchResults",

  public = list(

    #' @field results Stores the search results data.
    results = NULL,

    #' Constructor for SearchResults
    #' @param client An object containing the API client used to interact with the dataset.
    #' @param results List containing search results.
    #' @param dataset_id The identifier for the dataset being queried.
    #' @description Initializes a new SearchResults object with the specified client, results, and dataset identifier.
    initialize = function(client, results, dataset_id) {
      private$client <- client
      private$dataset_id <- dataset_id
      self$results <- results
    },

    #' Download resources
    #' @param output_dir A string specifying the directory where downloaded files will be saved, defaulting to the current directory.
    #' @param selected_indexes Optional; indices of the specific results to download.
    #' @description Downloads resources based on stored results or selected indices of results.
    #' @return Nothing returned but downloaded files are saved at the specified location.
    download = function(output_dir = ".", selected_indexes) {
      print("[Download] Start")

      if (!dir.exists(output_dir)){
        dir.create(output_dir)
      }

      resources_to_download <- self$results
      #resources_to_download <- if (missing(selected_indexes)) self$results else self$results[selected_indexes]
      i <- 0
      for(r in resources_to_download)
      {
        i <- i+1
        print(paste0("[Download] Downloading file ",i,"/",length(resources_to_download)))

        # try to fetch the file extension form $location, assume zip if NULL
        fex <- private$get_file_extention(r$id)
        if(is.null(fex))
        {
          fex <- private$get_file_extention(r$properties$location)
          if(is.null(fex))
          {
            if(i==1)
            {
              warning("No file extensions could be detected, assuming it is a 'zip'")
            }
          fex <- '.zip'
          }
        }

        local_path <- paste0(output_dir, '/', r$id, fex)

        if(!file.exists(local_path))
        {
          download_id <- private$get_download_id(r)
          is_ready <- private$ensure_download_is_ready(download_id)
          if (is_ready)
          {
            private$download_resource(download_id, local_path)
          }
        }
      }
      print("[Download] DONE")
    }
  ),

  private = list(
    client = NULL,
    dataset_id = NULL,

    check_status = function(download_id) {
      url <- paste(private$client$apiUrl, "dataaccess/download", download_id, sep="/")
      req <- request(url) %>%
             req_method("HEAD")

      resp <- private$client$send_request(req)
      resp$status_code
    },

    ensure_download_is_ready = function(download_id) {
      status <- 0
      while(status != 200 && status != 429 && status != 500) {
        Sys.sleep(3)
        status <- private$check_status(download_id)
      }
      status == 200
    },

    get_download_id = function(resource) {

      url <- paste0(private$client$apiUrl, "/dataaccess/download")
      params <- list(
          'cachable' = 'true',
          'dataset_id' = private$dataset_id,
          'product_id' = resource$id,
          'location' = resource$properties$location
      )
      req <-  httr2::request(url) %>%
              httr2::req_method("POST") %>%
              httr2::req_body_json(params)

      resp = private$client$send_request(req)$data
      resp$download_id
    },

    get_file_extention = function(filename)
    {
      filename <- basename(filename)
      # Check if the file contains a dot and is not a hidden file without an extension
      if (grepl("\\.", filename) && !grepl("^\\.", filename))
      {
        ext <- sub(".*\\.([a-zA-Z0-9]{1,4})$", "\\1", filename)
        if (nchar(ext) >= 1 && nchar(ext) <= 4)
        {
          return(paste0('.',ext))
        } else
        {
          return(NULL)
        }
      } else
      {
        return(NULL)
      }
    },

  download_resource = function(download_id, local_path) {

      url <- paste0(private$client$apiUrl, "/dataaccess/download/", download_id)
      req <-  httr2::request(url) %>%
              httr2::req_method("GET")

      resp = private$client$send_request(req, path = local_path)
      if (resp$status_code == 200) {
        return(NA)
      }

      stop(paste("Couldn't download: ", url))
    }

  )

)


