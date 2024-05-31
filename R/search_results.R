#' @title SearchResults Class
#'
#' @description
#' This class handles the storage and manipulation of search results including downloading resources based on a search query.
#'
#' SearchResults
#' @importFrom R6 R6Class
#' @export
SearchResults <- R6::R6Class("SearchResults",
  public = list(

    #' @field results Stores the search results data.
    results = NULL,

    #' @description Initializes a new SearchResults object with the specified client, results, and dataset identifier.
    #'
    #' @param client An object containing the API client used to interact with the dataset.
    #' @param results List containing search results.
    #' @param dataset_id The identifier for the dataset being queried.
    #' @return SearchResult instance
    #' @export
    initialize = function(client, results, dataset_id) {
      private$client <- client
      private$dataset_id <- dataset_id
      self$results <- results
    },

    #' @description
    #' Downloads resources based on stored results or selected indices of results.
    #' @param output_dir A string specifying the directory where downloaded files will be saved, defaulting to the current directory.
    #' @param selected_indexes Optional; indices of the specific results to download.
    #' @param stop_at_failure Optional; controls whether the download process of multiple files should immediately stop upon encountering the first failure.
    #' @return Nothing returned but downloaded files are saved at the specified location.
    #' @export
    download = function(output_dir = ".", selected_indexes, stop_at_failure = TRUE) {
      print("[Download] Start")

      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }

      resources_to_download <- self$results

      # resources_to_download <- if (missing(selected_indexes)) self$results else self$results[selected_indexes]
      i <- 0
      should_break <- FALSE
      for (r in resources_to_download) {
        if (should_break) {
          break
        }

        i <- i + 1

        print(paste0("[Download] Downloading file ", i, "/", length(resources_to_download)))

        tryCatch(
          {
            download_id <- private$get_download_id(r)
            is_ready <- private$ensure_download_is_ready(download_id)
            if (is_ready) {
              private$download_resource(download_id, output_dir)
            }
          },
          error = function(err) {
            print(err)
            print("[!] An error occurred during the download.")
            should_break <<- stop_at_failure
          }
        )
      }
      print("[Download] DONE")
    }
  ),
  private = list(
    client = NULL,
    dataset_id = NULL,
    check_status = function(download_id) {
      url <- paste(private$client$apiUrl, "dataaccess/download", download_id, sep = "/")
      req <- request(url) %>%
        req_method("HEAD")

      resp <- private$client$send_request(req)
      resp$status_code
    },
    ensure_download_is_ready = function(download_id) {
      status <- 0
      while (status != 200 && status != 429 && status != 500) {
        Sys.sleep(3)
        status <- private$check_status(download_id)
      }
      status == 200
    },
    get_download_id = function(resource) {
      url <- paste0(private$client$apiUrl, "/dataaccess/download")
      params <- list(
        "cachable" = "true",
        "dataset_id" = private$dataset_id,
        "product_id" = resource$id,
        "location" = resource$properties$location
      )
      req <- httr2::request(url) %>%
        httr2::req_method("POST") %>%
        httr2::req_body_json(params)

      resp <- private$client$send_request(req)$data
      resp$download_id
    },
    download_resource = function(download_id, output_dir) {
      url <- paste0(private$client$apiUrl, "/dataaccess/download/", download_id)
      req <- httr2::request(url) %>%
        httr2::req_method("GET")


      resp <- private$client$send_request(req, TRUE)
      if (resp$status_code != 200) {
        print(resp)
        stop(paste("Couldn't download: ", url))
      }

      # Extract the file name from the Content-Disposition header
      content_disposition <- httr2::resp_headers(resp, "content-disposition")
      filename <- if (!is.null(content_disposition)) {
        gsub('.*filename="?([^"]+)"?.*', '\\1', content_disposition)
      } else {
        "downloaded_file"
      }

      # Write the content to a file
      file_path <- file.path(output_dir, filename)
      writeBin(httr2::resp_body_raw(resp), file_path)

      return(NA)
    }
  )
)
