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

    #' @field total_count Stores the total count of results' element.
    total_count = NULL,

    #' @field total_size Stores the total size of results
    total_size = NULL,

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

      self$total_count <- length(sapply(results, FUN = function(x) {
        x$id
      }))
      if (self$total_count > 0) {
        self$total_size <- sum(sapply(results, function(x) if (!is.null(x$properties$size)) x$properties$size else 0))
      } else {
        self$total_size <- 0
      }
    },

    #' @description
    #' Downloads resources based on stored results or selected indices of results.
    #' @param output_dir A string specifying the directory where downloaded files will be saved.
    #' @param selected_indexes Optional; indices of the specific results to download.
    #' @param stop_at_failure Optional; controls whether the download process of multiple files should immediately stop upon encountering the first failure.
    #' @param force Optional; forces the download even if the file already exists in the specified output directory.
    #' @return Nothing returned but downloaded files are saved at the specified location.
    #' @export
    download = function(output_dir, selected_indexes, stop_at_failure = TRUE, force = FALSE) {
      if (self$total_count == 0 || !private$prompt_user_confirmation(self$total_size)) {
        return(NULL)
      }

      message("[Download] Start")

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

        message(paste0("[Download] Downloading file ", i, "/", length(resources_to_download)))

        tryCatch(
          {
            download_id <- private$get_download_id(r)
            is_ready <- private$ensure_download_is_ready(download_id)
            if (is_ready) {
              private$download_resource(download_id, output_dir, force)
            }
          },
          error = function(err) {
            warning("Error during the downloading:", err, sep = "\n")
            should_break <<- stop_at_failure
          }
        )
      }
      message("[Download] DONE")
    }
  ),
  private = list(
    LARGE_DOWNLOAD_SIZE = 100 * 1024 * 1024, # 100MB
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
    download_resource = function(download_id, output_dir, force = FALSE) {
      url <- paste0(private$client$apiUrl, "/dataaccess/download/", download_id)
      req <- httr2::request(url) %>%
        httr2::req_method("GET")


      resp <- private$client$send_request(req, TRUE)
      if (resp$status_code != 200) {
        stop(paste("Couldn't download: ", url))
      }

      # Extract the file name from the Content-Disposition header
      content_disposition <- httr2::resp_headers(resp, "content-disposition")
      filename <- if (!is.null(content_disposition)) {
        gsub('.*filename="?([^"]+)"?.*', "\\1", content_disposition)
      } else {
        "downloaded_file"
      }

      # Define the full file path
      file_path <- file.path(output_dir, filename)

      # Check if the file already exists and force flag is FALSE
      if (file.exists(file_path) && !force) {
        warning("File already exists:", file_path, "- Skipping download.\n")
        return(NA)
      }

      writeBin(httr2::resp_body_raw(resp), file_path)

      return(NA)
    },
    prompt_user_confirmation = function(total_size) {
      if (total_size >= private$LARGE_DOWNLOAD_SIZE) {
        repeat {
          message("The total size is", humanize::natural_size(total_size), ". Do you want to proceed? (Y/N): ")
          answer <- tolower(readLines(n = 1))
          if (answer %in% c("y", "n")) {
            return(answer == "y")
          } else {
            message("Invalid input. Please enter 'Y' or 'N'.\n")
            return(TRUE)
          }
        }
      } else {
        return(TRUE)
      }
    }
  )
)
