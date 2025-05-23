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
        self$total_size <- sum(sapply(results, function(x) if (!is.null(x$properties$size) && is.numeric(x$properties$size)) x$properties$size else 0))
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
    #' @param prompt Optional; enables all user prompts for decisions during file downloads. Defaults to true.
    #' @return Nothing returned but downloaded files are saved at the specified location.
    #' @export
    download = function(output_dir, selected_indexes, stop_at_failure = TRUE, force = FALSE, prompt = TRUE) {
      if (self$total_count == 0 || (prompt && !private$prompt_user_confirmation(self$total_size))) {
        return(NULL)
      }

      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }

      resources_to_download <- if (missing(selected_indexes)) self$results else self$results[selected_indexes]

      proggress_bar <- progress::progress_bar$new(
        format = "[Download] :current/:total (:percent) :elapsed",
        total = length(resources_to_download),
        clear = FALSE,
        width = 60
      )

      i <- 0
      for (r in resources_to_download) {
        i <- i + 1

        tryCatch(
          {
            download_id <- private$get_download_id(r)
            is_ready <- private$ensure_download_is_ready(download_id)
            if (is_ready) {
              private$download_resource(download_id, output_dir, force)
            }
            proggress_bar$tick()
          },
          error = function(err) {
            if (stop_at_failure) {
              proggress_bar$terminate()
              stop(conditionMessage(err))
            } else {
              err_msg <- conditionMessage(err)
              message(err_msg)

              # Special handling for HTTP 429
              if (grepl("\\[HTTP 429\\]", err_msg)) {
                proggress_bar$terminate()
                stop(sprintf("Operation halted due to rate limit (HTTP 429). Please slow down your requests."))
              }

              proggress_bar$message(conditionMessage(err))
              proggress_bar$tick()
            }
          }
        )
      }
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

      filename <- private$check_resource_name(url)
      file_path <- file.path(output_dir, filename)

      # Check if the file already exists and force flag is FALSE
      if (file.exists(file_path) && !force) {
        # warning("File already exists:", file_path, "- Skipping download.\n")
        return(NA)
      }

      req <- httr2::request(url) %>%
        httr2::req_method("GET")

      resp <- private$client$send_request(req, TRUE)
      if (resp$status_code != 200) {
        stop(sprintf(
          "Failed to download data.\nURL: %s\nStatus code: %s\nReason: %s",
          url,
          resp$status_code,
          resp$status_text %||% "Unknown"
        ))
      }

      writeBin(httr2::resp_body_raw(resp), file_path)
      return(NA)
    },
    prompt_user_confirmation = function(total_size) {
      if (total_size >= private$LARGE_DOWNLOAD_SIZE) {
        repeat {
          message("The total size is ", scales::label_bytes()(total_size), ". Do you want to proceed? (Y/N): ")
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
    },
    check_resource_name = function(url) {
      req <- httr2::request(url) %>%
        httr2::req_method("HEAD")

      resp <- private$client$send_request(req, TRUE)
      if (resp$status_code != 200) {
        stop(sprintf(
          "Failed to download data.\nURL: %s\nStatus code: %s\nReason: %s",
          url,
          resp$status_code,
          resp$status_text %||% "Unknown"
        ))
      }

      # Extract the file name from the Content-Disposition header
      content_disposition <- httr2::resp_headers(resp, "content-disposition")
      filename <- if (!is.null(content_disposition)) {
        gsub('.*filename="?([^"]+)"?.*', "\\1", content_disposition)
      } else {
        "downloaded_file"
      }

      return(filename)
    }
  )
)
