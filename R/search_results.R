SearchResults <- R6::R6Class("SearchResults",

  public = list(

    results = NULL,

    initialize = function(client, results, dataset_id) {
      private$client <- client
      private$dataset_id <- dataset_id
      self$results <- results
    },

    download = function(output_dir = ".", selected_indexes) {
      print("[Download] Start")

      if (!dir.exists(output_dir)){
        dir.create(output_dir)
      }

      print("[Download] Downloading...")

      resources_to_download <- self$results
      #resources_to_download <- if (missing(selected_indexes)) self$results else self$results[selected_indexes]

      for(r in resources_to_download) {
        local_path <- paste0(output_dir, '/', r$id, '.zip')
        if(!file.exists(local_path)) {
          download_id <- private$get_download_id(r)
          is_ready <- private$ensure_download_is_ready(download_id)
          if (is_ready) {
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


