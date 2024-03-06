SearchResults <- R6::R6Class("SearchResults",

  public = list(

    results = NULL,

    initialize = function(client, results, dataset_id) {
      private$client <- client
      private$dataset_id <- dataset_id
      print(paste("search_result | results.length = ", length(results)))
      self$results <- results
    },

    download = function(output_dir = ".", selected_indexes) {
      print("[Download] Start")

      if (!dir.exists(output_dir)){
        dir.create(output_dir)
      }

      print("[Download] Downloading...")

      resources_to_download <- self$results #if (missing(selected_indexes)) self$results else self$results[selected_indexes]

      for(r in resources_to_download) {
        local_path <- paste0(output_dir, '/', r$id, '.zip')
        if(!file.exists(local_path)) {
          download_id <- private$get_download_id(r)
          private$download_resource(download_id, local_path)
        }

      }
      print("[Download] DONE")
    }

  ),

  private = list(
    client = NULL,
    dataset_id = NULL,

#    check_status = function() {
#
#      url <- paste(private$client$apiUrl, "datarequest/status", self$job_id, sep="/")
#      req <- request(url) %>%
#             req_method("GET")
#
#      resp <- private$client$send_request(req)
#      self$status <- resp$status
#    },

#    ensure_job_is_completed = function() {
#      while(tolower(self$status) != 'completed' && tolower(self$status) != 'failed') {
#        Sys.sleep(3)
#        private$check_status()
#      }
#    },

#    get_result_page = function(page = 0, size = 20) {
#
#      url <- paste(private$client$apiUrl, "datarequest/jobs", self$job_id, "result", sep="/")
#      req <- request(url) %>%
#             req_method("GET") %>%
#             req_url_query(`page` = page, `size` = size)
#
#      resp <- private$client$send_request(req)
#    },

#    extract_result_essentials = function(entry = NULL) {
#      list('filename' = entry$filename,
#           'size' = entry$size,
#           'url' = entry$url,
#           'productInfo' = entry$productInfo,
#           'extraInfo' = entry$extraInformation
#           )
#    },

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

      resp = private$client$send_request(req)
      resp$download_id
    },


  download_resource = function(download_id, local_path) {

      print(paste('[] Getting download_id', download_id, ' into ', local_path, " ..."))

      url <- paste0(private$client$apiUrl, "/dataaccess/download/", download_id)
      req <-  httr2::request(url) %>%
              httr2::req_method("GET")

      private$client$send_request(req, path = local_path)

      resp <- httr2::last_response()
      if (resp$status_code == 200) {
        return(NA)
      }

      stop(paste("Couldn't download: ", url))
    }

  )

)


