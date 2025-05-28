Paginator <- R6::R6Class("Paginator",
  public = list(
    results = NULL,
    initialize = function(client, request_type = "POST") {
      private$client <- client
      private$request_type <- request_type
    },
    run = function(request, limit = NULL, items_per_page = 10) {
      results <- list()
      start_index <- 0

      if (!is.null(limit) && limit < items_per_page) {
        items_per_page <- limit
      }

      tryCatch(
        {
          repeat {
            resp <- private$get_page(request, start_index, items_per_page)

            for (f in resp$features) {
              results[[length(results) + 1]] <- f
              if (!is.null(limit) && length(results) >= limit) {
                break
              }
            }

            if ((!is.null(limit) && length(results) >= limit) ||
              length(results) >= resp$properties$totalResults || length(results) == 0) {
              break
            }

            start_index <- length(results)
          }

          results
        },
        error = function(err) {
          msg <- sprintf(
            "Failed to retrieve data using paginator:\n%s\n\nOriginal error:\n%s",
            conditionMessage(err),
            capture.output(str(err))
          )
          stop(msg)
        }
      )
    }
  ),
  private = list(
    client = NULL,
    request_type = NULL,
    get_page = function(request, start_index = 0, items_per_page = 10) {
      req <- request
      if (private$request_type == "POST") {
        req <- req %>%
          httr2::req_body_json_modify(
            startIndex = start_index,
            itemsPerPage = items_per_page
          )
      } else {
        params <- list(
          startIndex = start_index,
          itemsPerPage = items_per_page
        )
        req <- req %>%
          httr2::req_url_query(!!!params)
      }
      private$client$send_request(req)$data
    }
  )
)
