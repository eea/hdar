#' Client Class
#'
#' @description
#' The Client is the central gateway for interfacing with the HDA Service.
#' It provides a comprehensive suite of methods to perform operations and retrieve data from the service efficiently.
#'
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @export
Client <- R6::R6Class("Client",
  public = list(

    apiUrl = "https://gateway.prod.wekeo2.eu/hda-broker/api/v1",

    #' Initialize Client
    #'
    #' @description
    #' Constructor for the `Client` class. Initializes a new instance with authentication credentials.
    #'
    #' @param user Character string representing the username for authentication.
    #' @param password Character string representing the password for authentication.
    #' @param overwrite Logical; `FALSE` by default. If `TRUE`, overwrites any existing credentials.
    #' @return An instance of the `Client` class.
    #' @export
    initialize = function(user, password, overwrite = FALSE) {
      if (missing(user) || missing(password)) {
        # read from ~/.hdrc file
        cred <- private$read_credentials_from_file()
        user <- cred[1]
        password <- cred[2]

        # if still missing, throw error
        if (is.na(user) || is.na(password)) {
          stop("You must provide credentials")
        }
      }

      private$save_credentials_to_file(user, password, overwrite)

      private$auth <- Auth$new(user, password)
    },


    #' Retrieve a Token
    #'
    #' @description
    #' Retrieves the current authentication token.
    #'
    #' @return Character string representing the authentication token.
    #' @export
    token = function() {
      private$auth$token()
    },


    #' Generate a Token
    #'
    #' @description
    #' Generates a new authentication token.
    #'
    #' @return Character string representing the newly generated token.
    #' @export
    get_token = function() {
      private$auth$get_token()
    },


    #' Send a Request
    #'
    #' @description
    #' Sends a specified request to the server and returns the response.
    #'
    #' @param req A request object or list representing the HTTP request.
    #' @param path Optional character string specifying the local path where the response should be saved.
    #' @return A response object containing the server's response.
    #' @export
    send_request = function(req, path = NULL) {

      if (is.null(private$auth$token())) {
        private$auth$get_token()
      }


      req <- req %>%
        httr2::req_headers(Authorization = paste("Bearer", private$auth$token())) %>%
        httr2::req_retry(max_tries = 3)

      tryCatch(
        {
          req %>% httr2::req_perform(path = path)
        },
        error = function(err) {
          if (httr2::last_response()$status_code == 403 || httr2::last_response()$status_code == 401) {
            private$auth$get_token()
            req <- req %>% httr2::req_headers(Authorization = paste("Bearer", private$auth$token()))

            tryCatch(
              {
                req %>% httr2::req_perform(path = path)
              },
              error = function(err) {
                stop(paste("Network error. Reason: ", err))
              }
            )
          } else {
            stop(paste("Network error. Reason: ", err))
          }
        }
      )

      resp <- httr2::last_response()
      if (resp$status_code == 200 || resp$status_code == 201 || resp$status_code == 202) {
        content_type <- httr2::resp_content_type(resp)
        data <- NA
        if (!is.na(content_type) && content_type == "application/json") {
          data <- httr2::resp_body_json(resp)
        }
        return(list("data" = data, "status_code" = resp$status_code))
      }
      stop(httr2::resp_body_json(resp)$detail)
    },


    #' Show Terms and Conditions
    #'
    #' @description
    #' This function displays the terms and conditions for the services.
    #'
    #' @return An HTML document containing the terms and conditions in a collapsible format.
    #' @importFrom httr2 request req_method req_url_query
    #' @importFrom htmltools tagList tags tagAppendChild HTML html_print
    show_terms = function() {
      url <- paste0(self$apiUrl, "/terms")
      req <- httr2::request(url) %>%
        httr2::req_method("GET") %>%
        httr2::req_url_query(startIndex = 0, itemsPerPage = 50)

      features <- self$send_request(req)$data$features

      term_id <- sapply(features, function(x) {
        x$term_id
      })
      dupl <- duplicated(term_id)

      features <- features[!dupl]

      # Start the accordion container
      accordion <- htmltools::tags$div(class = "accordion", id = "accordionExample")

      # Iterate through each feature and create a collapsible card
      for (index in seq_along(features)) {
        feature <- features[[index]]
        card <- htmltools::tags$div(
          class = "card",
          htmltools::tags$div(
            class = "card-header", id = paste0("heading", index),
            htmltools::tags$h2(
              class = "mb-0",
              htmltools::tags$button(feature$title,
                class = "btn btn-link",
                type = "button",
                `data-toggle` = "collapse",
                `data-target` = paste0("#collapse", index),
                `aria-expanded` = "true",
                `aria-controls` = paste0("collapse", index),
                style = "width: 100%; text-align: left; padding: 0; color: #007BFF; background-color: transparent; border: none;"
              )
            )
          ),
          htmltools::tags$div(
            id = paste0("collapse", index), class = "collapse", `aria-labelledby` = paste0("heading", index), `data-parent` = "#accordionExample",
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$h4(paste("Term ID:", feature$term_id), style = "color: #6c757d;"),
              htmltools::HTML(feature$abstract)
            )
          )
        )
        # Add the card to the accordion
        accordion <- htmltools::tagAppendChild(accordion, card)
      }

      # Wrap in a container that includes Bootstrap CSS and JavaScript
      full_html <- htmltools::tagList(
        htmltools::tags$head(htmltools::tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")),
        htmltools::tags$body(
          accordion,
          htmltools::tags$script(src = "https://code.jquery.com/jquery-3.3.1.slim.min.js"),
          htmltools::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js"),
          htmltools::tags$script(src = "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js")
        )
      )

      html_file_path <- tempfile(fileext = ".html")
      htmltools::save_html(full_html, file = html_file_path)
      browseURL(html_file_path)
    },


    #' Accept Terms and Conditions
    #'
    #' @description
    #' Function to retrieve and accept terms and conditions. Accepting T&C is permanent,
    #' it is enough to run this function one.
    #' To read T&C see \code{\link{Client$show_terms}}.
    #'
    #' @param term_id A character vector of term_ids that you wish to accept.
    #'                If missing current status is returned.
    #'                Use "\strong{all}" if you want to accept all terms at once.
    #' @param reject  Logical, default 'FALSE'. If TRUE it inverts the operation and
    #'                the provided term_id's are rejected/revoked.
    #' @return A data frame reflecting the actual acceptance status for each term.
    #' @seealso \code{\link{show_terms}} to read the Terms and conditions.
    #' @importFrom httr2 request req_method
    terms_and_conditions = function(term_id, reject = FALSE) {
      terms <- private$get_terms_status()

      if (missing(term_id)) {
        return(terms)
      }

      if (!is.logical(reject)) {
        stop("'reject' must be a logical value (TRUE or FALSE)")
      }

      if (tolower(term_id[1]) == "all") {
        term_id <- terms$term_id
      }

      invalid_term_ids <- term_id[!term_id %in% terms$term_id]

      if (length(invalid_term_ids) > 0) {
        stop("Invalid term_id detected:\n", paste0("\t- ", invalid_term_ids, collapse = "\n"))
      }

      if (reject) {
        action <- "DELETE"
      } else {
        action <- "PUT"
      }

      for (i in seq_along(term_id))
      {
        url <- paste0(self$apiUrl, "/termsaccepted/", term_id[i])
        req <- httr2::request(url) %>% httr2::req_method(action)
        resp <- self$send_request(req)$data

        stopifnot(resp$status_code == 200)
      }
      tacs <- private$get_terms_status()
      tacs$title <- NULL
      return(tacs)
    },


    #' List datasets on WEkEO
    #'
    #' @description
    #' Lists datasets available on WEkEO, optionally filtered by a text pattern.
    #'
    #' @param pattern Optional character string to filter dataset names by matching text.
    #' @return List containing datasets and associated information.
    #' @importFrom httr2 request req_method req_url_query
    #' @export
    datasets = function(pattern = NULL) {
      url <- paste0(self$apiUrl, "/datasets")
      req <- httr2::request(url) %>%
        httr2::req_method("GET") %>%
        httr2::req_url_query(q = pattern, startIndex = 0, itemsPerPage = 20000)

      resp <- self$send_request(req)$data

      datasets <- lapply(resp$features, function(x) {
        # x <- resp$features#[[218]]
        meta <- x$metadata[["_source"]]

        abs <- meta[["abstract"]]
        if (!is.null(abs)) {
          doi <- regmatches(abs, regexpr("https://doi.org/[[:alnum:]\\-]+", abs))
          if (length(doi) == 0) {
            doi <- NULL
          }

          abstract <- gsub("https://doi.org/[[:alnum:]\\-]+", "", abs)
        } else {
          doi <- abstract <- NULL
        }

        list(
          "terms" = x$terms,
          "dataset_id" = x$dataset_id,
          "title" = meta[["datasetTitle"]],
          "abstract" = abstract,
          "doi" = doi,
          "thumbnails" = meta[["thumbnails"]]
        )
      })
    },


    #' Search function
    #'
    #' @description
    #' This function performs a search based on a specified query and returns an instance of \code{\link{SearchResults}}.
    #'
    #' @param query Character string representing the search query.
    #' @param limit Optional; a number specifying the maximum number of results to return.
    #' @return An instance of the \code{\link{SearchResults}} class containing the search results.
    #' @seealso \code{\link[=SearchResults]{SearchResults}} for details on the returned object.
    #' @importFrom httr2 request req_method req_body_json
    #' @export
    search = function(query, limit = NULL) {

      json_query <- jsonlite::toJSON(query, pretty = TRUE, auto_unbox = TRUE)
      json_query = strip_off_template_placeholders(json_query)

      url <- paste0(self$apiUrl, "/dataaccess/search")
      req <- httr2::request(url) %>%
        httr2::req_method("POST") %>%
        httr2::req_body_json(jsonlite::fromJSON(json_query))

      tryCatch(
        {
          paginator <- Paginator$new(self)
          results <- paginator$run(req, limit)

          SearchResults$new(self, results, query$dataset_id)
        },
        error = function(err) {
          #print(paste("error in search: ", err))
          stop(paste("Search query failed"))
        }
      )
    },


    #' Retrieve Raw Query Template
    #'
    #' @description
    #' Retrieves the raw query metadata for a specified datasetId.
    #'
    #' @param datasetId Character, representing the dataset's identifier.
    #' @param to_json Logical; if `TRUE`, returns the data in JSON format.
    #' @return List or JSON file containing the raw query options.
    #' @importFrom httr2 request req_method
    #' @importFrom jsonlite toJSON
    #' @note There are some inconsistencies between the return of `GET querymetadata` and what must be submitted to the HDA.
    #' Use \link{generate_query_template} to resolve these inconsistencies.
    #' @export
    get_querytemplate = function(datasetId, to_json = FALSE) {
      if (missing(datasetId)) {
        stop("The 'datasetId' parameter is required and was not provided.")
      }

      url <- paste0(self$apiUrl, "/dataaccess/queryable/", datasetId)
      req <- httr2::request(url) %>%
        httr2::req_method("GET")

      resp <- self$send_request(req)$data

      if (to_json) {
        resp <- jsonlite::toJSON(resp, pretty = TRUE, auto_unbox = TRUE)
      }
      resp
    },


    #' Generate a Query Template
    #'
    #' @description
    #' This function generates a query template based on a specified datasetId.
    #'
    #' @param datasetId A numeric or character ID representing the dataset.
    #' @return A JSON representing the generated query template.
    #' @export
    generate_query_template = function(datasetId) {
      resp <- self$get_querytemplate(datasetId)
      private$map_metadata_to_query_template(resp, datasetId)
    }
  ),
  private = list(
    auth = NULL,
    read_credentials_from_file = function() {
      if (!file.exists("~/.hdarc")) {
        return(c("", ""))
      }

      file <- readLines("~/.hdarc")
      user <- private$read_credential_property_from_file(file, "user")
      password <- private$read_credential_property_from_file(file, "password")

      c(user, password)
    },
    read_credential_property_from_file = function(file, prop_name) {
      regexp <- paste0("^", prop_name, ":(.*)$")

      idx <- grep(regexp, file)
      if (!idx) {
        return("")
      }
      prop_value <- gsub(regexp, "\\1", file[idx]) %>% trimws()
    },
    save_credentials_to_file = function(user, pwd, overwrite = FALSE) {
      if (!file.exists("~/.hdarc")) {
        file.create("~/.hdarc")
      } else if (!overwrite) {
        return()
      }

      fileConn <- file("~/.hdarc")
      writeLines(
        c(
          paste0("user:", user),
          paste0("password:", pwd)
        ), fileConn
      )
      close(fileConn)
    },
    map_metadata_to_query_template = function(data, dataset_id, to_json = TRUE) {
      obj <- {}

      obj$dataset_id <- dataset_id

      data <- data$properties

      for (param in names(data))
      {
        if (param == "dataset_id") next
        if (is.null(data[[param]])) next

        if (grepl("bbox", param, fixed = TRUE)) {
          extent <- list(
            data[[param]][["items"]][[1]]$minimum,
            data[[param]][["items"]][[2]]$minimum,
            data[[param]][["items"]][[3]]$maximum,
            data[[param]][["items"]][[4]]$maximum
          )
          # obj <- c(obj, setNames(list(extent), "bbox"))
          # obj$bbox <- list(extent)
          next
        }

        pValue <- extract_template_param_default_value(data[[param]])
        if (is.null(pValue)) {
          switch(param,
            "itemsPerPage" = {
              pValue <- 11
            },
            "startIndex" = {
              pValue <- 0
            },
            next
          )
        }
        obj <- c(obj, setNames(pValue, param))
      }
      if (to_json) {
        jsonlite::toJSON(obj, pretty = TRUE, auto_unbox = TRUE)
      } else {
        obj
      }
    },
    get_terms_status = function() {
      url <- paste0(self$apiUrl, "/terms")
      req <- httr2::request(url) %>%
        httr2::req_method("GET") %>%
        httr2::req_url_query(startIndex = 0, itemsPerPage = 50)
      terms <- self$send_request(req)$data$features

      # Convert the list of lists into a data frame, excluding the 'abstract'
      df <- do.call(rbind, lapply(terms, function(x) {
        x$abstract <- NULL # Remove the abstract element
        data.frame(t(unlist(x)), stringsAsFactors = FALSE)
      }))

      url <- paste0(self$apiUrl, "/termsaccepted")
      req <- httr2::request(url) %>%
        httr2::req_method("GET") %>%
        httr2::req_url_query(startIndex = 0, itemsPerPage = 50)
      accepted <- self$send_request(req)$data$features

      # Convert the list of lists into a data frame, excluding the 'abstract'
      accepted <- do.call(rbind, lapply(accepted, function(x) {
        data.frame(t(unlist(x)), stringsAsFactors = FALSE)
      }))

      df$accepted <- df$term_id %in% accepted$term_id

      # remove duplicates
      dupl <- duplicated(df$term_id)
      df <- df[!dupl, ]

      # remove title as its more confusing than helpful
      df$title <- NULL
      rownames(df) <- NULL
      df
    }
  )
)
