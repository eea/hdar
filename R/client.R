#' Client Class
#' @export Client
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
Client <- R6::R6Class("Client",

  public = list(

    apiUrl = 'https://gateway.prod.wekeo2.eu/hda-broker/api/v1',
    auth = NULL,

    #' Client Class Constructor
    #'
    #' This function initializes a new instance of the `Client` class with the specified parameters.
    #'
    #' @param user character. String representing the username for authentication.
    #' @param password character. String representing the password for authentication.
    #' @param overwrite logical. `FALSE` indicating whether to overwrite or not existing credentials.
    #'
    #' @return An instance of the `Client` class.
    #'
    initialize = function(user, password, overwrite = FALSE)
    {
      #self$apiUrl <- 'https://gateway.prod.wekeo2.eu/hda-broker/api/v1'

      if (missing(user) || missing(password))
      {
        # read from ~/.hdrc file
        cred      <- private$read_credentials_from_file()
        user      <- cred[1]
        password  <- cred[2]

        # if still missing, throw error
        if (is.na(user) || is.na(password))
        {
          stop("You must provide credentials")
        }
      }

      private$save_credentials_to_file(user, password, overwrite)

      self$auth <- Auth$new(user, password)
    },


    #' Retrieve a Token
    #'
    #' This function retrieves a previously generated token.
    #'
    #' @return A character string representing the retrieved token.
    #'
    token = function()
    {
      self$auth$token()
    },


    #' Generate a Token
    #'
    #' This function generates a unique token for authentication or other purposes.
    #'
    #' @return A character string representing the generated token.
    #'
    get_token = function()
    {
      self$auth$get_token()
    },


    #' Send a Request
    #'
    #' This function sends a specified request to a service.
    #'
    #' @param req A list or object representing the request to be sent.
    #' @param path An optional parameter specifying the path where the response should be save at.
    #'
    #' @return A response object or list containing the result of the sent request.
    #'
    send_request = function(req, path = NULL)
    {
      if (is.null(self$auth$token()))
      {
        self$auth$get_token()
      }

      req <- req %>%
              httr2::req_headers(Authorization = paste("Bearer", self$auth$token())) %>%
              httr2::req_retry(max_tries = 3)

      tryCatch(
        {
          req %>% httr2::req_perform(path = path)
        },
        error = function(err) {
          if (httr2::last_response()$status_code == 403 || httr2::last_response()$status_code == 401)
          {
            self$auth$get_token()
            req <- req %>% httr2::req_headers(Authorization = paste("Bearer", self$auth$token()))

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
      if (resp$status_code == 200 || resp$status_code == 201 || resp$status_code == 202)
      {
        content_type <- httr2::resp_content_type(resp)
        data <- NA
        if (!is.na(content_type) && content_type == "application/json")
        {
          data <- httr2::resp_body_json(resp)
        }
        return(list('data' = data, 'status_code' = resp$status_code))
      }
      stop(httr2::resp_body_json(resp)$detail)
    },


    #' Accept Terms and Conditions
    #'
    #' This function indicates the acceptance of terms and conditions for a service.
    #'
    #' @return none.
    #' @importFrom httr2 request req_method
    #'
    accept_tac = function()
    {
      url <- paste0(self$apiUrl, "/termsaccepted/Copernicus_General_License")
      req <- httr2::request(url) %>% httr2::req_method("PUT")

      resp <- self$send_request(req)$data
    },


    #' List datasets on WEkEO
    #'
    #' This function lists datasets that can be accessed on WEkEO. You can also
    #' narrow your search by means of a pattern.
    #'
    #' @param pattern character. A string to filter dataset names by matching text. If NULL (default), it lists all datasets. This isn't a regex search, but a simple text match.
    #'
    #' @return list. Containing datasets and associated information.
    #'
    #' @importFrom httr2 request req_method req_url_query
    #'
    datasets = function(pattern = NULL)
    {
      url <- paste0(self$apiUrl, "/datasets")
      req <- httr2::request(url) %>%
             httr2::req_method("GET") %>%
             httr2::req_url_query(q = pattern, startIndex = 0, itemsPerPage = 20000)

      resp <- self$send_request(req)$data

      datasets <- lapply(resp$features, function(x)
            {
              meta <- x$metadata[["_source"]]

              abs <- meta[["abstract"]]
              titles <- regmatches(abs, gregexpr("\\'\\'\\'([^\\']+):", abs))[[1]]


              list (
                "terms" = x$terms,
                "dataset_id" = x$dataset_id,
                "title" = meta[["datasetTitle"]],
                "abtits" = titles,
                #"abstract" = gsub("https://doi.org/[[:alnum:]\\-]+", "", abs),
                #"doi" = regmatches(abs, regexpr("https://doi.org/[[:alnum:]\\-]+", abs)),
                "thumbnails" = meta[["thumbnails"]]
              )
            }
      )
    },


    #' Search function
    #'
    #' This function performs a search based on a specified query.
    #'
    #' @param query Character. String representing the search query.
    #' @return A list or data frame containing the search results.
    #' @importFrom httr2 request req_method req_body_json

    search = function(query, limit = NULL)
    {

      url <- paste0(self$apiUrl, "/dataaccess/search")
      req <- httr2::request(url) %>%
             httr2::req_method("POST") %>%
             httr2::req_body_json(query)

      tryCatch(
        {
          paginator <- Paginator$new(self)
          results = paginator$run(req, limit)

          SearchResults$new(self, results, query$dataset_id)
        },
        error = function(err) {
          #print(paste("error in search: ", err))
          stop(paste("Search query failed"))
        }
      )
    },


    #' Retrieve Raw query template
    #'
    #' This function performs `GET:querymetadata` for a specified datasetId.
    #'
    #' @param datasetId character. 'datasetId' of the specific dataset.
    #' @param to_json logical. If `FALSE` return list, otherwise `toJSON(resp, pretty = TRUE, auto_unbox=TRUE)`
    #' @return list or json file containing the raw query options.
    #' @importFrom httr2 request req_method
    #' @importFrom jsonlite toJSON

    # @note There are some inconsistencies between the return of `GET querymetadata` and what must be submitted to the HDA. Use `generate_query_template` to resolve these inconsistencies.
    #'
    get_querytemplate = function(datasetId, to_json=FALSE)
    {
      url <- paste0(self$apiUrl, "/dataaccess/queryable/", datasetId)
      req <- httr2::request(url) %>%
             httr2::req_method("GET")

      resp <- self$send_request(req)$data

      if(to_json)
      {
        resp <- jsonlite::toJSON(resp, pretty = TRUE, auto_unbox=TRUE)
      }
      resp
    },


    #' Generate a Query Template
    #'
    #' This function generates a query template based on a specified datasetId.
    #'
    #' @param datasetId A numeric or character ID representing the dataset.
    #' @return A JSON representing the generated query template.
    #'
    generate_query_template = function(datasetId)
    {
      resp <- self$get_querytemplate(datasetId)
      private$map_metadata_to_query_template(resp, datasetId)
    }
  ),

  private = list(

    read_credentials_from_file = function()
    {
      if(!file.exists("~/.hdarc"))
      {
        return(c("",""))
      }

      file <- readLines("~/.hdarc")
      user <- private$read_credential_property_from_file(file, "user")
      password <- private$read_credential_property_from_file(file, "password")

      c(user, password)
    },

    read_credential_property_from_file = function(file, prop_name)
    {
      regexp <- paste0("^", prop_name, ":(.*)$")

      idx <- grep(regexp, file)
      if (!idx)
      {
        return("")
      }

      prop_value = gsub(regexp, "\\1", file[idx]) %>% trimws()
    },

    save_credentials_to_file = function(user, pwd, overwrite = FALSE)
    {
      if(!file.exists("~/.hdarc"))
      {
        file.create("~/.hdarc")
      } else if (!overwrite)
      {
        return()
      }

      fileConn <- file("~/.hdarc")
      writeLines(
        c(
          paste0("user:", user),
          paste0("password:", pwd)
        ), fileConn)
      close(fileConn)
    },

    map_metadata_to_query_template = function(data, dataset_id, to_json=TRUE)
    {
      obj <- {}

      obj <- c(obj, setNames(list(dataset_id), "dataset_id"))
      data <- data$properties

      for (param in names(data))
      {

        if (param == 'dataset_id') next
        if (is.null(data[[param]])) next

        if (grepl("bbox", param, fixed = TRUE))
        {
          extent <- list(
            data[[param]][["items"]][[1]]$minimum,
            data[[param]][["items"]][[2]]$minimum,
            data[[param]][["items"]][[3]]$maximum,
            data[[param]][["items"]][[4]]$maximum
          )
          obj <- c(obj, setNames(list(extent), "bbox"))
          next
        }

        pValue <- extractTemplateParamDefaultValue(data[[param]])
        if (is.null(pValue)) next

        obj <- c(obj, setNames(list(pValue), param))
      }
      if (to_json)
      {
        jsonlite::toJSON(obj, pretty = TRUE, auto_unbox=TRUE)
      } else
      {
        obj
      }
    }
  )
)


