#' Auth Class
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @export
Auth <- R6::R6Class("Auth",
                    public = list(

                      #' @field apiUrl The base URL for the API endpoint.
                      #' This field holds the URL to the API that the client interacts with.
                      apiUrl = 'https://gateway.prod.wekeo2.eu/hda-broker',

                      #' Auth Class Constructor
                      #'
                      #' This function initializes a new instance of the `Auth` class with the specified parameters.
                      #'
                      #' @param user A character string representing the username for authentication.
                      #' @param password A character string representing the password for authentication.
                      #'
                      #' @return An instance of the `Auth` class.
                      #'
                      initialize = function(user = NULL, password = NULL)
                      {
                        private$user <- user
                        private$password <- password
                      },

                      #' Retrieve a Token
                      #'
                      #' This function retrieves a previously generated token.
                      #'
                      #' @return A character string representing the retrieved token.
                      #'
                      token = function()
                      {
                        private$token_value
                      },

                      #' Generate a Token
                      #'
                      #' This function generates a unique token for authentication or other purposes.
                      #'
                      #' @return A character string representing the generated token.
                      #'
                      get_token = function()
                      {
                        url <- paste0(self$apiUrl, "/gettoken")

                        params <- list(
                          'username' = private$user,
                          'password' = private$password
                        )
                        req <-  httr2::request(url) %>%
                          httr2::req_method("POST") %>%
                          httr2::req_body_json(params)

                        try(req %>% httr2::req_perform())
                        resp <- httr2::last_response()

                        if (resp$status_code == 200)
                        {
                          resp_body <- resp %>% httr2::resp_body_json()
                          private$token_value <- resp_body$access_token
                          return(private$token_value)
                        }

                        private$token_value <- NULL
                        stop(resp$detail)
                      }
                    ),

                    private = list(
                      token_value = NULL,
                      user = NULL,
                      password = NULL
                    )
)
