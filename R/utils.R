url_pattern <- "^https?://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,}(\\:[0-9]+)?(/\\S*)?$"
is_valid_url <- function(url) {
  grepl(url_pattern, url)
}

PLACEHOLDER_TAG <- "__###"

is_placeholder <- function(value) {
  is_single_string(value) && stringr::str_detect(value, paste0("^", PLACEHOLDER_TAG))
}

extract_param_meta_for_string <- function(meta) {
  value <- NULL
  comment <- NA
  possible_values <- NA

  if (exists("default", where = meta)) {
    value <- meta$default
  }

  description <- NA
  if (exists("type", where = meta)) {
    description <- paste("Value of", meta$type)

    if (exists("pattern", where = meta)) {
      description <- paste(description, "type with pattern:", meta$pattern)
    }
    if (exists("format", where = meta)) {
      description <- paste(description, "type with format:", meta$format)
    }
  }

  if (is.null(value) || nchar(value) == 0) {
    value <- paste(PLACEHOLDER_TAG, description)
  } else {
    comment <- description
  }

  if (exists("oneOf", where = meta) && length(meta$oneOf) > 0) {
    possible_values <- sapply(meta$oneOf, function(x) x$const)
    value <- possible_values[[1]]

    if (length(possible_values) == 1) {
      possible_values <- I(list(list(possible_values)))
    } else {
      possible_values <- I(list(possible_values))
    }

    comment <- "One of"
  }

  data.frame(value = value, comment = comment, possible_values = possible_values)
}

extract_param_meta_for_number <- function(meta) {
  value <- NULL
  comment <- NA

  if (exists("default", where = meta)) {
    value <- meta$default
  }

  description <- ""

  if (exists("minimum", where = meta)) {
    description <- paste0(description, "Min: ", meta$minimum, " ")
  }
  if (exists("maximum", where = meta)) {
    description <- paste0(description, "Max: ", meta$maximum, " ")
  }

  if (is.null(value) || nchar(value) == 0) {
    value <- paste(PLACEHOLDER_TAG, description)
  } else {
    comment <- description
  }

  data.frame(value = value, comment = comment, possible_values = NA)
}

extract_param_meta_for_array <- function(meta) {
  value <- NULL

  if (exists("items", where = meta)) {
    if (exists("oneOf", meta$items) && length(meta$items$oneOf) > 0) {
      value <- sapply(meta$items$oneOf, function(x) {
        x$const
      })
      if (length(value) == 1) {
        value <- I(list(list(value)))
      } else {
        value <- I(list(value))
      }
    }
  }

  data.frame(value = I(value), comment = NA, possible_values = NA)
}

extract_param_metadata <- function(meta) {
  switch(meta$type,
    "string" = extract_param_meta_for_string(meta),
    "number" = extract_param_meta_for_number(meta),
    "array" = extract_param_meta_for_array(meta),
  )
}

strip_off_template_placeholders <- function(template) {
  output <- {}

  t <- jsonlite::fromJSON(template, simplifyVector = FALSE)
  for (param in names(t))
  {
    if (startsWith(param, "_comment_")) next
    if (startsWith(param, "_values_")) next
    value <- t[[param]]
    if (!is_placeholder(value)) {
      output[[param]] <- value
    }
  }
  jsonlite::toJSON(output, pretty = TRUE, auto_unbox = TRUE, digits = 17)
}

is_single_string <- function(input) {
  is.character(input) & length(input) == 1
}

format_error_message <- function(
    err,
    context = "Operation failed",
    include_url = TRUE,
    fallback_status = "Unknown") {

  if (is.null(err)) {
    return("Unknown Network Error")
  }

  if (!is.null(err) && inherits(err, "httr2_response")) {
    resp <- err
    parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)

    # Try to parse the 'detail' field to extract error's metadata
    parsed_detail <- NULL
    if (!is.null(parsed$detail) && is.character(parsed$detail) && grepl("^\\s*\\{", parsed$detail)) {
      parsed_detail <- tryCatch(jsonlite::fromJSON(parsed$detail), error = function(e) NULL)
    }

    top_status <- parsed$status_code %||% httr2::resp_status(resp) %||% fallback_status
    top_title <- parsed$title %||% "Unspecified error"
    top_url <- parsed$url %||% NULL

    nested_status <- parsed_detail$status_code %||% NULL
    nested_title <- parsed_detail$title %||% NULL
    nested_detail_text <- parsed_detail$detail %||% ""

    msg_match <- regmatches(nested_detail_text, regexpr("message='([^']+)'", nested_detail_text))
    url_match <- regmatches(nested_detail_text, regexpr("url='([^']+)'", nested_detail_text))

    extracted_message <- NA
    if (length(msg_match) == 1 && grepl("message='[^']+'", msg_match)) {
      extracted_message <- sub("message='([^']+)'", "\\1", msg_match)
    }

    extracted_url <- NA
    if (length(url_match) == 1 && grepl("url='[^']+'", url_match)) {
      extracted_url <- sub("url='([^']+)'", "\\1", url_match)
    }

    final_title <- nested_title %||% top_title
    final_status <- nested_status %||% top_status

    message <- sprintf("%s [HTTP %s]: %s", context, final_status, final_title)

    if (!is.null(extracted_message) && !is.na(extracted_message) && nzchar(extracted_message)) {
      message <- paste0(message, "\nDetail: ", extracted_message)
    }

    if (include_url && !is.null(extracted_url) && !is.na(extracted_url) &&nzchar(extracted_url)) {
      message <- paste0(message, "\nURL: ", extracted_url)
    }

    return(message)
  }


  # Fallback to base condition message
  msg <- conditionMessage(err)
  first_line <- strsplit(msg, "\n")[[1]][1]
  sprintf("%s: %s", context, first_line)
}

