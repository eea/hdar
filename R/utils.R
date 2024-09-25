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
    value <- paste(possible_values, collapse = " , ")
    comment <- paste0("One of")
  }

  data.frame(value = value, comment = comment)
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

  data.frame(value = value, comment = comment)
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

  data.frame(value = I(value), comment = NA)
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
