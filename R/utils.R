url_pattern <- "^https?://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,}(\\:[0-9]+)?(/\\S*)?$"
is_valid_url <- function(url) {
  grepl(url_pattern, url)
}

PLACEHOLDER_TAG = "__###"

is_placeholder <- function(value) {
  is_single_string(value) && stringr::str_detect(value, paste0("^", PLACEHOLDER_TAG))
}

extract_template_param_string_default_value <- function(meta) {
  value <- NULL

  if (exists("default", where = meta)) {
    value <- meta$default
  }

  if ((is.null(value) || value == '') && exists("type", where = meta)) {
    if (exists("pattern", where = meta)) {
      value <- paste(PLACEHOLDER_TAG, "Value of", meta$type, "type with pattern:", meta$pattern)
    } else {
      value <- paste(PLACEHOLDER_TAG, "Value of", meta$type)
    }
  }

  if (exists("oneOf", where = meta) && length(meta$oneOf) > 0) {
    value <- meta$oneOf[[1]]$const
  }

  if (is.null(value) || nchar(value) == 0) NULL else value
}

extract_template_param_array_default_value <- function(meta) {
  if (exists("items", where = meta)) {
    if (exists("oneOf", meta$items) && length(meta$items$oneOf) > 0) {
      value <- sapply(meta$items$oneOf, function(x) {
        x$const
      })
      return(I(list(value)))
    }
  }

  NULL
}

extract_template_param_default_value <- function(meta) {
  switch(meta$type,
    "string" = extract_template_param_string_default_value(meta),
    "array"  = extract_template_param_array_default_value(meta)
  )
}

strip_off_template_placeholders <- function(template) {
  output <- {}

  t <- jsonlite::fromJSON(template)
  for (param in names(t))
  {
    value <- t[[param]]
    if (!is_placeholder(value)) {
      output[[param]] <- value
    }
  }
  jsonlite::toJSON(output, pretty = TRUE, auto_unbox=TRUE)
}

is_single_string <- function(input) {
  is.character(input) & length(input) == 1
}
