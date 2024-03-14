pattern <- "^https?://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,}(\\:[0-9]+)?(/\\S*)?$"

isValidUrl = function(url) {
  grepl(pattern, url)
}

extractTemplateParamStringDefaultValue = function(meta) {
  value <- NULL

  if(exists('default', where = meta)) {
    value <- meta$default
  }

  if(exists('oneOf', where = meta) && length(meta$oneOf) > 0) {
    value <- meta$oneOf[[1]]$const
  }

  if(is.null(value) || nchar(value) == 0) NULL else value
}

extractTemplateParamArrayDefaultValue = function(meta) {

  if(exists('items', where = meta)) {

    if (exists('oneOf', meta$items) && length(meta$items$oneOf) > 0) {
      value <- meta$items$oneOf[[1]]$const
      return(list(value))
    }

  }

  NULL
}


extractTemplateParamDefaultValue = function(meta) {
  switch (meta$type,
    "string" = extractTemplateParamStringDefaultValue(meta),
    "array"  = extractTemplateParamArrayDefaultValue(meta))
}
