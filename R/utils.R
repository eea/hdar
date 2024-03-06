pattern <- "^https?://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,}(\\:[0-9]+)?(/\\S*)?$"

isValidUrl = function(url) {
  grepl(pattern, url)
}
