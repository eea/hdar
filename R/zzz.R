#' @importFrom utils packageVersion
NULL

.onAttach <- function(libname, pkgname) {
  # Get the package version
  version <- utils::packageVersion(pkgname)
  # Console message for additional information
  packageStartupMessage(paste(pkgname, version, "\n"))
}
