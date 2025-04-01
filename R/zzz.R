.onAttach <- function(libname, pkgname) {
  # Get the package version
  #' @importFrom utils packageVersion
  version <- packageVersion(pkgname)
  # Console message for additional information
  packageStartupMessage(paste(pkgname, version, "\n"))
}
