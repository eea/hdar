.onAttach <- function(libname, pkgname) {
  # Get the package version
  version <- packageVersion(pkgname)
  # Console message for additional information
  packageStartupMessage(paste(pkgname, version, "\n"))
}
