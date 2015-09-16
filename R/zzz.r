.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to rphenoscape package")
}

mssg <- function(v, ...) if(v) message(...)
