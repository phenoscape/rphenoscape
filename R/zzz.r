.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my first package")
}

mssg <- function(v, ...) if(v) message(...)
