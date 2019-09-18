#' Summary and metadata about the Phenoscape KB
#'
#' Returns the date of the current release of the Phenoscape Knowledgebase (KB) and
#' counts of annotated matrices, taxa, phenotypes, characters, and states.
#' 
#' @return A list of class "KBinfo" with summary statistics of annotation counts
#'   and other KB metadata (specifically, a timestamp for the current KB build).
#' @examples
#' kbmeta <- get_KBinfo()
#' names(kbmeta)
#' kbmeta
#' @export
get_KBinfo <- function() {
  resp <- get_json_data(pkb_api("/kb/annotation_summary"), query = list())
  for (timeElem in grep(".*_time", names(resp), value = TRUE)) {
    resp[[timeElem]] <- strptime(resp[[timeElem]], format = "%FT%T", tz = "UTC")
  }
  class(resp) <- c(class(resp), "KBinfo")
  resp
}

#' Print method for objects of class KBinfo
#'
#' @param x the object of class "KBinfo" to be printed
#' @param tz the timezone in which to print timestamp values, defaults to
#'  local timezone
#' @param ... additional paramaters that might be passed to print methods,
#'  ignored here
#' @rdname get_KBinfo
#' @export
print.KBinfo <- function(x, ..., tz = "") {
  cat(sapply(names(x),
             function(i) {
               if (any(grepl("POSIX[c,l]t", class(x[[i]]))))
                 val <- format(as.POSIXct(x[[i]]), usetz = TRUE, tz = tz)
               else
                 val <- x[[i]]
               paste0(toupper(substring(i, 1, 1)),
                      sub("_", " ", substring(i, 2)), ": ", val)
             }),
      sep = "\n")
  invisible(x)
}
