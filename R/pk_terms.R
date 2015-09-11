#' Check invasive species status for a set of species from GISD database
#'
#' @import httr
#' @export
#'
#' @param iri characters
#' @param verbose logical; If TRUE (default), informative messages printed.
#'
#' @return A data.frame with term id, label, and definition
#'
#' @description To be expanded
#'
#' @author Hong Xu  \email{hx23@duke.edu}
#' @examples
#'

gk_term <- function(iri, verbose=TRUE) {
  mssg(verbose, "Retrieving term IDs...")
  url <- paste("http://kb.phenoscape.org/api/term?iri=", iri, sep="")
  tt <- GET(url)
  stop_for_status(tt)
  outseq <- content(tt, as="text")

  return(outseq)
}

