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

pk_term_detail <- function(iri, verbose=TRUE) {
  mssg(verbose, "Retrieving term IDs...")

  queryseq <- list(iri = iri)
  res <- GET("http://kb.phenoscape.org/api/term", query = queryseq)
  stop_for_status(res)
  out <- content(res, as = "text")

  lst <- jsonlite::fromJSON(out, simplifyVector = TRUE, flatten = TRUE)
  # result <- dplyr::tbl_df(data.frame(lst))

  return(lst)
}

pk_get_labels <- function(...) {
  iris <- paste(..., sep=",")

  queryseq <- list(iris = iris)
  res <- GET("http://kb.phenoscape.org/api/term/labels", query = queryseq)
  stop_for_status(res)
  out <- content(res, as = "text")

  lst <- jsonlite::fromJSON(out, simplifyVector = TRUE, flatten = TRUE)

  return(lst)
}

