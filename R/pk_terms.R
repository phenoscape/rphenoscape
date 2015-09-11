#' Check invasive species status for a set of species from GISD database
#'
#' @importFrom jsonlite fromJSON
#' @name pk_terms
#' @export
#'
#' @param term characters
#' @param as characters
#' @param verbose logical; If TRUE (default), informative messages printed.
#'
#' @return A data.frame with term id, label, and definition
#'
#' @description To be expanded
#'
#' @author Hong Xu  \email{hx23@duke.edu}
#' @examples
#'
#'
#'
#' @export
#' @rdname pk_terms
pk_term_detail <- function(term, as, verbose=TRUE) {
  mssg(verbose, "Retrieving term IDs...")

  iri_df <- pk_get_iri(term, as)

  # naively take the first result
  if (length(iri_df) == 0) {
    mssg(TRUE, paste("Could not find", term, "in the database."))
    return()
  }

  iri <- iri_df[1, "@id"]
  queryseq <- list(iri = iri)

  result <- pk_GET("http://kb.phenoscape.org/api/term", queryseq)
  return(result)
}

pk_GET <- function(url, queryseq) {
  res <- GET(url, query = queryseq)
  stop_for_status(res)
  out <- content(res, as = "text")

  lst <- jsonlite::fromJSON(out, simplifyVector = TRUE, flatten = TRUE)
  return(lst)
}

pk_get_iri <- function(text, as, limit=10) {

  as_type <- match.arg(as, c("vto", "uberon"))
  onto_id <- switch(as_type,
                    vto = vto_id(),
                    uberon = uberon_id())

  queryseq <- list(text = text, definedBy = onto_id, limit = limit )

  res <- GET('http://kb.phenoscape.org/api/term/search_classes', query = queryseq)
  stop_for_status(res)
  out <- content(res, as = "text")

  lst <- jsonlite::fromJSON(out, simplifyVector = TRUE, flatten = TRUE)

  return(lst$results)
}

vto_id <- function() 'http://purl.obolibrary.org/obo/vto.owl'
uberon_id <- function() 'http://purl.obolibrary.org/obo/uberon.owl'

