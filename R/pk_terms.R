#' Get traits
#'
#' @name pk_terms
#' @import httr
#' @param term characters
#' @param verbose logical; If TRUE (default), informative messages printed.
#'
#' @return A data.frame with term id, label, and definition
#'
#' @description To be expanded
#'
#'
#' @export
#' @rdname pk_terms
pk_taxa_detail <- function(term, verbose=TRUE) {
  pk_details(term, as = "vto", verbose)
}
#' @export
#' @rdname pk_terms
pk_anatomical_detail <- function(term, verbose=TRUE) {
  pk_details(term, as = "uberon", verbose)
}
#' @export
#' @rdname pk_terms
pk_phenotype_detail <- function(term, verbose=TRUE) {
  pk_details(term, as = "pato", verbose)
}

pk_details <- function(term, as, verbose=TRUE) {

  iri_df <- pk_get_iri(term, as, verbose)
  if (length(iri_df) == 0) {
    mssg(TRUE, paste("Could not find", term, "in the database."))
    return()
  }
  # naively take the first result
  iri <- iri_df[1, "@id"]
  queryseq <- list(iri = iri)

  pk_GET("http://kb.phenoscape.org/api/term", queryseq, verbose)

}

pk_GET <- function(url, queryseq, verbose=TRUE) {
  mssg(verbose, "Retrieving details...")
  res <- GET(url, query = queryseq)
  stop_for_status(res)
  out <- content(res, as = "text")

  jsonlite::fromJSON(out, simplifyVector = TRUE, flatten = TRUE)

}

pk_get_iri <- function(text, as, verbose=TRUE, limit=10) {

  mssg(verbose, paste("Querying the IRI for", text, sep = " "))
  as_type <- match.arg(as, c("vto", "uberon", "pato"))
  onto_id <- switch(as_type,
                    vto = taxa_id(),
                    uberon = anatomical_id(),
                    pato = phenotype_id())

  queryseq <- list(text = text, definedBy = onto_id, limit = limit )

  mssg(T, queryseq)
  res <- GET('http://kb.phenoscape.org/api/term/search_classes', query = queryseq)
  stop_for_status(res)
  out <- content(res, as = "text")

  lst <- jsonlite::fromJSON(out, simplifyVector = TRUE, flatten = TRUE)

  return(lst$results)
}

taxa_id <- function() 'http://purl.obolibrary.org/obo/vto.owl'
anatomical_id <- function() 'http://purl.obolibrary.org/obo/uberon.owl'
phenotype_id <- function() 'http://purl.obolibrary.org/obo/pato.owl'

