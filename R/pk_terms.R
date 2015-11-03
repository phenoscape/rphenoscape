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
pk_taxon_detail <- function(term, verbose=TRUE) {
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

#' @export
#' @rdname pk_terms
pk_gene_detail <- function(term, verbose=TRUE) {
  queryseq <- list(text = term)
  res <- httr::GET("http://kb.phenoscape.org/api/gene/search", query = queryseq)
  stop_for_pk_status(res)
  out <- httr::content(res, as = "text")
  out # TODO: parsing
}

pk_details <- function(term, as, verbose=TRUE) {
  iri <- pk_get_iri(term, as)
  if (iri == FALSE) return(invisible(FALSE))

  mssg(verbose, "Retrieving term details")

  queryseq <- list(iri = iri)
  lst <- pk_GET("http://kb.phenoscape.org/api/term", queryseq, verbose)
  dplyr::as_data_frame(lst)
}



