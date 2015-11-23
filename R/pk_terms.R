#' Get traits
#'
#' @name pk_terms
#' @import httr
#' @param term characters
#' @param verbose logical; If TRUE (default), informative messages printed.
#'
#' @return A data.frame with term id, label, and definition
#'
#' @description Retrieve details about a taxa, an anatomical structure, or a phenotype.
#'
#'
#' @export
#' @rdname pk_terms
pk_taxon_detail <- function(term, verbose=TRUE) {
  #pk_details(term, as = "vto", verbose)
  # update to a verbose version of taxon search
  iri <- pk_get_iri(term, as = "vto")
  if (iri == FALSE) return(invisible(FALSE))
  mssg(verbose, "Retrieving term details")

  queryseq <- list(iri = iri)
  lst <- pk_GET(pk_taxon_url, queryseq)
  det <- dplyr::as_data_frame(as.list(unlist(lst)))
  det$extinct <- as.logical(det$extinct)
  det
}
#'
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

#' Test if a taxa is extinct.
#'
#' @param term characters
#' @return logical
#'
#' @export
pk_is_extinct <- function(term) {
  det <- pk_taxon_detail(term)
  if (is.logical(det)) return(invisible(FALSE))
  det$extinct
}

pk_details <- function(term, as, verbose=TRUE) {
  iri <- pk_get_iri(term, as)
  if (iri == FALSE) return(invisible(FALSE))

  mssg(verbose, "Retrieving term details")

  queryseq <- list(iri = iri)
  lst <- pk_GET("http://kb.phenoscape.org/api/term", queryseq)
  dplyr::as_data_frame(lst)
}

pk_taxon_url <- "http://kb.phenoscape.org/api/taxon"


