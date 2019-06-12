#' Get term details (ID, label, definition)
#'
#' @name pk_terms
#' @import httr
#' @param term character. The term to be searched.
#' @param verbose logical: optional. If TRUE (default), informative messages printed.
#' @return A data.frame with term id, label, and definition
#' @description
#' Retrieve details about a taxon, an anatomical structure, a gene, or a phenotype.
#'
#' @examples
#' pk_taxon_detail("Coralliozetus")
#' pk_anatomical_detail("basihyal bone")
#' pk_gene_detail("socs5")
#'
#' @export
#' @rdname pk_terms
pk_taxon_detail <- function(term, verbose=FALSE) {
  #pk_details(term, as = "vto", verbose)
  # update to a verbose version of taxon search
  iri <- pk_get_iri(term, as = "taxon", nomatch = FALSE)
  if (iri == FALSE) return(invisible(NA))
  mssg(verbose, "Retrieving term details")

  queryseq <- list(iri = iri)
  lst <- unlist(pk_GET(pk_taxon_url, queryseq))
  det <- as.data.frame(t(lst))
  colnames(det) <- sub("@", "", names(lst))
  det
}

#' @export
#' @rdname pk_terms
pk_anatomical_detail <- function(term, verbose=FALSE) {
  pk_details(term, as = "anatomy", verbose = verbose)
}
#' @export
#' @rdname pk_terms
pk_phenotype_detail <- function(term, verbose=FALSE) {
  pk_details(term, as = "pato", verbose = verbose)
}

#' @export
#' @rdname pk_terms
pk_gene_detail <- function(term, verbose=FALSE) {
  # TODO: resolve taxon to NCBI IRI
  queryseq <- list(text = term)
  res <- pk_GET("http://kb.phenoscape.org/api/gene/search", query = queryseq)
  res$results
}

#' Test if a taxon is extinct.
#'
#' @param taxon character, the taxon name to be tested.
#' @return logical, TRUE if extinct, FALSE if not
#' @export
pk_is_extinct <- function(taxon) {
  det <- pk_taxon_detail(taxon)
  if (is.na(det)) return(invisible(NA))
  det$extinct
}


pk_details <- function(term, as, verbose=FALSE) {
  iri <- pk_get_iri(term, as, verbose = verbose)
  if (is.na(iri)) return(invisible(NA))

  mssg(verbose, "Retrieving term details")

  queryseq <- list(iri = iri)
  lst <- pk_GET("http://kb.phenoscape.org/api/term", queryseq)
  names(lst) <- sub("@", "", names(lst))
  as.data.frame(Filter(function(x) !is.list(x), lst))
}

pk_taxon_url <- "http://kb.phenoscape.org/api/taxon"


