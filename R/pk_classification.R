#' Get term classification
#'
#' @param x character. Name of the term
#' @param verbose logical: optional. If TRUE (default), informative messages printed.
#' @name pk_class
#' @return A list containing data.frame
#'
#' @description Return direct superclasses, direct subclasses, and equivalent classes of a given term
#'
#'
#' @export
#' @rdname pk_class
pk_taxon_class <- function(x, verbose=TRUE) {
  pk_class(x, as = "taxon", verbose)
}
#' @export
#' @rdname pk_class
pk_anatomical_class <- function(x, verbose=TRUE) {
  pk_class(x, as = "anatomy", verbose)
}
#' @export
#' @rdname pk_class
pk_phenotype_class <- function(x, verbose=TRUE) {
  pk_class(x, as = "pato", verbose)
}

pk_class <- function(x, as, verbose=TRUE) {
  iri <- pk_get_iri(x, as = as, verbose = verbose)
  if (is.na(iri)) return(invisible(NA))

  mssg(verbose, "Retrieving classification information")

  queryseq <- list(iri = iri)
  pk_GET(pk_class_url, query = queryseq)
}
pk_class_url <- "http://kb.phenoscape.org/api/term/classification"




