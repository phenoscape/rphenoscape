#' Classificatoin
#'
#' @name pk_class
#' @param x characters, name of the term
#'
#' @return A list containing data.frame
#'
#' @description Return direct superclasses, direct subclasses, and equivalent classes of a given term
#'
#'
#' @export
#' @rdname pk_class
pk_taxon_class <- function(x, verbose=TRUE) {
  pk_class(x, as = "vto", verbose)
}
#' @export
#' @rdname pk_class
pk_anatomical_class <- function(x, verbose=TRUE) {
  pk_class(x, as = "uberon", verbose)
}
#' @export
#' @rdname pk_class
pk_phenotype_class <- function(x, verbose=TRUE) {
  pk_class(x, as = "pato", verbose)
}


pk_class <- function(x, as, verbose=TRUE) {
  iri <- pk_get_iri(x, as = as)
  if (iri == FALSE) return(invisible(FALSE))

  mssg(verbose, "Retrieving classification information")

  queryseq <- list(iri = iri)
  pk_GET(pk_class_url, queryseq = queryseq)
}


pk_class_url <- "http://kb.phenoscape.org/api/term/classification"
