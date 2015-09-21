#' Classificatoin
#'
#' @name pk_class
#' @param term characters
#'
#' @return A list containing data.frame
#'
#' @description Return direct superclasses, direct subclasses, and equivalent classes of a given term
#'
#'
#' @export
#' @rdname pk_class
pk_taxon_class <- function(x, verbose=TRUE) {
  iri <- pk_get_iri(x, as = "vto")
  if (iri == FALSE) return(invisible(FALSE))

  mssg(verbose, "Retrieving classification information")

  queryseq <- list(iri = iri)
  pk_GET(pk_class_url, queryseq = queryseq)

}

pk_class_url <- "http://kb.phenoscape.org/api/term/classification"
