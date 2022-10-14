#' Get term classification
#'
#' @param x character. Name of the term
#' @param as character. The ontology or ontologies as which to find the term.
#'   Can be provided in several ways: (1) IRI of the ontology; (2) the ID space
#'   for OBO ontologies such as UBERON, VTO, etc; (3) `"anatomy"` and `"taxon"`
#'   as shorthand for all anatomy and taxon ontologies, respectively; (4) NA to
#'   disable any filtering by defining ontology. Options (1) and (2) can be
#'   combined. There is no default.
#' @param verbose logical: optional. If TRUE, prints messages prior to potentially
#'   time-consuming operations. Default is FALSE.
#' @name term_classification
#' @return A list containing data.frame
#'
#' @description Return direct superclasses, direct subclasses, and equivalent classes of a given term
#'
#'
#' @export
#' @rdname term_classification
term_classification <- function(x, as, verbose=FALSE) {
  res <- term_classification_raw(x, as, verbose)
  rclean_jsonld_names(res)
}

term_classification_raw <- function(x, as, verbose=FALSE) {
  iri <- get_term_iri(x, as = as, verbose = verbose)
  if (is.na(iri)) return(invisible(NA))

  mssg(verbose, "Retrieving classification information")

  queryseq <- list(iri = iri)
  get_json_data(pkb_api("/term/classification"), queryseq)
}
