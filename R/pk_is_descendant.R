#' Test which candidate terms are ancestors or descendants of a term
#'
#' Tests which in a list of candidate terms are ancestors to or descendants of
#' the query term. Note that terms are notconsidered ancestors and descendants
#' of themselves.
#'
#' Any of both the query term and the list of candidate terms can be supplied
#' as labels (names), or as IRIs. The function will first resolve any labels
#' to IRIs, allowing any ontology as the target. If labels aren't unique enough
#' across ontologies, it is advisable to do the resolution before calling these
#' functions, using [pk_get_iri][pk_get_iri] with the appropriate ontology set.
#' @param term character, the label (name) or IRI of the query term
#' @param candidates character, the list of candidate term names or IRIs
#' @return A logical vector indicating which candidate terms are ancestors and
#'   descendants, respectively, of the query term.
#' @examples
#' \dontrun{
#' # taxa:
#' pk_is_descendant("Halecostomi", c("Halecostomi", "Icteria", "Sciaenidae"))
#' pk_is_ancestor("Sciaenidae", c("Halecostomi", "Abeomelomys", "Sciaenidae"))
#'
#' # anatomical entities:
#' pk_is_descendant("paired fin", c("pectoral fin", "pelvic fin", "dorsal fin"))
#' pk_is_ancestor("pelvic fin", c("paired fin", "hindlimb", "fin"))
#' 
#' # phenotypic quality
#' pk_is_ancestor("triangular", c("shape", "color", "amount"))
#' pk_is_descendant("shape", c("T-shaped", "star shaped", "yellow"))
#' }
#' @export
#' @rdname pk_is_descendant
pk_is_descendant <- function(term, candidates) {
  pk_is(term, candidates, mode = 'descendant')
}

#' @export
#' @rdname pk_is_descendant
pk_is_ancestor <- function(term, candidates) {
  pk_is(term, candidates, mode = 'ancestor')
}


pk_is <- function(term, candidates, mode = c("ancestor", "descendant")) {
  mode <- match.arg(mode)
  term_iris <- sapply(c(term, candidates),
                      pk_get_iri, as = NA, exactOnly = TRUE)
  if (any(is.na(term_iris)))
    warning("The following names could not be resolved as an exact match. ",
            "Results are incomplete.\n\t",
            paste0(c(term, candidates)[is.na(term_iris)], collapse = "\n\t"),
            call. = FALSE)

  queryseq <- list(iri = term_iris[1])
  if (mode == 'ancestor')
    apiURL <- pk_ancestor_url
  else
    apiURL <- pk_descendant_url
  res <- pk_GET(apiURL, queryseq)
  res <- res$results
  if (length(res) == 0) {
    warning("Could not find the ", mode, "s of ", term, " in the database.")
    return(invisible(NA))
  }
  term_iris[-1] %in% res$`@id`
}

pk_subsumer_url <- "http://kb.phenoscape.org/api/term/least_common_subsumers"
pk_ancestor_url <- "http://kb.phenoscape.org/api/term/all_ancestors"
pk_descendant_url <- "http://kb.phenoscape.org/api/term/all_descendants"
