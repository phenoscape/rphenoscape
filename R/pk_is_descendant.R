#' Test which candidate terms are ancestors or descendants of a term
#'
#' Tests which in a list of candidate terms are ancestors to or descendants of
#' the query term. Note that terms are not considered ancestors and descendants
#' of themselves.
#'
#' Any of both the query term and the list of candidate terms can be supplied
#' as labels (names), or as IRIs. The function will first resolve any labels
#' to IRIs, allowing any ontology as the target. If labels aren't unique enough
#' across ontologies, it is advisable to do the resolution before calling these
#' functions, using [get_term_iri()] with the appropriate ontology set.
#' @param term character, the label (name) or IRI of the query term
#' @param candidates character, the list of candidate term names or IRIs
#' @param includeRels character, the relationships R for which to include
#'   subclasses of expressions "R _some_ T", where for `is_descendant` T is the
#'   query term, and for `is_ancestor` it is a candidate term.
#'   At present, the only option is `"part_of"`, which will typically only make
#'   sense for anatomy terms. The default is not to include these.
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
#' pk_is_descendant("paired fin", c("pelvic fin", "pelvic fin ray"))
#' pk_is_descendant("paired fin", c("pelvic fin", "pelvic fin ray"), includeRels = "part_of")
#'
#' pk_is_ancestor("pelvic fin", c("paired fin", "hindlimb", "fin"))
#' pk_is_ancestor("pelvic fin ray", c("paired fin", "fin"))
#' pk_is_ancestor("pelvic fin ray", c("paired fin", "fin"), includeRels = "part_of")
#'
#' # phenotypic quality
#' pk_is_ancestor("triangular", c("shape", "color", "amount"))
#' pk_is_descendant("shape", c("T-shaped", "star shaped", "yellow"))
#' }
#' @export
#' @rdname pk_is_descendant
pk_is_descendant <- function(term, candidates, includeRels = c("none", "part_of")) {
  includeRels <- match.arg(includeRels)
  pk_is(term, candidates, mode = 'descendant', includeRels = includeRels)
}

#' @export
#' @rdname pk_is_descendant
pk_is_ancestor <- function(term, candidates, includeRels = c("none", "part_of")) {
  includeRels <- match.arg(includeRels)
  pk_is(term, candidates, mode = 'ancestor', includeRels = includeRels)
}


pk_is <- function(term, candidates,
                  mode = c("ancestor", "descendant"),
                  includeRels = c("none", "part_of")) {
  mode <- match.arg(mode)
  includeRels <- match.arg(includeRels)

  term_iris <- sapply(c(term, candidates),
                      get_term_iri, as = NA, exactOnly = TRUE)
  if (any(is.na(term_iris)))
    warning("The following names could not be resolved as an exact match. ",
            "Results are incomplete.\n\t",
            paste0(c(term, candidates)[is.na(term_iris)], collapse = "\n\t"),
            call. = FALSE)

  queryseq <- list(iri = term_iris[1])
  if (includeRels == "part_of") {
    queryseq <- c(queryseq, parts = "true")
  }

  if (mode == 'ancestor')
    apiURL <- pkb_api("/term/all_ancestors")
  else
    apiURL <- pkb_api("/term/all_descendants")
  res <- pk_GET(apiURL, queryseq)
  res <- res$results

  if (length(res) == 0)
    rep(FALSE, times = length(term_iris) - 1)
  else
    term_iris[-1] %in% res$`@id`
}
