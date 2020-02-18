#' Determine the general category of terms
#'
#' Terms in the Phenoscape KB fall into different general categories: entity,
#' quality, phenotype (which typically are entity-quality compositions), and
#' taxon. The category is sometimes needed to plug a term IRI into the right
#' parameter for a function or API call.
#'
#' The implementation will first try infer the category from the object type
#' and the ontology for terms of certain OBO ontologies. Where that fails
#' it will rely subsumption by specific upper ontology terms, specifically
#' the BFO terms "independent continuant" (for entity terms) and "quality"
#' (for quality terms).
#' @param x a vector of one or more term IRIs, or a list of such IRIs or term
#'   objects (such as phenotype objects)
#' @return A character vector with the term categories ("entity", "quality",
#'   "phenotype", or "taxon") of the terms in the input list.
#' @examples
#' term_category(c("http://purl.obolibrary.org/obo/UBERON_0011618",
#'                 "http://purl.obolibrary.org/obo/PATO_0002279",
#'                 "http://purl.obolibrary.org/obo/VTO_0071642"))
#' @export
term_category <- function(x) {
  if (is.phenotype(x)) return("phenotype")
  # try to infer from OBO ontology ID
  onts <- obo_prefix(x)
  types <- obo_ont_type(onts)
  # for those unresolved, check for phenotype objects
  terms <- x[is.na(types)]
  if (length(terms) > 0) {
    is.phen <- sapply(terms, is.phenotype)
    types[is.na(types)] <- ifelse(is.phen, "phenotype", NA)
  }
  # for those remaining unresolved, try to determine by upper ontology ancestor
  terms <- x[is.na(types)]
  if (length(terms) > 0) {
    types[is.na(types)] <- sapply(terms, function(term) {
      EorQ <- pk_is_ancestor(term, candidates = c(entity_root(), quality_root()))
      if (EorQ[1])
        "entity"
      else if (EorQ[2])
        "quality"
      else
        "phenotype"
    })
  }
  types
}

entity_root <- local({
  .term <- NULL
  function() {
    if (is.null(.term)) {
      .term <<- find_term("independent continuant",
                          definedBy = "bfo", matchTypes = "exact")$id
    }
    .term
  }
})

quality_root <- local({
  .term <- NULL
  function() {
    if (is.null(.term)) {
      .term <<- find_term("quality", definedBy = "bfo", matchTypes = "exact")$id
    }
    .term
  }
})

obo_ont_type <- function(ont) {
  ont[is.na(ont)] <- ""
  ontIRIs <- ontology_iri(ont)
  is.ent <- ontIRIs %in% anatomy_ontology_iris()
  is.taxon <- ontIRIs %in% taxon_ontology_iris()
  ifelse(is.ent,
         "entity",
         ifelse(is.taxon,
                "taxon",
                sapply(ont, function(o) {
                  switch(o,
                         PATO = "quality",
                         MP = "phenotype",
                         HP = "phenotype",
                         NCBITaxon = "taxon",
                         NA)
                })))
}

#' @description
#' `obo_prefix` extracts the OBO ontology prefix from IRIs
#' @return
#' `obo_prefix` returns a character vector of the same length as the input
#'   vector or list, with NA in the positions where extracting the OBO
#'   ontology prefix failed.
#' @rdname term_category
#' @importFrom stringi stri_match_first_regex
#' @export
obo_prefix <- function(x) {
  is.iri <- sapply(x, is.character)
  if (! all(is.iri))
    x[! is.iri] <- sapply(x[! is.iri],
                          function(term) {
                            if ("id" %in% names(term))
                              term$id
                            else
                              as.character(term)
                          })
  m <- stringi::stri_match_first_regex(x,
                                       "^https?://purl.obolibrary.org/obo/([A-Za-z0-9]+)_[0-9]+")
  m[, 2]
}