#' Determine the general category of terms
#'
#' Terms in the Phenoscape KB fall into different general categories: entity
#' (anatomical entities), quality, phenotype (which typically are entity-quality
#' compositions), and taxon. The category is sometimes needed to plug a term IRI
#' into the right parameter for a function or API call.
#'
#' The implementation uses the following successive steps until a determination
#' is made, or all possibilities are exhausted:
#' - Try infer the category from the object type and the ontology for terms of
#'   certain OBO ontologies.
#' - Consider subsumption by specific upper ontology terms, specifically
#'   the BFO terms "independent continuant" (for entity terms) and "quality"
#'   (for quality terms).
#' - If a label can be obtained, and it matches the pattern "p some X" with
#'   p being a property used for composing classes (e.g., part of, has part, etc),
#'   extract X and recursively apply the algorithm to X.
#' - If superclasses are retrievable and any of them has a label starting with
#'   "phenotype of", determine category as phenotype.
#' - If superclasses are retrievable, apply the algorithm recursively to each
#'   superclass until a positive determination is made.
#'
#' Due to requiring potentially multiple KB API calls per term for those for which
#' the first step fails, this algorithm can be slow.
#' @param x a vector of one or more term IRIs, or a list of such IRIs or term
#'   objects (such as phenotype objects)
#' @return A character vector with the term categories ("entity", "quality",
#'   "phenotype", or "taxon") of the terms in the input list. The category is
#'   NA for terms for which no determination could be made.
#' @examples
#' term_category(c("http://purl.obolibrary.org/obo/UBERON_0011618",
#'                 "http://purl.obolibrary.org/obo/PATO_0002279",
#'                 "http://purl.obolibrary.org/obo/VTO_0071642",
#'                 "http://purl.obolibrary.org/obo/MP_0030825"))
#' phens <- get_phenotypes("basihyal bone")
#' term_category(phens$id[1:3])
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
    types[is.na(types)] <- sapply(terms, term_category_detective)
  }
  types
}

#' Perform some detective work to determine the category of a term
#'
#' This function does the following steps as part of the algorithm used by
#' [term_category()].
#' - If a label can be obtained, and it matches the pattern "p some X" with
#'   p being a property used for composing classes (e.g., part of, has part, etc),
#'   extract X and recursively apply the algorithm to X.
#' - If superclasses are retrievable and any of them has a label starting with
#'   "phenotype of", determine category as phenotype.
#' - If superclasses are retrievable, apply the algorithm recursively to each
#'   superclass until a positive determination is made.
#' @param term The IRI of the term to do detective work for.
#' @return The term category as a character vector, or NA if category determination
#' was unsuccessful.
#' @seealso [term_category()]
#' @noRd
term_category_detective <- function(term) {
  if (term %in% entity_roots())
    "entity"
  else if (term %in% quality_roots())
    "quality"
  else if (any(startsWith(term, semweb_ns())))
    "entity"
  else {
    isE <- is_ancestor(term, candidates = entity_roots(), includeRels = "part_of")
    if (all(isE))
      "entity"
    else if (any(is_ancestor(term, candidates = quality_roots())))
      "quality"
    else if (any(isE))
      "entity"
    else {
      ti <- term_classification(term)
      if (length(ti) == 0)
        NA
      else {
        categ <- NA
        # if {part of, has part, develops from} some X, then should be of same
        # category as X (provided we can find X)
        ent_expr_prefixes <- c("part of some ", "has part some ", "develops from some ")
        if (! is.null(ti$label)) {
          label_matches <- startsWith(ti$label, ent_expr_prefixes)
          if (any(label_matches)) {
            cand_ent_label <- sub(ent_expr_prefixes[label_matches], "", ti$label)
            cand_ents <- find_term(cand_ent_label, matchBy = "rdfs:label", matchTypes = c("exact"))
            # no match by default returns NA
            if (is.data.frame(cand_ents))
              categ <- term_category(cand_ents$id[1])
          }
        }
        # is that failed
        if (is.na(categ)) {
          # if subClassOf of X, then should be of same category as X
          superCls <- ti$subClassOf
          if (is.data.frame(superCls) && nrow(superCls) > 0) {
            # are the superclasses with labels starting with "phenotype of"?
            if (any(startsWith(as.character(superCls$label), "phenotype of"))) {
              categ <- "phenotype"
            } else {
              # otherwise recursively try superclasses
              for (cls in superCls$id) {
                categ <- term_category(cls)
                if (! is.na(categ)) break
              }
            }
          }
        }
        categ
      }
    }
  }
}

entity_roots <- function() {
  c(term_iri("independent continuant", preferOntologies = c("BFO")),
    term_iri("anatomical structure", firstOnly = FALSE))
}

quality_roots <- function() {
  term_iri("quality", preferOntologies = c("BFO", "PATO"), firstOnly = FALSE)
}

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

#' Extract the OBO ontology prefix from IRIs
#'
#' @param x a list or vector of IRIs, and/or objects that have an "id" key.
#' @return
#'   A character vector of the same length as the input
#'   vector or list, with NA in the positions where extracting the OBO
#'   ontology prefix failed.
#' @examples
#' tt <- c("http://purl.obolibrary.org/obo/UBERON_0011618",
#'         "http://purl.obolibrary.org/obo/PATO_0002279",
#'         "http://purl.obolibrary.org/obo/VTO_0071642",
#'         "http://purl.obolibrary.org/obo/MP_0030825",
#'         "http://purl.obolibrary.org/obo/NCBITaxon_7955")
#' obo_prefix(tt)
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
