#' Returns a NeXML-format evolutionary character matrix.
#'
#' @name pk_ontotrace
#' @param nex, a nexml object
#'
#' @return data.frame: The OntoTrace matrix.
#'
#' @description
#' Generate matrix of inferred presence/absence associations for anatomical structures
#' subsumed by the provided entity class expression, for any taxa within the provided
#' taxon class expression.
#' @examples
#' \dontrun{
#' nex0 <- get_ontotrace_data(taxon = "Ictalurus", entity = "fin")
#'
#' nex <- get_ontotrace_data(taxon = c("Ictalurus", "Ameiurus"), entity = "fin spine")
#' pk_get_ontotrace(nex)
#' pk_get_ontotrace_meta(nex)
#' }
#' @importFrom RNeXML get_characters
#' @export
#' @rdname pk_ontotrace
pk_get_ontotrace <- function(nex) {

  m <- get_characters(nex, rownames_as_col = TRUE,
                      otu_id = TRUE, otus_id = TRUE)
  return(m)
}

#' @importFrom RNeXML get_taxa get_metadata get_level
#' @importFrom dplyr filter inner_join select rename "%>%"
#' @export
#' @rdname pk_ontotrace
pk_get_ontotrace_meta <- function(nex) {

  # NULLing out : for the R CMD CHECK
  property <- label <- href <- otu <- otus.x <- char <- NULL

  id_taxa <- get_taxa(nex)
  id_taxa_meta <- get_metadata(nex, "otu")

  id_taxa <- (id_taxa_meta
              %>% filter(property == meta_attr_taxon)
              %>% inner_join(id_taxa, by = c("otu" = "otu"))
              %>% select(label, href, otu, otus.x)
              %>% rename(otus = otus.x))

  id_entities <- get_level(nex, "characters/format/char")
  id_entities_meta <- get_metadata(nex, level="characters/format/char")

  id_entities <- (id_entities_meta
                  %>% filter(property == meta_attr_entity)
                  %>% inner_join(id_entities, by = c("char" = "char"))
                  %>% select(label, href, char))

  m_re <- list(id_taxa = id_taxa,
               id_entities = id_entities)

  return(m_re)
}

#' Obtain a synthetic presence/absence matrix
#'
#' Queries the Phenoscape KB for a synthetic presence/absence character matrix
#' for the given taxa and anatomical entities, and returns the result as a
#' [nexml][RNeXML::nexml] object (from the RNeXML package).
#'
#' The character matrix includes both asserted and logically inferred states. The
#' query always includes all subclasses of both taxa and entities, and by default
#' also includes all parts of the entities. See parameter `relation` for changing
#' this. By default, only characters that are variable across the resulting taxa
#' are included; use `variable_only` to change this.
#'
#' @param taxon character, required. A vector of taxon names.
#' @param entity character, required.
#'   A single character string or a vector of anatomical class expressions.
#' @param relation character string, optional.
#'   The relationship to the entities to be included in the result. Must be
#'   either "part of" or "develops from", or set to NA to disable.
#'   Default is "part of".
#' @param variable_only logical, optional.
#'   Whether to only include characters that are variable across the selected
#'   set of taxa. Default is TRUE.
#' @param strict logical, optional. Whether or not to treat any failure to resolve
#'   any taxon or entity names to IRI as input error. Resolution by partial or
#'   other inexact match results in a warning, but is not considered a failure.
#'   If FALSE, query execution will continue with the taxon and entity terms
#'   that did resolve to IRI. Default is TRUE, meaning any resolution failure will
#'   result in an error.
#' @return [RNeXML::nexml] object
#' @examples
#' \dontrun{
#' # one taxon (including subclasses), one entity (including subclasses and 
#' # by default its parts)
#' nex <- get_ontotrace_data(taxon = "Ictalurus", entity = "fin")
#'
#' # same as above, except do not include parts or other relationships (fin
#' # presence/absence does not vary across Ictalurus, hence need to allow
#' # non-variable characters)
#' nex <- get_ontotrace_data(taxon = "Ictalurus", entity = "fin",
#'                           relation = NA, variable_only = FALSE)
#'
#' # instead of parts, include entities in develops_from relationship to the query entity
#' nex <- get_ontotrace_data(taxon = "Ictalurus", entity = "paired fin bud",
#'                           relation = "develops from", variable_only = FALSE)
#'
#' # query with multiple taxa, and/or multiple entities:
#' nex <- get_ontotrace_data(taxon = c("Ictalurus", "Ameiurus"),
#'                           entity = c("pectoral fin", "pelvic fin"))
#'
#' # Use the RNeXML API to obtain the character matrix etc:
#' m <- RNeXML::get_characters(nex)
#' dim(m)      # number of taxa and characters
#' rownames(m) # taxon names
#' colnames(m) # characters (entity names)
#'
#' }
#' @importFrom RNeXML nexml
#' @export
get_ontotrace_data <- function(taxon, entity,
                               relation = 'part of',
                               variable_only = TRUE,
                               strict = TRUE) {

  relation_iri <- NA
  if (! is.na(relation)) {
    tryCatch(
      relation_type <- match.arg(tolower(relation), c("part of", "develops from")),
      error = function(e) {
        stop(conditionMessage(e), call. = FALSE)
      })
    relation_iri <- switch(relation_type,
                          "part of" = partOf_iri(),
                          "develops from" = term_iri("develops from",
                                                     type = "owl:ObjectProperty",
                                                     preferOntologies = c("BFO", "RO")))
  }

  taxon_iris <- lapply(taxon,
                       FUN = get_term_iri, as = "taxon", exactOnly = TRUE)
  entity_iris <- lapply(entity, FUN = get_term_iri, as = "anatomy")

  # check for successful resolution of all search terms
  kinds <- c()
  if (any(is.na(taxon_iris))) {
    kinds <- c("taxon")
    taxon_iris <- taxon_iris[! is.na(taxon_iris)]
  }
  if (any(is.na(entity_iris))) {
    kinds <- c(kinds, "entity")
    entity_iris <- entity_iris[! is.na(entity_iris)]
  }
  if (length(kinds) > 0 && strict)
    stop("One or more ", paste(kinds, collapse = " and "),
         " names failed to resolve to IRI, unable to continue", call. = FALSE)

  # insert necessary "<" and ">" before concatenating string
  taxon_iris <- lapply(taxon_iris, FUN = function(x) paste0("<", x, ">"))
  entity_iris <- lapply(entity_iris, FUN = function(x) paste0("<", x, ">"))
  if (! is.na(relation_iri)) {
    relation_id <- paste0("<", relation_iri, ">")
    entity_iris <- lapply(entity_iris,
                          FUN = function(x) sprintf("(%s or %s %s %s)",
                                                    x,
                                                    relation_id,
                                                    quantifier,
                                                    x))
  }

  queryseq = list(taxon = paste(taxon_iris, collapse = " or "),
                  entity = paste(entity_iris, collapse = " or "),
                  variable_only = variable_only)

  nex <- get_nexml_data(pkb_api("/ontotrace"), queryseq)
  return(nex)
}


meta_attr_taxon <- "dwc:taxonID"
meta_attr_entity <- "obo:IAO_0000219"

#------------------------------#
#      Tests for RNeXML        #


test_read_nex <- function(path = path0) {
  nex <- nexml_read(path)
  nex
}

path0 <- "https://raw.githubusercontent.com/phenoscape/rphenoscape/char-annots-example/inst/examples/ontotrace-result.xml"
quantifier <- " some " # seperate quantifier
