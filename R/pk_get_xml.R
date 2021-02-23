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

  relation_id <- NA
  if (! is.na(relation)) {
    tryCatch(
      relation_type <- match.arg(tolower(relation), c("part of", "develops from")),
      error = function(e) {
        stop(conditionMessage(e), call. = FALSE)
      })
    relation_id <- switch(relation_type,
                          "part of" = part_relation,
                          "develops from" = develops_relation)
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
  if (! is.na(relation_id)) {
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


#' get_study_data
#' @param study_ids, a list of study IDs.
#' @param verbose logical: optional. If TRUE, prints messages prior to potentially
#'   time-consuming operations. Default is FALSE.
#' @return A list of [nexml][RNeXML::nexml] objects
#' @examples
#' \dontrun{
#' slist <- pk_get_study_list(taxon = "Ameiurus", entity = "pelvic splint")
#' nex_list <- get_study_data(slist$id)
#' }
#' @export
get_study_data <- function(study_ids, verbose = FALSE) {

  mssg(verbose, "....This might take a while....")
  ret <- vector('list')

  for (s in study_ids) {
    mssg(verbose, s)
    queryseq <- list(iri = s)
    nex <- get_nexml_data(pkb_api("/study/matrix"), queryseq)
    ret[[s]] <- nex
  }

  ret
}


quantifier <- " some " # seperate quantifier
part_relation <- "<http://purl.obolibrary.org/obo/BFO_0000050>" # "part of"
develops_relation <- "<http://purl.obolibrary.org/obo/RO_0002202>" # "develops from"
