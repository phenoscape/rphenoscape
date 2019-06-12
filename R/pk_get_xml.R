#' Obtain a synthetic presence/absence matrix
#' 
#' Queries the Phenoscape KB for a synthetic presence/absence character matrix
#' for the given taxa and anatomical entities, and returns the result as a
#' `nexml` object (from the RNeXML package).
#'
#' The character matrix includes both asserted and logically inferred states. The
#' query always includes all subclasses of both taxa and entities, and by default
#' also includes all parts of the entities. See parameter `relation` for changing
#' this. By default, only characters that are variable across the resulting taxa
#' are included; use `variable_only` to change this.
#' 
#' @import RNeXML
#' @import dplyr
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
#' @return RNeXML::nexml object
#' @examples
#' \dontrun{
#' # one taxon (including subclasses), one entity (including subclasses and 
#' # by default its parts)
#' nex <- pk_get_ontotrace_xml(taxon = "Ictalurus", entity = "fin")
#'
#' # same as above, except do not include parts or other relationships (fin
#' # presence/absence does not vary across Ictalurus, hence need to allow
#' # non-variable characters)
#' nex <- pk_get_ontotrace_xml(taxon = "Ictalurus", entity = "fin",
#'                             relation = NA, variable_only = FALSE)
#'
#' # instead of parts, include entities in develops_from relationship to the query entity
#' nex <- pk_get_ontotrace_xml(taxon = "Ictalurus", entity = "paired fin bud",
#'                             relation = "develops from", variable_only = FALSE)
#'
#' # query with multiple taxa, and/or multiple entities:
#' nex <- pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus"),
#'                             entity = c("pectoral fin", "pelvic fin"))
#'
#' # Use the RNeXML API to obtain the character matrix etc:
#' m <- RNeXML::get_characters(nex)
#' dim(m)      # number of taxa and characters
#' rownames(m) # taxon names
#' colnames(m) # characters (entity names)
#' 
#' }
#' @export
pk_get_ontotrace_xml <- function(taxon, entity, relation = 'part of', variable_only = TRUE) {

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
                       FUN = pk_get_iri, as = "taxon", exactOnly = TRUE)
  entity_iris <- lapply(entity, FUN = pk_get_iri, as = "anatomy")

  # check for successful resolution of all search terms
  if (any(is.na(taxon_iris)) || any(is.na(entity_iris))) {
    return(invisible(nexml()))
  }

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

  res <- httr::POST(ontotrace_url, body = queryseq, encode = "form")
  stop_for_pk_status(res)
  # if passing parsed XML to RNeXML, it needs to be in classes of the XML
  # package, but httr::content now uses the xml2 package for parsing text/xml
  out <- httr::content(res, as = "text")

  nex <- nexml_read(out)
  return(nex)

}


#' pk_get_study_xml
#' @param study_ids, a list of study IDs.
#' @return A list of [nexml][RNeXML::nexml-class] objects
#' @examples
#' \dontrun{
#' slist <- pk_get_study_list(taxon = "Ameiurus", entity = "pelvic splint")
#' nex_list <- pk_get_study_xml(slist$id)
#' }
#' @export
pk_get_study_xml <- function(study_ids) {

  message("....This might take a while....")
  ret <- vector('list')

  for (s in study_ids) {
    message(s)
    queryseq <- list(iri = s)
    res <- httr::GET(pk_study_matrix_url, query = queryseq)
    stop_for_pk_status(res)
    # if passing parsed XML to RNeXML, it needs to be in classes of the XML
    # package, but httr::content now uses the xml2 package for parsing text/xml
    out <- httr::content(res, as = "text")

    message("Parse NeXML....")
    nex <- nexml_read(out)

    ret[[s]] <- nex
  }

  ret
}


ontotrace_url <- "https://kb.phenoscape.org/api/ontotrace"
quantifier <- " some " # seperate quantifier
part_relation <- "<http://purl.obolibrary.org/obo/BFO_0000050>" # "part of"
develops_relation <- "<http://purl.obolibrary.org/obo/RO_0002202>" # "develops from"
pk_study_matrix_url <- "http://kb.phenoscape.org/api/study/matrix"
