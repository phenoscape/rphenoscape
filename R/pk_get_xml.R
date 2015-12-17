#' pk_get_ontotrace_xml
#' @import RNeXML
#' @import dplyr
#' @param taxon character: Required. A single character string or a vector of taxa.
#' @param entity characters: Required. A single character string or a vector of anatomical class expressions.
#' @param relation character string: Optional. Has to be either "part of" or "develops from". Default is "part of".
#' @param variable_only logical: Optional. Default is TRUE.
#' @return RNeXML object
#' @examples
#' \dontrun{
#' nex0 <- pk_get_ontotrace_xml(taxon = "Ictalurus australis", entity = "fin")
#' }
#' \dontrun{
#' nex <- pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus"),
#'                             entity = "fin spine")
#' }
#' @export
pk_get_ontotrace_xml <- function(taxon, entity, relation = 'part of', variable_only = TRUE) {

  tryCatch(
    relation_type <- match.arg(tolower(relation), c("part of", "develops from")),
    error = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    })

  relation_id <- switch(relation_type,
                        "part of" = part_relation,
                        "develops from" = develops_relation)

  taxon_iris <- lapply(taxon, FUN = pk_get_iri, as = "vto", verbose = FALSE)
  entity_iris <- lapply(entity, FUN = pk_get_iri, as = "uberon", verbose = FALSE)

  # FALSE will be returned by pk_get_iri if there's no match in database
  if (FALSE %in% taxon_iris || FALSE %in% entity_iris) {
#     stop(paste(c("Could not find",
#                  taxon[which(taxon_iris == FALSE)],
#                  entity[which(entity_iris == FALSE)],
#                  "in the database."),
#                collapse = " * "),
#          call. = FALSE)
    return(invisible(FALSE))
  }

  # insert necessary "<" and ">" before concatenating string
  taxon_iris <- lapply(taxon_iris, FUN = function(x) paste0("<", x, ">"))
  entity_iris <- lapply(entity_iris, FUN = function(x) paste0("(", relation_id, quantifier, "<", x, ">)"))

  queryseq = list(taxon = paste(taxon_iris, collapse = " or "),
                  entity = paste(entity_iris, collapse = " or "),
                  variable_only = variable_only)

  res <- httr::GET(ontotrace_url, query = queryseq)
  stop_for_pk_status(res)
  out <- httr::content(res, as = "parsed")

  nex <- nexml_read(out)
  return(nex)

}


#' pk_get_study_xml
#' @param study_ids, a list of study ids.
#' @return a list of nexml objects
#' @export
pk_get_study_xml <- function(study_ids) {

  message("....This might take a while....")
  ret <- vector('list')

  for (s in study_ids) {
    message(s)
    queryseq <- list(iri = s)
    res <- httr::GET(pk_study_matrix_url, query = queryseq)
    stop_for_pk_status(res)
    out <- httr::content(res, as = "parsed")

    message("Parse NeXML....")
    nex <- nexml_read(out)

    ret[[s]] <- nex
  }

  ret
}


ontotrace_url <- "http://kb.phenoscape.org/api/ontotrace"
quantifier <- " some " # seperate quantifier
part_relation <- "<http://purl.obolibrary.org/obo/BFO_0000050>" # "part of"
develops_relation <- "<http://purl.obolibrary.org/obo/RO_0002202>" # "develops from"
pk_study_matrix_url <- "http://kb.phenoscape.org/api/study/matrix"
