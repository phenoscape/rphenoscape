#' Query studies by taxa and anatomical entities
#'
#' @param taxon
#' @param entity
#'
#' @return data.frame
#'
#' @description
#' Return studies containing taxa which are members of the optional input taxon
#' expression and are have annotated phenotypes which are relevant to the optional
#' input entity expression.
#' @export
pk_search_studies <- function(taxon, entity) {
  taxon_id <- pk_get_iri(taxon, as = "vto")
  entity_id <- pk_get_iri(entity, as = "uberon")
}


pk_study_url <- "http://kb.phenoscape.org/api/study/query"
