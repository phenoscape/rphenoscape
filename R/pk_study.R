#' Query studies by taxa and anatomical entities
#'
#' @param taxon
#' @param entity
#' @param relation
#'
#' @return data.frame
#'
#' @description
#' Return studies containing taxa which are members of the optional input taxon
#' expression and are have annotated phenotypes which are relevant to the optional
#' input entity expression.
#' @export
pk_search_studies <- function(taxon, entity, relation = "part of") {

  tryCatch(
    relation_type <- match.arg(tolower(relation), c("part of", "develops from")),
    error = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    })

  relation_id <- switch(relation_type,
                        "part of" = part_relation,
                        "develops from" = develops_relation)

  taxon_id <- pk_get_iri(taxon, as = "vto")
  entity_id <- pk_get_iri(entity, as = "uberon")

  queryseq <- list(taxon = make_machester(taxon_id),
                   entity = paste0(relation_id, " some ", make_machester(entity_id)))

  out <- pk_GET(pk_study_url, queryseq)
  d <- out$results

  if (length(d) == 0) {
    mssg(T, paste("No study found in database."))
    return(invisible(FALSE))
  }

  dplyr::as_data_frame(d)
}

make_machester <- function(x) paste0("<", x, ">")

pk_study_url <- "http://kb.phenoscape.org/api/study/query"
part_relation <- "<http://purl.obolibrary.org/obo/BFO_0000050>" # "part of"
develops_relation <- "<http://purl.obolibrary.org/obo/RO_0002202>" # "develops from"
