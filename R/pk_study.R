#' Query studies by taxa and anatomical entities
#' @name pk_study
#' @param taxon
#' @param entity
#' @param relation
#'
#' @import RNeXML
#' @return data.frame
#'
#' @description
#' Return studies containing taxa which are members of the optional input taxon
#' expression and are have annotated phenotypes which are relevant to the optional
#' input entity expression.
#'
#' @examples
#'
#' pk_get_studies(taxon = "Ictalurus", entity = "fin")
#'
#' @export
#' @rdname pk_study
pk_get_studies <- function(taxon, entity, relation = "part of") {

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

#' @export
#' @rdname pk_study
pk_search_studies <- function(taxon, entity, relation = "part of") {
  sdf <- pk_get_studies(taxon, entity, relation)
  if (is.logical(sdf)) return(invisible(FALSE))

#   for (s in sdf$`@id`) {
#
#   }
  s1 <- sdf$`@id`[1]
  queryseq <- list(iri = s1)
  res <- httr::GET(pk_study_matrix_url, query = queryseq)
  stop_for_pk_status(res)
  out <- httr::content(res, as = "parsed")

  message("Parse NeXML....")
  nex <- nexml_read(out)

  message("Map symbols to labels...")
  # matrix
  mat0 <- get_characters(nex)
  mat <- rbind(colnames(mat0), mat0)
  #
  states <- get_level(nex, "characters/format/states/state")[, c("symbol", "label", "states")]
  #
  chars <- get_level(nex, "characters/format/char")[, c("states", "label")]

  # util function
  translate_symbol <- function(col) {
    lab <- col[1]
    rest <- col[-1]
    # get the states id corresponds to current column
    st <- chars$states[chars$label == lab]
    # find matching rows in states data frame for current column
    states_match <- states[states$states == st, ]
    #
    sapply(rest, function(x) {
      if(!is.na(x)) states_match$label[states_match$symbol == x]
      else NA
    })
  }

  lst <- apply(mat, 2, translate_symbol)
  ret <- as.data.frame(lst, stringsAsFactors = FALSE)
  ret
}






make_machester <- function(x) paste0("<", x, ">")

pk_study_url <- "http://kb.phenoscape.org/api/study/query"
pk_study_matrix_url <- "http://kb.phenoscape.org/api/study/matrix"
part_relation <- "<http://purl.obolibrary.org/obo/BFO_0000050>" # "part of"
develops_relation <- "<http://purl.obolibrary.org/obo/RO_0002202>" # "develops from"
