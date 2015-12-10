#' Query the list of studies by taxa and anatomical entities
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
#' slist <- pk_get_study_list(taxon = "Ictalurus", entity = "fin")
#' nex_list <- pk_get_study_xml(slist$id)
#' pk_get_study(nex_list)
#' pk_get_study_meta(nex_list)
#'
#' @export
pk_get_study_list <- function(taxon, entity, relation = "part of") {

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

  d <- dplyr::as_data_frame(d)
  d %>% dplyr::rename(id = `@id`)
}

#' pk_get_study
#' @param nexmls, a list of RNeXml object
#' @export
pk_get_study <- function(nexmls) {

  message("....This might take a while....")
  ret <- vector('list')

  for (n in nexmls) {
    ret[[s]] <- pk_get_study_by_one(n)
  }

  ret
}


pk_get_study_by_one <- function(nex) {

  message("Map symbols to labels...")
  # matrix
  mat0 <- get_characters(nex, rownames_as_col = TRUE, otu_id = TRUE)
  mat <- rbind(colnames(mat0), mat0)
  #
  states <- get_level(nex, "characters/format/states/state")[, c("symbol", "label", "states")]
  chars <- get_level(nex, "characters/format/char")[, c("states", "char", "label")]

  # util function
  translate_symbol <- function(col) {
    lab <- col[1]
    rest <- col[-1]
    # get the states id corresponds to current column
    if (unique_label(chars)) {
      st <- chars$states[chars$label == lab]
    } else {
      st <- chars$states[chars$char == lab]
    }
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
unique_label <- function(m) {
  # this is dependent on <char/> being in
  # the format of  character_*
  cname <- colnames(m)[1]
  c <- grepl('character', cname)
  return(!c)
}

pk_study_url <- "http://kb.phenoscape.org/api/study/query"
part_relation <- "<http://purl.obolibrary.org/obo/BFO_0000050>" # "part of"
develops_relation <- "<http://purl.obolibrary.org/obo/RO_0002202>" # "develops from"
