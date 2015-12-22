#' Query the list of studies by taxa and anatomical entities
#' @param taxon character. The taxon name.
#' @param entity character. The entity name.
#' @param relation character. Can be chosen from "part of" and "develops from".
#'
#' @import RNeXML
#' @import dplyr
#' @return data.frame
#'
#' @description
#' Return studies containing taxa which are members of the optional input taxon
#' expression and are have annotated phenotypes which are relevant to the optional
#' input entity expression.
#' @examples
#' \dontrun{
#' slist <- pk_get_study_list(taxon = "Ameiurus", entity = "pelvic splint")
#' nex_list <- pk_get_study_xml(slist$id[2]) # retrieve the matrix for the 2nd study
#' pk_get_study(nex_list)
#' pk_get_study_meta(nex_list)
#' }
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

  if (taxon_id == FALSE || entity_id == FALSE) {
    return(invisible(FALSE))
  }

  queryseq <- list(taxon = make_machester(taxon_id),
                   entity = paste0(relation_id, " some ", make_machester(entity_id)))

  out <- pk_GET(pk_study_url, queryseq)
  d <- out$results

  if (length(d) == 0) {
    mssg(T, paste("No study found in database."))
    return(invisible(FALSE))
  }
  # NULLing out : for the R CMD CHECK
  `@id` <- NULL

  d <- dplyr::as_data_frame(d)
  d %>% dplyr::rename(id = `@id`)
}

#' pk_get_study
#' @param nexmls, a list of RNeXml object
#' @export
pk_get_study <- function(nexmls) {

  snames <- names(nexmls)
  ret <- vector('list')

  for (i in 1:length(nexmls)) {
    s <- snames[i]
    n <- nexmls[[i]]
    ret[[s]] <- pk_get_study_by_one(n)
  }

  ret
}

#' pk_get_study_meta
#' @param nexmls, a list of RNeXml object
#' @export
pk_get_study_meta <- function(nexmls) {

  snames <- names(nexmls)
  ret <- vector('list')

  for (i in 1:length(nexmls)) {
    s <- snames[i]
    n <- nexmls[[i]]
    ret[[s]] <- pk_get_study_meta_by_one(n)
  }

  ret
}


# get study matrix from one nexml object
pk_get_study_by_one <- function(nex) {

  message("Map symbols to labels...")
  # matrix
  mat0 <- get_characters(nex, rownames_as_col = TRUE, otu_id = TRUE)
  mat <- rbind(colnames(mat0)[-c(1,2)], mat0[, -c(1,2)])
  #
  states <- get_level(nex, "characters/format/states/state")[, c("symbol", "label", "states")]
  chars <- get_level(nex, "characters/format/char")[, c("states", "char", "label")]

  # util function
  translate_symbol <- function(col) {
    lab <- col[1]
    rest <- col[-1]
    # get the states id corresponds to current column
    if (unique_label(mat)) {
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
  cbind(mat0[, c(1,2)], ret)

}

# get meta data for study matrix from one nexml object
pk_get_study_meta_by_one <- function(nex) {

  # NULLing out : for the R CMD CHECK
  rel <- label <- href <- otu <- otus.x <- char <- NULL

  id_taxa <- get_taxa(nex)
  id_taxa_meta <- get_metadata(nex, "otu")

  id_taxa <- (id_taxa_meta
              %>% filter(rel == meta_attr_taxon)
              %>% inner_join(id_taxa, by = c("otu" = "otu"))
              %>% select(label, href, otu, otus.x)
              %>% rename(otus = otus.x))

  id_entities <- get_level(nex, "characters/format/char")
  #id_entities_meta <- get_metadata(nex, level="characters/format/char")

  id_entities <- select(id_entities, label, char)

  m_re <- list(id_taxa = id_taxa,
               id_entities = id_entities)

  return(m_re)
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
