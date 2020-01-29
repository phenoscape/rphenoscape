#' Query the list of studies by taxa, anatomical entities, and qualities.
#' @param taxon character. The name of the taxon by which to filter, if any.
#' @param entity character. The name of the anatomical entity by which to filter, if any.
#' @param quality character. The name of the phenotypic quality by which to filter, if any.
#' @param phenotype character. The phenotype (as its identifier) by which to filter, if any.
#'   If provided, matching studies (through its one or more of its character states)
#'   must be linked to the given phenotype, or one subsumed by it. This must be provided
#'   as identifier, no text search will be performed for resolution.
#' @param includeRels character or vector of characters. The names of relationships
#'  for anatomical entities to include in addition to subtype (`is_a`, `rdfs:subClassOf`).
#'  Defaults to `"part_of"`. Set to `FALSE` to not include any additional relationships.
#'  Otherwise one or more of `"part of"`, `"historical homologous to"`, and
#'  `"serially homologous to"`, or set to `TRUE` to include all possible ones. It is
#'  acceptable to use unambiguous prefixes, for example `"historical homolog"`.
#' @param relation character. Deprecated, for backwards compatibility defaults to
#'  `part of`. Only used if `includeRels` is left at its default value.
#'
#' @return data.frame
#'
#' @description
#' Return studies that contain taxa which are members of the optional input taxon,
#' and characters which have phenotype annotations subsumed by the given entity and quality
#' terms.
#' @examples
#' \dontrun{
#' # by default, parts are included
#' slist <- pk_get_study_list(taxon = "Siluridae", entity = "fin")
#' colnames(slist)
#' nrow(slist)
#'
#' # can also disable parts
#' slist <- pk_get_study_list(taxon = "Siluridae", entity = "fin", includeRels = FALSE)
#' nrow(slist)
#'
#' # or filter studies only by entity, including their parts
#' slist <- pk_get_study_list(entity = "pelvic fin", includeRels = c("part of"))
#' nrow(slist)
#'
#' # or filter studies only by entity, including their parts
#' slist <- pk_get_study_list(entity = "pelvic fin", includeRels = c("part of"))
#' nrow(slist)
#'
#' # including not only parts but also historical and serial homologs
#' slist <- pk_get_study_list(entity = "pelvic fin",
#'                            includeRels = c("part of",
#'                                            "serially homologous to",
#'                                            "historical homologous to"))
#' nrow(slist)
#' # relationship names can be given as prefixes
#' slist1 <- pk_get_study_list(entity = "pelvic fin",
#'                             includeRels = c("part", "serial", "historical"))
#' nrow(slist1) == nrow(slist)
#'
#' # or apply no filter, obtaining all studies in the KB
#' slist <- pk_get_study_list()
#' nrow(slist)
#' }
#' @export
pk_get_study_list <- function(taxon = NA, entity = NA, quality = NA,
                              phenotype = NA,
                              includeRels = NA, relation = "part of") {

  if (all(is.na(includeRels)))
    includeRels <- c(relation)
  else if (is.logical(includeRels))
    if (includeRels)
      includeRels <- c("part of",
                       "historical homologous to",
                       "serially homologous to")
    else
      includeRels <- c()

  if (length(includeRels) > 0) {
    tryCatch(
      includeRels <- match.arg(includeRels,
                               c("part of",
                                 "historical homologous to",
                                 "serially homologous to"),
                               several.ok = TRUE),
      error = function(e) {
        stop(conditionMessage(e), call. = FALSE)
      })
  }

  queryseq <- lapply(includeRels,
                     function(x)
                       switch(x,
                              "part of"=c(parts="true"),
                              "historical homologous to"=c(historical_homologs="true"),
                              "serially homologous to"=c(serial_homologs="true")))
  queryseq <- as.list(unlist(queryseq))

  iriQueryParam <- function(lookupText, definedIn, paramName) {
    if (is.na(lookupText)) return(list())
    termIRI <- pk_get_iri(lookupText, as = definedIn)
    if (termIRI == FALSE) stop("Failed to resolve ", lookupText, " in ", toupper(definedIn),
                               call. = FALSE)
    paramList <- list(termIRI)
    names(paramList) <- paramName
    paramList
  }

  queryseq <- c(queryseq,
                iriQueryParam(taxon, "vto", "in_taxon"),
                iriQueryParam(entity, "uberon", "entity"),
                iriQueryParam(quality, "pato", "quality"),
                limit = "100000")

  if (! is.na(phenotype)) queryseq <- c(queryseq, phenotype = phenotype)

  out <- pk_GET(pk_study_url, queryseq)
  d <- out$results

  if (length(d) == 0) {
    warning("No study found in database.", call. = FALSE)
    return(invisible(FALSE))
  }

  d %>% dplyr::rename(id = "@id")
}

#' pk_get_study
#' @param nexmls, a list of [nexml][RNeXML::nexml-class] objects
#' @return A list of data.frames containing matrices
#' @examples
#' \dontrun{
#' slist <- pk_get_study_list(taxon = "Ameiurus", entity = "pelvic splint")
#' nex_list <- pk_get_study_xml(slist$id) # get the list of NeXML objects for the studies
#' pk_get_study(nex_list) # retrieve the study matrices
#' pk_get_study_meta(nex_list) # retrieve the meta data for the studies
#' }
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
#' @param nexmls, a list of NeXML objects
#' @return A list of data.frames containing taxa and characters
#' @examples
#' \dontrun{
#' slist <- pk_get_study_list(taxon = "Ameiurus", entity = "pelvic splint")
#' nex_list <- pk_get_study_xml(slist$id) # get the list of NeXML objects for the studies
#' pk_get_study(nex_list) # retrieve the study matrices
#' pk_get_study_meta(nex_list) # retrieve the meta data for the studies
#' }
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
  property <- label <- href <- otu <- otus.x <- char <- NULL

  id_taxa <- get_taxa(nex)
  id_taxa_meta <- get_metadata(nex, "otu")

  id_taxa <- (id_taxa_meta
              %>% filter(property == meta_attr_taxon)
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

unique_label <- function(m) {
  # this is dependent on <char/> being in
  # the format of  character_*
  cname <- colnames(m)[1]
  c <- grepl('character', cname)
  return(!c)
}

pk_study_url <- "http://kb.phenoscape.org/api/study/query"
