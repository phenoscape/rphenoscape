#' Query the list of studies by taxa, anatomical entities, and qualities.
#' @param taxon character. The taxon by which to filter, if any.
#' @param entity character. The anatomical entity by which to filter, if any.
#' @param quality character. The phenotypic quality by which to filter, if any.
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
#' @return A data.frame with columns "id" and "label", or an empty list if no
#'   matching study was found.
#'
#' @description
#' Return studies that contain taxa which are members of the optional input taxon,
#' and characters which have phenotype annotations subsumed by the given entity and quality
#' terms.
#' @examples
#' # by default, parts are included
#' slist <- get_studies(taxon = "Siluridae", entity = "fin")
#' colnames(slist)
#' nrow(slist)
#'
#' # can also disable parts
#' slist <- get_studies(taxon = "Siluridae", entity = "fin", includeRels = FALSE)
#' nrow(slist)
#'
#' # or filter studies only by entity, including their parts
#' slist <- get_studies(entity = "pelvic fin", includeRels = c("part of"))
#' nrow(slist)
#'
#' # or filter studies only by entity, including their parts
#' slist <- get_studies(entity = "pelvic fin", includeRels = c("part of"))
#' nrow(slist)
#'
#' # including not only parts but also historical and serial homologs
#' slist <- get_studies(entity = "pelvic fin",
#'                      includeRels = c("part of",
#'                                      "serially homologous to",
#'                                      "historical homologous to"))
#' nrow(slist)
#' # relationship names can be given as prefixes
#' slist1 <- get_studies(entity = "pelvic fin",
#'                       includeRels = c("part", "serial", "historical"))
#' nrow(slist1) == nrow(slist)
#'
#' # or apply no filter, obtaining all studies in the KB
#' slist <- get_studies()
#' nrow(slist)
#' @export
get_studies <- function(taxon = NA, entity = NA, quality = NA,
                        phenotype = NA,
                        includeRels = c("part of"), relation = c("part of")) {

  argsInCall <-  as.list(match.call())[-1]
  # check for deprecated relation parameter
  if (! is.null(argsInCall$relation)) {
    warning("parameter 'relation' is deprecated, use 'includeRels' instead",
            call. = FALSE)
    if (is.null(argsInCall$includeRels)) includeRels <- relation
  }
  # need to make sure to apply our defaults where they differ
  argsInCall$includeRels <- includeRels
  # note that evaluation needs to be in this function's parent frame, or
  # otherwise using it in apply() and friends won't work
  queryseq <- do.call(pkb_args_to_query, argsInCall, envir = parent.frame())
  queryseq <- c(queryseq, limit = "0")
  if (! is.na(phenotype)) queryseq <- c(queryseq, phenotype = phenotype)

  out <- get_json_data(pkb_api("/study/query"), queryseq)
  d <- out$results

  if (length(d) > 0) {
    d <- dplyr::rename(d, id = "@id")
  }
  d
}

#' pk_get_study
#' @param nexmls, a list of [nexml][RNeXML::nexml-class] objects
#' @return A list of data.frames containing matrices
#' @examples
#' \dontrun{
#' slist <- get_studies(taxon = "Ameiurus", entity = "pelvic splint")
#' nex_list <- get_study_data(slist$id) # get the list of NeXML objects for the studies
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
#' slist <- get_studies(taxon = "Ameiurus", entity = "pelvic splint")
#' nex_list <- get_study_data(slist$id) # get the list of NeXML objects for the studies
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
