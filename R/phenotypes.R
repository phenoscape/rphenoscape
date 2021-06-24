#' Retrieve phenotypes by entity, quality, taxon, and study
#'
#' Retrieves "semantic phenotypes", i.e., phenotypes encoded as ontological
#' expressions. Filtering is possible by anatomical entity (optionally including
#' entities related by certain properties, see `includeRels`), phenotypic
#' quality, taxonomic group where the phenotypes have been recorded, and study
#' (a.k.a. publication).
#'
#' Entity, quality, and taxon can be given as IRI or as name (i.e, term label).
#' In the latter case, names will be resolved to IRIs against anatomy ontologies,
#' PATO, and taxonomy ontologies, respectively. Warnings will be issued if only
#' a partial match can be found. The study must be given as IRI.
#' @param entity character, the anatomical entity by which to filter, if any.
#' @param quality character, the phenotypic quality by which to filter, if any.
#' @param taxon character, the taxon by which to filter, if any.
#' @param study character, the identifier of the study by which to filter, if any.
#' @param includeRels character or vector of characters. The names of relationships
#'   for anatomical entities to include in addition to subtype (`rdfs:subClassOf`).
#'   Defaults to `"part of"`. Set to `FALSE` to not include any additional relationships.
#'   Otherwise one or more of `"part of"`, `"historical homologous to"`, and
#'   `"serially homologous to"`, or set to `TRUE` to include all possible ones. It
#'   is acceptable to use unambiguous prefixes, for example `"historical"`.
#' @param .withTaxon logical, whether to include taxa in the result if `taxon` is
#'   provided. If TRUE, only the combination of phenotype and taxon will be
#'   unique in the returned data frame. Default is FALSE, meaning by default
#'   providing a value for `taxon` only acts as another filter but does not
#'   change format or redundancy of the result. Ignored if `taxon` is
#'   not provided as a character value.
#' @param verbose logical, whether to print messages informing about potentially
#'   time-consuming operations. Default is FALSE.
#' @examples
#' phens1 <- get_phenotypes(entity = "pelvic fin")
#' head(phens1)
#'
#' # by default, parts are already included
#' phens2 <- get_phenotypes(entity = "pelvic fin", includeRels = c("part"))
#' nrow(phens1) == nrow(phens2)
#' table(phens2$id %in% phens1$id)
#'
#' # but historical homologues are not
#' phens2 <- get_phenotypes(entity = "pelvic fin", includeRels = c("part", "hist"))
#' table(phens2$id %in% phens1$id)
#'
#' # neither are serially homologous
#' phens2 <- get_phenotypes(entity = "pelvic fin", includeRels = TRUE)
#' table(phens2$id %in% phens1$id)
#'
#' # filter also by quality
#' phens2 <- get_phenotypes(entity = "pelvic fin", quality = "shape")
#' table(phens1$id %in% phens2$id)
#'
#' # filter also by quality and taxon
#' phens2 <- get_phenotypes(entity = "pelvic fin", quality = "shape", taxon = "Siluriformes")
#' table(phens1$id %in% phens2$id)
#'
#' # filter by entity, quality and taxon, and return taxa as well (resulting in
#' # (phenotype, taxon) "tuples")
#' phens2a <- get_phenotypes(entity = "pelvic fin", quality = "shape", taxon = "Siluriformes",
#'                           .withTaxon = TRUE)
#' head(phens2a)
#' nrow(phens2a) - nrow(phens2) # lots of redundancy due to n:n relationship
#' nrow(unique(phens2a[,c("id", "label")])) == nrow(phens2) # but same #phenotypes
#'
#' # can compute and visualize similarity
#' sm <- jaccard_similarity(terms = phens2$id, .labels = phens2$label, .colnames = "label")
#' plot(hclust(as.dist(1-sm)))
#' @return A data frame with columns "id" and "label".
#'
#'   If a character value for `taxon` was provided, and `.withTaxon` is TRUE’,
#'   columns "taxon.id" and "taxon.label" will be returned as well. While
#'   (phenotypes, taxon) tuples will be unique, both phenotypes and taxa
#'   individually will then be redundant in the returned data frame (the
#'   association is n:n).
#' @export
get_phenotypes <- function(entity = NA, quality = NA, taxon = NA, study = NA,
                           includeRels = c("part of"),
                           .withTaxon = FALSE,
                           verbose = FALSE) {
  argsInCall <-  as.list(match.call())[-1]
  # need to make sure to apply our defaults where they differ
  argsInCall$includeRels <- includeRels
  # note that evaluation needs to be in this function's parent frame, or
  # otherwise using it in apply() and friends won't work
  queryseq <- do.call(pkb_args_to_query, argsInCall, envir = parent.frame())
  queryseq <- c(queryseq, limit = "1000000")

  mssg(verbose, "Querying for phenotypes ...")
  if (is.na(taxon) || ! .withTaxon)
    endp <- "/phenotype/query"
  else
    endp <- "/taxon/annotations"
  out <- get_json_data(pkb_api(endp), queryseq)
  res <- out$results
  if (length(res) > 0) {
    nms <- sub("@", "", x = colnames(res))
    isTaxonCol <- startsWith(nms, "taxon")
    if (any(isTaxonCol)) {
      # remove 'phenotype' prefix for phenotype ID and label
      nms <- sub("phenotype.", "", x = nms)
      # ensure that the additional taxon ID and label columns are last, so
      # we're only adding columns to the end
      i <- seq(1, ncol(res))
      reordering <- c(i[! isTaxonCol], i[isTaxonCol])
      res <- res[, reordering]
      nms <- nms[reordering]
    }
    colnames(res) <- nms
  }
  res
}

#' Which phenotypes match filter
#'
#' Determines which of one or more phenotype IDs match the given filter.
#'
#' At present, the only supported query filter is a list of studies (as their
#' IDs). A phenotype matches if it is linked to at least one of the studies. 
#' @param x character, vector of phenotype IDs to test. A data.frame is
#'   allowed if it has a column labeled "id".
#' @param studies character, vector of study IDs for which to test. A data.frame
#'   is allowed if it has a column labeled "id".
#' @return a logical vector of the same length as the vector of phenotypes, with
#'   TRUE as element if the corresponding phenotype matches, and FALSE otherwise.
#' @examples
#' x <- get_phenotypes(entity = "basihyal bone")
#' nrow(x)
#' # which of these are in the same study or studies as the first one?
#' phenotype_matches(x, get_studies(phenotype = x$id[1]))
#' @export
phenotype_matches <- function(x, studies) {
  if ("id" %in% colnames(x)) x <- x$id
  if ("id" %in% colnames(studies)) studies <- studies$id

  if (! is.character(x))
    stop("x is not a vector of phenotype IDs", call. = FALSE)

  if (length(studies) == 0)
    rep(FALSE, times = length(x))
  else
    sapply(x, function(phen) {
      phen.studies <- get_studies(phenotype = phen)
      if (is.logical(phen.studies))
        FALSE
      else
        any(phen.studies$id %in% studies)
    }, USE.NAMES = FALSE)
}
