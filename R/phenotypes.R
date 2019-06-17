#' Retrieve phenotypes by entity, quality, taxon, and study
#'
#' Retrieves "semantic phenotypes", i.e., phenotypes encodes as ontological
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
#' @param verbose logical, whether to print messages informing about potentially
#'   time-consuming operations. Default is FALSE.
#' @examples
#' \dontrun{
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
#' # filter by quality
#' phens2 <- get_phenotypes(entity = "pelvic fin", quality = "shape")
#' table(phens1$id %in% phens2$id)
#'
#' # filter by quality and taxon
#' phens2 <- get_phenotypes(entity = "pelvic fin", quality = "shape", taxon = "Siluriformes")
#' table(phens1$id %in% phens2$id)
#'
#' # can compute and visualize similarity
#' sm <- jaccard_similarity(terms = phens2$id, .labels = phens2$label, .colnames = "label")
#' plot(hclust(as.dist(1-sm)))
#' }
#' @return A data frame with columns "id" and "label".
#' @export
get_phenotypes <- function(entity = NA, quality = NA, taxon = NA, study = NA,
                           includeRels = c("part of"),
                           verbose = FALSE) {
  argsInCall <-  as.list(match.call())[-1]
  # need to make sure to apply our defaults where they differ
  argsInCall$includeRels <- includeRels
  queryseq <- do.call(pkb_args_to_query, argsInCall)
  queryseq <- c(queryseq, limit = "1000000")

  mssg(verbose, "Querying for phenotypes ...")
  out <- get_json_data(pkb_api("/phenotype/query"), queryseq)
  res <- out$results
  colnames(res) <- sub("@", "", x = colnames(res))

  res
}
