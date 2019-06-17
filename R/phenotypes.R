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
#' # by default, parts are included
#' phens <- get_phenotypes(entity = "kinethmoid bone")
#' phens
#' }
#' @return A data frame with columns "id" and "label".
#' @export
get_phenotypes <- function(entity = NA, quality = NA, taxon = NA, study = NA,
                           includeRels = c("part of"),
                           verbose = FALSE) {
  queryseq <- do.call(pkb_args_to_query, as.list(match.call())[-1])
  queryseq <- c(queryseq, limit = "1000000")

  mssg(verbose, "Querying for phenotypes ...")
  out <- get_json_data(pkb_api("/phenotype/query"), queryseq)
  res <- out$results
  colnames(res) <- sub("@", "", x = colnames(res))

  res
}
