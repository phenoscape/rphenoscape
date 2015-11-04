#' pk_is_descendant
#' @param taxon
#' @param candidates, a list or vector of taxon IRIs, now only works for one candiate input
#' @return a list of matching ids
#'
#' @name pk_desc
#'
#' @export
#' @rdname pk_desc
pk_is_descendant <- function(taxon, candidates) {
  #TODO: https://github.com/phenoscape/phenoscape-kb-services/issues/14
  # workaround: use least common subsumer
  taxon_iris <- lapply(c(taxon, candidates), FUN = pk_get_iri, as = "vto", verbose = TRUE)
  if (FALSE %in% taxon_iris) return(invisible(FALSE))
  queryseq <- list(iris = paste(taxon_iris, collapse = ","),
                   definedBy = "http://purl.obolibrary.org/obo/vto.owl")

  result <- pk_GET(pk_subsumer_url, queryseq)$results
  if (length(result) == 0) return(FALSE)
  ans <- result$`@id`
  taxon_iris[-1] %in% ans

}
#' @export
#' @rdname pk_desc
pk_is_ancestor <- function(taxon, candidates) {

}

pk_subsumer_url <- "http://kb.phenoscape.org/api/term/least_common_subsumers"
"iris=http://purl.obolibrary.org/obo/VTO_0036225,http://purl.obolibrary.org/obo/VTO_0036217&definedBy=http://purl.obolibrary.org/obo/vto.owl"
