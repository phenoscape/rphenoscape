#' pk_is_descendant
#' @param taxon character. The taxon name
#' @param candidates a list or vector of taxon IRIs, now only works for one candiate input
#' @return a list of matching ids
#'
#' @name pk_desc
#' @examples
#' \dontrun{
#' pk_is_descendant("Halecostomi", c("Halecostomi", "Icteria", "Sciaenidae"))
#' pk_is_ancestor("Sciaenidae", c("Halecostomi", "Abeomelomys", "Sciaenidae"))
#' }
#' @export
#' @rdname pk_desc
pk_is_descendant <- function(taxon, candidates) {
  taxon_iris <- lapply(c(taxon, candidates), FUN = pk_get_iri, as = "vto", verbose = F)
  if (FALSE %in% taxon_iris) return(invisible(FALSE))

  queryseq <- list(iri = taxon_iris[[1]])
  result <- pk_GET(pk_descendant_url, queryseq)$results
  if (length(result) == 0) {
    message(paste("Could not find the descendants of", taxon, "in the database."))
    return(invisible(FALSE))
  }
  taxon_iris[-1] %in% result$`@id`

}

#' @export
#' @rdname pk_desc
pk_is_ancestor <- function(taxon, candidates) {
  taxon_iris <- lapply(c(taxon, candidates), FUN = pk_get_iri, as = "vto", verbose = F)
  if (FALSE %in% taxon_iris) return(invisible(FALSE))

  queryseq <- list(iri = taxon_iris[[1]])
  result <- pk_GET(pk_anacestor_url, queryseq)$results
  if (length(result) == 0) {
    message(paste("Could not find the ancestors of", taxon, "in the database."))
    return(invisible(FALSE))
  }
  taxon_iris[-1] %in% result$`@id`
}


pk_is_descendant_ <- function(taxon, candidates) {
  taxon_iris <- lapply(c(taxon, candidates), FUN = pk_get_iri, as = "vto", verbose = F)
  if (FALSE %in% taxon_iris) return(invisible(FALSE))
  queryseq <- list(iris = paste(taxon_iris, collapse = ","),
                   definedBy = "http://purl.obolibrary.org/obo/vto.owl")

  result <- pk_GET(pk_subsumer_url, queryseq)$results
  if (length(result) == 0) return(FALSE)
  ans <- result$`@id`
  taxon_iris[-1] %in% ans

}

pk_subsumer_url <- "http://kb.phenoscape.org/api/term/least_common_subsumers"
pk_anacestor_url <- "http://kb.phenoscape.org/api/term/all_ancestors"
pk_descendant_url <- "http://kb.phenoscape.org/api/term/all_descendants"
