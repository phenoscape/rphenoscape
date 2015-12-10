#' @export
pk_get_iri <- function(text, as, verbose=FALSE, limit=10) {
  mssg(verbose, paste("Querying the IRI for", text, sep = " "))
  as_type <- match.arg(as, c("vto", "uberon", "pato"))
  onto_id <- switch(as_type,
                    vto = taxon_id(),
                    uberon = anatomical_id(),
                    pato = phenotype_id())

  queryseq <- list(text = text, definedBy = onto_id, limit = limit )
  lst <- pk_GET('http://kb.phenoscape.org/api/term/search_classes', query = queryseq)
  iri_df <- lst$results

  if (length(iri_df) == 0) {
    mssg(T, paste0("Could not find \"", text, "\" in the database. Please check your input."))
    return(invisible(FALSE))
  }
  # naively take the first result
  # TODO: to be updated once API is updated
  iri_df[1, "@id"]
}

taxon_id <- function() 'http://purl.obolibrary.org/obo/vto.owl'
anatomical_id <- function() 'http://purl.obolibrary.org/obo/uberon.owl'
phenotype_id <- function() 'http://purl.obolibrary.org/obo/pato.owl'

pk_GET <- function(url, queryseq) {
  res <- httr::GET(url, query = queryseq)
  stop_for_pk_status(res)
  out <- httr::content(res, as = "text")

  jsonlite::fromJSON(out, simplifyVector = TRUE, flatten = TRUE)

}
