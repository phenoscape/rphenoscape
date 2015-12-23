#' Resolve a text term into IRI
#'
#' @param text character. The term to be resolved
#' @param as character. Ontology type. For a taxon, use "vto", anatomical structure, "uberon", phenotype, "pato".
#' @param verbose logical: optional. If TRUE (default), informative messages printed.
#'
#' @return character. The resolved IRI.
#' @export
pk_get_iri <- function(text, as, verbose=FALSE) {
  mssg(verbose, paste("Querying the IRI for", text, sep = " "))
  as_type <- match.arg(as, c("vto", "uberon", "pato"))
  onto_id <- switch(as_type,
                    vto = taxon_id(),
                    uberon = anatomical_id(),
                    pato = phenotype_id())

  queryseq <- list(text = text, definedBy = onto_id, limit = 10)
  lst <- pk_GET('http://kb.phenoscape.org/api/term/search_classes', query = queryseq)
  iri_df <- lst$results

  if (length(iri_df) == 0) {
    mssg(T, paste0("Could not find \"", text, "\" in the database. Please check your input."))
    return(invisible(FALSE))
  }
  # naively take the first result
  # TODO: to be updated once API is updated
  #iri_df[1, "@id"]
  if ('exact' %in% iri_df$matchType) {
    id <- iri_df$`@id`[iri_df$matchType == 'exact']
  }
  else {
    if ('partial' %in% iri_df$matchType) {
      match_type <- 'partial'
    } else {
      match_type <- 'broad'
    }
    labs <-  iri_df$label[iri_df$matchType == match_type]
    ids <- iri_df$`@id`[iri_df$matchType == match_type]
    deco <- ifelse(length(labs) == 1, 'only', 'first')
    warning_msg <- paste("No exact match for", text,
                         "can be found in database. Returning the",
                         deco, match_type,
                         "match", labs[1])
    if (deco == "first") {
      warning_msg <- paste0(warning_msg,
                           ". Other candidates are ",
                           paste(labs[2:length(labs)], collapse = ", "))
    }
    warning(warning_msg,
            call. = FALSE)
    id <- ids[1]
  }

  id
}

taxon_id <- function() 'http://purl.obolibrary.org/obo/vto.owl'
anatomical_id <- function() 'http://purl.obolibrary.org/obo/uberon.owl'
phenotype_id <- function() 'http://purl.obolibrary.org/obo/pato.owl'

pk_GET <- function(url, query) {
  res <- httr::GET(url, query = query)
  stop_for_pk_status(res)
  out <- httr::content(res, as = "text")

  jsonlite::fromJSON(out, simplifyVector = TRUE, flatten = TRUE)

}
