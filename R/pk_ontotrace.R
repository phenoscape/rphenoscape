#' Returns a NeXML-format evolutionary character matrix.
#' @import RNeXML
#' @param taxon characters
#' @param entity characters; anatomical class expression
#'
#' @return data.frame
#'
#' @description
#' Generate matrix of inferred presence/absence associations for anatomical structures
#' subsumed by the provided entity class expression, for any taxa within the provided
#' taxon class expression
#'
#'
#' @export

pk_ontotrace <- function(taxon, entity, variable_only=TRUE) {
  taxon_iri <- pk_get_iri(taxon, "vto")
  ana_iri <- pk_get_iri(entity, "uberon")
  if (length(taxon_iri) == 0 || length(ana_iri) == 0) {
    mssg(length(taxon_iri) == 0, paste("Could not find", taxon, "in the database."))
    mssg(length(ana_iri) == 0, paste("Could not find", entity, "in the database."))
    return(invisible(FALSE))
  }
  queryseq = list(taxon = paste0("<", taxon_iri, ">"),
                  entity = paste0(default_connection, "<", ana_iri, ">"),
                  variable_only = variable_only)
  res <- httr::GET(ontotrace_url, query = queryseq)
  stop_for_pk_status(res)
  out <- content(res, as = "text")

  # TODO: parse NeXML
  get_characters(out)

}

ontotrace_url <- "http://kb.phenoscape.org/api/ontotrace"
default_connection <- "<http://purl.obolibrary.org/obo/BFO_0000050> some"
