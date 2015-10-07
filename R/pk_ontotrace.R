#' Returns a NeXML-format evolutionary character matrix.
#'
#' @name pk_ontotrace
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
#' @rdname pk_ontotrace
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
  out <- httr::content(res, as = "text")

  # TODO: parse NeXML

  # try to read in as XMLInternalDocument object
  #xml_doc <- XML::xmlParse(out, asText = TRUE)
  #nexml_read(xml_doc) # error: nexml_read cannot read XMLInternalDocument
                       # cannot coerce type 'externalptr' to vector of type 'character'
  # try to read in as file
  d <- tempfile()
  write(out, file = d)
  nex <- nexml_read(d)  # error in parsing xml : “ns:LiteralMeta” is not a defined class
                        # “ns:ResourceMeta” is not a defined class
                        # after changing to"nex:LiteralMeta" and “nex:ResourceMeta” it works
  # fail loudly
#   tryCatch(
#     nex <- nexml_validate(d),
#     error = function(x) stop(x),
#     warning = function(x) stop(x)
#   )

  unlink(d)
  get_characters(nex)
}

test_read_ns <- function() {
  nex <- nexml_read(paste0(path, "test_original.xml"))
  #tree <- get_trees_list(nex)
  get_characters(nex) # to keep
  #meta <- get_metadata(nex)
  #ns <- get_namespaces(nex)
  #return(list(character_matrix = df, phylo = tree, meta = meta, namespace = ns))
}

test_read_nex <- function() {
  nex <- nexml_read(paste0(path, "test.xml"))
  get_characters(nex)
}

test_validate_ns <- function() {
  nexml_validate(paste0(path, "test_original.xml"))
}

test_validate_nex <- function() {
  nexml_validate(paste0(path, "test.xml"))
}

path <- paste0(system.file(package = "rphenoscape"), "/examples/")
ontotrace_url <- "http://kb.phenoscape.org/api/ontotrace"
default_connection <- "<http://purl.obolibrary.org/obo/BFO_0000050> some" # "part-of some"
