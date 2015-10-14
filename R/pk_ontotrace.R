#' Returns a NeXML-format evolutionary character matrix.
#'
#' @name pk_ontotrace
#' @import RNeXML
#' @param taxon characters
#' @param entity characters; anatomical class expression
#' @param relation c("part of", "develops from")
#' @variable_only logical
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
pk_ontotrace <- function(..., relation = "part of", variable_only=TRUE) {
  taxon_entity_list <- list(...)

  if (length(taxon_entity_list$taxon) == 0 || length(taxon_entity_list$entity) == 0) {
    mssg(TRUE, "please explicitly specify taxon and entity parameter,
         e.g. pk_ontotrace(taxon = \"Ictalurus\", entity = \"fin\")")
    return(invisible(FALSE))
  }

  relation_type <- match.arg(tolower(relation), c("part of", "develops from"))
  relation_id <- switch(relation_type,
                        "part of" = part_relation,
                        "develops from" = develops_relation)

  taxon_iris <- lapply(taxon_entity_list$taxon, FUN = pk_get_iri, as = "vto")
  entity_iris <- lapply(taxon_entity_list$entity, FUN = pk_get_iri, as = "uberon")

  if (FALSE %in% taxon_iris || FALSE %in% entity_iris) {
#     mssg(TRUE, paste("Could not find",
#                      taxon_entity_list$taxon[which(taxon_iris == FALSE)],
#                      taxon_entity_list$entity[which(entity_iris == FALSE)],
#                      "in the database."))
    return(invisible(FALSE))
  }


  # insert necessary "<" and ">" before concatenating string
  taxon_iris <- lapply(taxon_iris, FUN = function(x) paste0("<", x, ">"))
  entity_iris <- lapply(entity_iris, FUN = function(x) paste0("(", relation_id, "<", x, ">)"))

  queryseq = list(taxon = paste(taxon_iris, collapse = " or "),
                  entity = paste(entity_iris, collapse = " or "),
                  variable_only = variable_only)
  # TEST
  print(queryseq)

  res <- httr::GET(ontotrace_url, query = queryseq)
  stop_for_pk_status(res)
  out <- httr::content(res, as = "text")

  # write to a temporary file
  d <- tempfile()
  write(out, file = d)
  # parse
  nex <- nexml_read(d)

  unlink(d)
  # return the matrix
  get_characters(nex)
}


ontotrace_url <- "http://kb.phenoscape.org/api/ontotrace"
part_relation <- "<http://purl.obolibrary.org/obo/BFO_0000050> some " # "part of some"
develops_relation <- "<http://purl.obolibrary.org/obo/RO_0002202> some " # "develops from some"

#------------------------------#
#      Tests for RNeXML        #

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

