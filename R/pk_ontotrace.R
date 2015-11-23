#' Returns a NeXML-format evolutionary character matrix.
#'
#' @name pk_ontotrace
#' @import RNeXML
#' @import dplyr
#' @param taxon character: Required. A single character string or a vector of taxa.
#' @param entity characters: Required. A single character string or a vector of anatomical class expressions.
#' @param relation character string: Optional. Has to be either "part of" or "develops from". Default is "part of".
#' @param get_metadata logical: Optional. If TRUE the result will contain the metadata data.frame. Default is TRUE
#' @param variable_only logical: Optional. Default is TRUE.
#'
#' @return data.frame: The OntoTrace matrix.
#'
#' @description
#' Generate matrix of inferred presence/absence associations for anatomical structures
#' subsumed by the provided entity class expression, for any taxa within the provided
#' taxon class expression.
#' @examples
#' pk_ontotrace(taxon = "Ictalurus", entity = "fin")
#' pk_ontotrace(taxon = c("Ictalurus", "Ameiurus"), entity = "fin spine", get_metadata = TRUE)
#'
#'
#' @export
#' @rdname pk_ontotrace
pk_ontotrace <- function(taxon, entity, get_metadata = FALSE, relation = "part of", variable_only=TRUE) {
#   taxon_entity_list <- list(...)
#
#   if (length(taxon_entity_list$taxon) == 0 || length(taxon_entity_list$entity) == 0) {
#     stop("please explicitly specify taxon and entity parameter,
#          e.g. pk_ontotrace(taxon = \"Ictalurus\", entity = \"fin\"),
#          or pk_ontotrace(taxon = c(\"Ictalurus\", \"Ameiurus\"), entity = c(\"fin\", \"spine\"))",
#          call. = FALSE)
#   }

  tryCatch(
    relation_type <- match.arg(tolower(relation), c("part of", "develops from")),
    error = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    })

  relation_id <- switch(relation_type,
                        "part of" = part_relation,
                        "develops from" = develops_relation)

  taxon_iris <- lapply(taxon, FUN = pk_get_iri, as = "vto", verbose = FALSE)
  entity_iris <- lapply(entity, FUN = pk_get_iri, as = "uberon", verbose = FALSE)

  # FALSE will be returned by pk_get_iri if there's no match in database
  if (FALSE %in% taxon_iris || FALSE %in% entity_iris) {
    stop(paste(c("Could not find",
                 taxon[which(taxon_iris == FALSE)],
                 entity[which(entity_iris == FALSE)],
                 "in the database."),
               collapse = " * "),
         call. = FALSE)
  }

  # insert necessary "<" and ">" before concatenating string
  taxon_iris <- lapply(taxon_iris, FUN = function(x) paste0("<", x, ">"))
  entity_iris <- lapply(entity_iris, FUN = function(x) paste0("(", relation_id, quantifier, "<", x, ">)"))

  queryseq = list(taxon = paste(taxon_iris, collapse = " or "),
                  entity = paste(entity_iris, collapse = " or "),
                  variable_only = variable_only)

  res <- httr::GET(ontotrace_url, query = queryseq)
  stop_for_pk_status(res)
  out <- httr::content(res, as = "parsed")

  nex <- nexml_read(out)

  m <- get_characters(nex, rownames_as_col = TRUE, otu_id = get_metadata, otus_id = get_metadata) # returned data.frame with taxon names as row index
  #m_re <- dplyr::as_data_frame(cbind(lcol, rcol, stringsAsFactors = FALSE))
  m_re <- dplyr::as_data_frame(m)

  if (get_metadata == TRUE) {
    id_taxa <- get_taxa(nex)
    id_taxa_meta <- get_metadata(nex, "otu")


    id_taxa <- (id_taxa_meta
                %>% filter(rel == meta_attr_taxon)
                %>% inner_join(id_taxa, by = c("otu" = "otu"))
                %>% select(label, href, otu, otus.x)
                %>% rename(otus = otus.x))

    id_entities <- get_level(nex, "characters/format/char")
    id_entities_meta <- get_metadata(nex, level="characters/format/char")

    id_entities <- (id_entities_meta
                    %>% filter(rel == meta_attr_entitiy)
                    %>% inner_join(id_entities, by = c("char" = "char"))
                    %>% select(label, href, char))

    m_re <- list(matrix = m_re,
                 id_taxa = id_taxa,
                 id_entities = id_entities
                 )
  }
  return(m_re)
}


ontotrace_url <- "http://kb.phenoscape.org/api/ontotrace"
quantifier <- " some " # seperate quantifier
part_relation <- "<http://purl.obolibrary.org/obo/BFO_0000050>" # "part of"
develops_relation <- "<http://purl.obolibrary.org/obo/RO_0002202>" # "develops from"
meta_attr_taxon <- "dwc:taxonID"
meta_attr_entitiy <- "obo:IAO_0000219"

#------------------------------#
#      Tests for RNeXML        #

test_read_ns <- function() {
  #nex <- nexml_read(paste0(path, "test_original.xml"))
  nex <- nexml_read(path)
  get_characters(nex) # to keep
}

test_read_nex <- function() {
  nex <- nexml_read(path)
  nex
}

test_validate_ns <- function() {
  #nexml_validate(paste0(path, "test_original.xml"))
}

test_validate_nex <- function() {
  #nexml_validate(paste0(path, "test.xml"))
}

#path <- paste0(system.file(package = "rphenoscape"), "/examples/")
path <- "https://raw.githubusercontent.com/phenoscape/rphenoscape/char-annots-example/inst/examples/ontotrace-result.xml"
