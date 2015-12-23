#' Returns a NeXML-format evolutionary character matrix.
#'
#' @name pk_ontotrace
#' @import RNeXML
#' @import dplyr
#' @param nex, a nexml object
#'
#' @return data.frame: The OntoTrace matrix.
#'
#' @description
#' Generate matrix of inferred presence/absence associations for anatomical structures
#' subsumed by the provided entity class expression, for any taxa within the provided
#' taxon class expression.
#' @examples
#' \dontrun{
#' nex0 <- pk_get_ontotrace_xml(taxon = "Ictalurus", entity = "fin")
#'
#' nex <- pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus"), entity = "fin spine")
#' pk_get_ontotrace(nex)
#' pk_get_ontotrace_meta(nex)
#' }
#' @export
#' @rdname pk_ontotrace
pk_get_ontotrace <- function(nex) {

  m <- get_characters(nex, rownames_as_col = TRUE,
                      otu_id = TRUE, otus_id = TRUE)
  m_re <- dplyr::as_data_frame(m)
  return(m_re)
}

#' @export
#' @rdname pk_ontotrace
pk_get_ontotrace_meta <- function(nex) {

  # NULLing out : for the R CMD CHECK
  rel <- label <- href <- otu <- otus.x <- char <- NULL

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

  m_re <- list(id_taxa = id_taxa,
               id_entities = id_entities)

  return(m_re)
}


meta_attr_taxon <- "dwc:taxonID"
meta_attr_entitiy <- "obo:IAO_0000219"

#------------------------------#
#      Tests for RNeXML        #


test_read_nex <- function(path = path0) {
  nex <- nexml_read(path)
  nex
}


path0 <- "https://raw.githubusercontent.com/phenoscape/rphenoscape/char-annots-example/inst/examples/ontotrace-result.xml"
