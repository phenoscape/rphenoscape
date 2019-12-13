#' Get term details (ID, label, definition)
#'
#' Retrieve details about a taxon, an anatomical structure, a gene, or a phenotypic
#' quality.
#' @name pk_terms
#' @param term character, the query term, either as name or IRI. Names are looked
#'   up against taxonomies, anatomy ontologies, and PATO for `pk_taxon_detail`,
#'   `pk_anatomical_detail`, and `pk_phenotype_detail`, respectively.
#'
#'   For `pk_taxon_detail` this can also be a list (or vector) of terms (taxa).
#' @param taxon character, the NCBI taxon name or corresponding NCBITaxon
#'   ontology IRI for which to match the gene name.
#' @param verbose logical, whether informative messages should be printed. The
#'   default is `FALSE`.
#' @return A data.frame, with at least columns "id" and "label".
#'
#'   For `pk_taxon_detail`, additional columns are "extinct" (logical),
#'   "rank.id", "rank.label", and where available "common_name". The rows
#'   corresponding to taxon names that failed to be resolved to IRIs will be NA.
#'
#'   For `pk_anatomical_detail` and `pk_phenotype_detail`, the additional
#'   column is "definition".
#'
#'   For `pk_gene_detail`, the additional columns are "taxon.id" and "taxon.label"
#'   for the corresponding NCBI Taxonomy ID and name, and "matchType" ('exact'
#'   or 'partial').
#' @examples
#' pk_taxon_detail("Coralliozetus")
#' pk_anatomical_detail("basihyal bone")
#' pk_gene_detail("socs5")
#'
#' @export
#' @rdname pk_terms
pk_taxon_detail <- function(term, verbose=FALSE) {
  iriList <- sapply(term,
                    get_term_iri, as = "taxon",  verbose = verbose,
                    USE.NAMES = FALSE)
  if (length(iriList) == 1 && is.na(iriList)) return(invisible(NA))

  mssg(verbose, "Retrieving term details")
  det <- sapply(iriList[! is.na(iriList)],
                function(iri)
                  as.data.frame(get_json_data(pkb_api("/taxon"), list(iri = iri),
                                              ensureNames = "common_name"),
                                check.names = FALSE, stringsAsFactors = FALSE),
                USE.NAMES = FALSE)
  if (any(is.na(iriList))) {
    res <- matrix(nrow = length(iriList), ncol = nrow(det))
    colnames(res) <- row.names(det)
    res[is.na(iriList),] <- rep(NA, times = nrow(det))
    if (! all(is.na(iriList)))
      res[! is.na(iriList),] <- apply(det, 1, as.character)
  } else if (is.null(dim(det))) {
    warning("Failed to find term details for any of the input terms:\n\t",
            paste0(term, collapse = "\n\t"), call. = FALSE)
    return(invisible(NA))
  } else
    res <- apply(det, 1, function(x) ifelse(is.na(x), NA, as.character(x)))
  if (! is.matrix(res)) res <- t(res)
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  colnames(res) <- sub("@", "", names(res))
  res[, "extinct"] <- as.logical(res[, "extinct"])
  res
}

#' @export
#' @rdname pk_terms
pk_anatomical_detail <- function(term, verbose=FALSE) {
  pk_details(term, as = "anatomy", verbose = verbose)
}
#' @export
#' @rdname pk_terms
pk_phenotype_detail <- function(term, verbose=FALSE) {
  pk_details(term, as = "pato", verbose = verbose)
}

#' @export
#' @rdname pk_terms
pk_gene_detail <- function(term, taxon = NA, verbose=FALSE) {
  queryseq <- list(text = term)
  res <- get_json_data(pkb_api("/gene/search"), query = queryseq)
  res <- res$results
  colnames(res) <- sub("@", "", names(res))
  if (! is.na(taxon)) {
    taxcol <- "taxon.label"
    if (startsWith(taxon, "http")) taxcol <- "taxon.id"
    res <- res[res[, taxcol] == taxon,]
  }
  res
}

#' Determine which taxa are extinct
#'
#' This is simply a convenience function on top of [pk_taxon_detail()].
#' @param taxon character, the taxa or list of taxa, as names or IRIs. Names
#'   will first be looked up, and a warning will be issued for names that fail
#'   to be found as a taxon name. Names and IRIs can be intermixed.
#' @param verbose logical, whether or not to print informative messages for
#'   possibly time-consuming operations. The default is `FALSE`.
#' @return A logical named vector with value `TRUE` if the corresponding input
#'   taxon is marked as extinct, and FALSE otherwise. For taxon names that failed
#'   to be looked up, the value will be NA. Names will be the input taxa where
#'   there were given as names, and the label of the respective taxon otherwise.
#' @export
pk_is_extinct <- function(taxon, verbose = FALSE) {
  det <- pk_taxon_detail(taxon, verbose = verbose)
  if (all(is.na(det))) return(invisible(NA))
  res <- det$extinct
  names(res) <- ifelse(startsWith(taxon, "http"), det$label, taxon)
  res
}


pk_details <- function(term, as, verbose=FALSE) {
  iri <- get_term_iri(term, as, verbose = verbose)
  if (is.na(iri)) return(invisible(NA))

  mssg(verbose, "Retrieving term details")

  queryseq <- list(iri = iri)
  lst <- pk_GET("http://kb.phenoscape.org/api/term", queryseq)
  names(lst) <- sub("@", "", names(lst))
  as.data.frame(Filter(function(x) !is.list(x), lst))
}

#' Obtains the labels for a list of terms
#'
#' Attempts to obtain the label for each term, identified by IRI, in the input
#' list. Terms for which no label is found in the database will have NA as the
#' label in the result (see Value).
#' @param term_iris character, a list of term IRIs
#' @param preserveOrder logical, whether the resulting data frame (see Value)
#'   is to be in the same row order as `termIRIs`. The default is not to preserve
#'   order.
#' @param verbose logical, whether to print information about possibly
#'   time-consuming operations.
#' @return A data.frame with columns "id" and "label". The "id" column contains
#'   the IRIs. The label will be `NA` for term IRIs that are not present in the
#'   KB, or for which the KB cannot produce a label.
#' @export
get_term_label <- function(term_iris, preserveOrder = FALSE, verbose = FALSE) {
  if (length(term_iris) == 1) term_iris <- c(term_iris)
  queryseq <- list(iris = as.character(jsonlite::toJSON(term_iris)))
  res <- get_json_data(pkb_api("/term/labels"), query = queryseq)
  res <- res$results
  if (! is.data.frame(res)) {
    if (is.null(res$label)) res$label <- NA
    res <- as.data.frame(res, check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (length(res) > 0) {
    names(res) <- sub("@", "", names(res))
  }
  if (nrow(res) > 0) {
    noLabel <- is.na(res$label)
    if (any(noLabel)) {
      res[noLabel, "label"] <- sapply(res$id[noLabel], function(iri) {
        clInfo <- pk_class(iri, as = NA, verbose = verbose)
        if (length(clInfo) <= 1 || clInfo$label == iri)
          NA
        else
          clInfo$label
      },
      USE.NAMES = FALSE)
    }
    if (preserveOrder) {
      reordering <- match(term_iris, res$id)
      res <- res[reordering,]
    }
  }

  res
}

pk_taxon_url <- "http://kb.phenoscape.org/api/taxon"


