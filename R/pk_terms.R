#' Get term details (ID, label, definition)
#'
#' @name pk_terms
#' @param term character. The term to be searched.
#' @param verbose logical: optional. If TRUE (default), informative messages printed.
#' @return A data.frame with term id, label, and definition
#' @description
#' Retrieve details about a taxon, an anatomical structure, a gene, or a phenotype.
#'
#' @examples
#' pk_taxon_detail("Coralliozetus")
#' pk_anatomical_detail("basihyal bone")
#' pk_gene_detail("socs5")
#'
#' @export
#' @rdname pk_terms
pk_taxon_detail <- function(term, verbose=FALSE) {
  #pk_details(term, as = "vto", verbose)
  # update to a verbose version of taxon search
  iri <- pk_get_iri(term, as = "taxon", nomatch = FALSE)
  if (iri == FALSE) return(invisible(NA))
  mssg(verbose, "Retrieving term details")

  queryseq <- list(iri = iri)
  lst <- unlist(pk_GET(pk_taxon_url, queryseq))
  det <- as.data.frame(t(lst))
  colnames(det) <- sub("@", "", names(lst))
  det
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
pk_gene_detail <- function(term, verbose=FALSE) {
  # TODO: resolve taxon to NCBI IRI
  queryseq <- list(text = term)
  res <- pk_GET("http://kb.phenoscape.org/api/gene/search", query = queryseq)
  res$results
}

#' Test if a taxon is extinct.
#'
#' @param taxon character, the taxon name to be tested.
#' @return logical, TRUE if extinct, FALSE if not
#' @export
pk_is_extinct <- function(taxon) {
  det <- pk_taxon_detail(taxon)
  if (is.na(det)) return(invisible(NA))
  det$extinct
}


pk_details <- function(term, as, verbose=FALSE) {
  iri <- pk_get_iri(term, as, verbose = verbose)
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
#' @return A data.frame with columns "id" and "label".
#' @export
get_term_label <- function(term_iris, preserveOrder = FALSE, verbose = FALSE) {
  if (length(term_iris) == 1) {
    queryseq <- list(iri = term_iris[[1]])
    res <- get_json_data(pkb_api("/term/label"), query = queryseq)
    if (identical(res$`@id`, res$label))
      res <- data.frame(id = term_iris, label = c(NA), stringsAsFactors = FALSE)
    else
      res <- as.data.frame(res, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    queryseq <- list(iris = paste0(term_iris, collapse = ","))
    res <- get_json_data(pkb_api("/term/labels"), query = queryseq)
    res <- res$results
  }
  if (length(res) > 0) {
    names(res) <- sub("@", "", names(res))
  }
  # /term/labels fails to produce a label for some terms where /term/label can,
  # so try to fill in labels not previously provided, and warn where that fails
  if (length(term_iris) > 1 &&
      (length(res) == 0 || nrow(res) < length(term_iris))) {
    if (length(res) == 0)
      iriMap <- rep(NA, times = length(term_iris))
    else
      iriMap <- match(term_iris, res$id)
    unmatched <- data.frame(id = term_iris[is.na(iriMap)])
    unmatched <- cbind(unmatched, label = sapply(unmatched$id, term_label))
    if (any(is.na(unmatched)))
      warning("Failed to find label for following input IRIs, substituting NA:\n\t",
              paste0(unmatched$id[is.na(unmatched$label)], collapse = "\n\t"),
              call. = FALSE)
    res <- rbind(res, unmatched)
  }
  if (preserveOrder && nrow(res) > 0) {
    reordering <- match(term_iris, res$id)
    res <- res[reordering,]
  }

  res
}

term_label <- function(term_iri) {
  res <- get_json_data(pkb_api("/term/label"), query = list(iri = term_iri))
  if (identical(res$`@id`, res$label))
    NA
  else
    res$label
}

pk_taxon_url <- "http://kb.phenoscape.org/api/taxon"


