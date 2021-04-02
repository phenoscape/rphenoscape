#' Get term details (ID, label, definition)
#'
#' Retrieve details about a taxon, an anatomical structure, a gene, or a phenotypic
#' quality.
#' @name terms
#' @param term character, the query term, either as name or IRI. Names are looked
#'   up against taxonomies, anatomy ontologies, and PATO for `taxon_info`,
#'   `anatomy_term_info`, and `phenotypic_quality_term_info`, respectively.
#'
#'   For `taxon_info` this can also be a list (or vector) of terms (taxa).
#' @param taxon character, the NCBI taxon name or corresponding NCBITaxon
#'   ontology IRI for which to match the gene name.
#' @param verbose logical, whether informative messages should be printed. The
#'   default is `FALSE`.
#' @return A data.frame, with at least columns "id" and "label".
#'
#'   For `taxon_info`, additional columns are "extinct" (logical),
#'   "rank.id", "rank.label", and where available "common_name". The rows
#'   corresponding to taxon names that failed to be resolved to IRIs will be NA.
#'
#'   For `anatomy_term_info` and `phenotypic_quality_term_info`, the additional
#'   column is "definition".
#'
#'   For `gene_info`, the additional columns are "taxon.id" and "taxon.label"
#'   for the corresponding NCBI Taxonomy ID and name, and "matchType" ('exact'
#'   or 'partial').
#' @examples
#' taxon_info("Coralliozetus")
#' anatomy_term_info("basihyal bone")
#' gene_info("socs5")
#'
#' @export
#' @rdname terms
taxon_info <- function(term, verbose=FALSE) {
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
#' @rdname terms
anatomy_term_info <- function(term, verbose=FALSE) {
  term_info(term, as = "anatomy", verbose = verbose)
}
#' @export
#' @rdname terms
phenotypic_quality_term_info <- function(term, verbose=FALSE) {
  term_info(term, as = "pato", verbose = verbose)
}

#' @export
#' @rdname terms
gene_info <- function(term, taxon = NA, verbose=FALSE) {
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
#' This is simply a convenience function on top of [taxon_info()].
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
is_extinct <- function(taxon, verbose = FALSE) {
  det <- taxon_info(taxon, verbose = verbose)
  if (all(is.na(det))) return(invisible(NA))
  res <- det$extinct
  names(res) <- ifelse(startsWith(taxon, "http"), det$label, taxon)
  res
}


term_info <- function(term, as, verbose=FALSE) {
  iri <- get_term_iri(term, as, verbose = verbose)
  if (is.na(iri)) return(invisible(NA))

  mssg(verbose, "Retrieving term details")

  queryseq <- list(iri = iri)
  lst <- get_json_data(pkb_api("/term"), queryseq)
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
        clInfo <- term_classification(iri, as = NA, verbose = verbose)
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

### terminfo class funcs

#' @export
as.terminfo <- function(x, withClassification = FALSE, ...) {
  UseMethod("as.terminfo", x)
}

#' @export
as.terminfo.default <- function(x, ...) {
  res <- lapply(x, function(elem) {
    if (is.terminfo(elem))
      elem
    else {
      terminfo(elem, ...)
    }
  })
  if (length(res) == 1)
    res[[1]]
  else
    res
}

#' @export
as.terminfo.data.frame <- function(x, ...) {
  if (is.null(x$id)) stop("data frame must have 'id' column containing IRI", call. = FALSE)
  as.terminfo(x$id, ...)
}

#' @export
is.terminfo <- function(x) {
  inherits(x, "terminfo")
}

#' @export
terminfo <- function(iri, withClassification = FALSE) {
  stopifnot(is.character(iri))
  res <- get_json_data(pkb_api("/term"),
                       query = list(iri = iri),
                       forceGET = TRUE)
  if (is.null(res$label)) {
    labelres <- get_json_data(pkb_api("/term/label"),
                         query = list(iri = iri),
                         forceGET = TRUE)
    res$label <- labelres$label
  }
  if (withClassification) {
    res$classification <- get_json_data(pkb_api("/term/classification"),
                                        query = list(iri = iri),
                                        forceGET = TRUE)
  }
  res <- rclean_jsonld_names(res)
  structure(res, class = c("terminfo", class(res)))
}

#' @export
print.terminfo <- function(x, ...) {
  cat("terminfo '", if (is.null(x$label)) x$id else x$label, "'\n", sep = "")
  if (x$definition != "") {
    cat("Definition: ", x$definition, "\n", sep = "")
  }
  if (length(x$synonyms) > 0) {
    cat("Synonyms:\n    ",
        paste(x$synonyms$value, collapse = "\n    ", sep = ""), 
        "\n",
        sep = ""
    )
  }
  if (length(x$relationships) > 0) {
    cat("Relationships:\n    ",
        paste(x$relationships$property.label, " ", x$relationships$value.label, collapse = "\n    ", sep = ""),
        "\n",
        sep = ""
    )
  }
  if (length(x$classification) > 0) {
    if (length(x$classification$subClassOf) > 0) {
      cat("Subclass of:\n    ",
          paste(x$classification$subClassOf$label, collapse = "\n    ", sep = ""),
          "\n",
          sep = ""
      )
    }
    if (length(x$classification$superClassOf) > 0) {
      cat("Superclass of:\n    ",
          paste(x$classification$superClassOf$label, collapse = "\n    ", sep = ""),
          "\n",
          sep = ""
      )
    }
    if (length(x$classification$equivalentTo) > 0) {
      cat("Equivalent to:\n    ",
          paste("  ", x$classification$equivalentTo$label, collapse = "\n    ", sep = ""),
          "\n",
          sep = ""
      )
    }
  }
  invisible(x)
}
