#' Obtains term frequencies for the Phenoscape KB
#'
#' Determines the frequencies for the given input list of terms, based on
#' the selected corpus.
#'
#' Depending on the corpus selected, the frequencies are queried directly
#' from pre-computed counts through the KB API, or are calculated based on
#' matching row counts obtained from query results. Currently, the Phenoscape KB
#' has precomputed counts for corpora "taxa" and "genes". Calculated counts for
#' the "taxon_annotations" corpus are most reliable for phenotype terms and their
#' subsumers. For entity terms, subsumers can include many generated
#' post-composed terms (such as "part_of some X", where X is, for example, an
#' anatomy term), and at least currently these aren't handled correctly by the
#' Phenoscape KB, resulting in counts of zero for such terms. For some of these
#' the implementation here will try to rewrite the query (see parameter
#' `decodeIRI`), but this only works to a limited extent.
#' @note
#' Term categories being accurate is vital for obtaining correct counts and
#' thus frequencies. Auto-determining term categories yields reasonably accurate
#' results, but with caveats. One, it can be time-consuming, and two, especially
#' for entity terms and their subsumers it is often not 100% accurate. This
#' function will try to correct for that by assuming that if not all terms
#' are determined to be of the same category, but one category holds for more
#' than 90% of the terms, that it must be the correct category for all terms.
#' If the list of terms is legitimately of different categories, it is best to
#' determine (and possibly correct) categories beforehand, and then pass the
#' result as `as`. If all terms are of the same category and the category is
#' known beforehand, it saves time and prevents potential errors to supply this
#' category using `as`.
#' @param x a vector or list of one or more terms, either as IRIs or as term
#'   objects.
#' @param as the category or categories of the input terms (see [term_category()]).
#'   Supported categories are "entity", "quality", and "phenotype". The value
#'   must either be a single category (applying to all terms), or a vector of
#'   categories (of same length as `x`). If provided as "auto" (or NULL), the
#'   category of each term is automatically determined. The default is "auto".
#' @param corpus the name of the corpus for which to determine frequencies.
#'   Supported values are "taxon_annotations", "taxa", "gene_annotations", and
#'   "genes". (At present, support for "gene_annotations" is pending support in
#'   the Phenoscape API.) The default is "taxon_annotations".
#' @param decodeIRI boolean. If TRUE (the default), attempt to decode
#'   post-composed entity IRIs, and under certain circumstances rewrite the
#'   count query according to the results. At present, this is used only for
#'   entity IRIs detected as "part_of some X" post-compositions, and only for
#'   the "taxon_annotations" corpus. In those cases, the count query will be
#'   rewritten to first query for X, then for X including parts, and the resulting
#'   count is the result of the latter minus that of the former.
#'
#'   The decoding algorithm may be imprecise, so one may want to turn this off,
#'   the result of which will usually be a frequency of zero for those IRIs, due
#'   to limitations in the Phenoscape KB API.
#' @param ... additional query parameters to be passed to the function querying
#'   for counts, see [pkb_args_to_query()]. Currently this is only used for
#'   corpus "taxon_annotations", and the only useful parameter is `includeRels`,
#'   which can be used to include historical and serial homologues in the counts.
#'   It can also be used to always include parts for entity terms.
#' @return a vector of frequencies as floating point numbers (between zero
#'   and 1.0), of the same length (and ordering) as the input list of terms.
#' @examples
#' terms <- c("pectoral fin", "pelvic fin", "dorsal fin", "paired fin")
#' IRIs <- sapply(terms, pk_get_iri, as = "anatomy")
#' term_freqs(IRIs)
#' 
#' phens <- get_phenotypes(entity = "basihyal bone")
#' term_freqs(phens$id, as = "phenotype", corpus = "taxon_annotations")
#' term_freqs(phens$id, as = "phenotype", corpus = "taxa")
#' 
#' @export
term_freqs <- function(x,
                       as = c("auto", "entity", "quality", "phenotype"),
                       corpus = c("taxon_annotations", "taxa", "gene_annotations", "genes"),
                       decodeIRI = TRUE,
                       ...) {
  as <- match.arg(as, several.ok = TRUE)
  corpus <- match.arg(corpus)

  if (as[1] == "auto") {
    as <- term_category(x)
    # check for signs of mistakes and auto-correct
    fracs <- table(as) / length(as)
    if (any(fracs < 0.1) && any(fracs > 0.9)) {
      majorType <- names(fracs)[fracs == max(fracs)]
      as[as != majorType] <- majorType
    }
  } else if (length(as) > 1 && length(as) != length(x))
    stop("'as' must be a single value, or have the same length as 'x'", call. = FALSE)
  else if (any(as == "auto"))
    stop("'auto' can only be applied to all terms", call. = FALSE)
  
  ctotal <- corpus_size(corpus = corpus)
  if (corpus == "taxa" || corpus == "genes") {
    if (any(as != "phenotype"))
      stop("corpus '", corpus, "' requires phenotype terms", call. = FALSE)
    corpusID <- paste0("http://kb.phenoscape.org/sim/", corpus)
    query <- list(terms = as.character(jsonlite::toJSON(x)),
                  corpus_graph = corpusID)
    freqs <- get_csv_data(pkb_api("/similarity/frequency"), query = query,
                          header = FALSE, row.names = 1, check.names = FALSE)
    reordering <- match(x, rownames(freqs))
    freqs <- freqs[reordering,] / ctotal
  } else if (corpus == "taxon_annotations") {
    freqs <- mapply(annotations_count,
                    iri = x, termType = as, decodeIRI = decodeIRI, ...)
    freqs <- freqs / ctotal
  } else {
    stop("corpus '", corpus, "' is currently unsupported", call. = FALSE)
  }
  unname(freqs)
}

#' @importFrom utils URLdecode
decode_entity_postcomp <- function(x) {
  res <- stringi::stri_match_all_regex(
    URLdecode(x),
    pattern = "<(https?://[A-Za-z0-9-_/.]+)>[ +]+some[ +]+<(https?://[A-Za-z0-9-_/.]+)>")
  lapply(res, function(m) {
    if (all(is.na(m[1,])))
      list(rels=c(), entities=c())
    else
      list(rels=m[,2], entities=m[,3])
  })
}

annotations_count <- function(iri, termType, decodeIRI = TRUE,
                              apiEndpoint = "/taxon/annotations",
                              ...) {
  query <- pkb_args_to_query(...)
  query$total <- TRUE
  if (termType == "entity" && decodeIRI) {
    comps <- decode_entity_postcomp(iri)[[1]]
    if ((length(comps$entities) == 1) && any(partOf_iri() %in% comps$rels)) {
      query[[termType]] <- comps$entities
    }
  }
  if (is.null(query[[termType]])) query[[termType]] <- iri
  res <- get_json_data(pkb_api(apiEndpoint), query = query)
  # if the IRI used for counting is a result of decoding the IRI, _and_ if
  # we haven't included parts in the count already
  if ((query[[termType]] != iri) && (is.null(query$parts) || ! query$parts)) {
    # count with including parts, then subtract entities alone (counted before)
    query$parts <- TRUE
    res2 <- get_json_data(pkb_api(apiEndpoint), query = query)
    res2$total - res$total
  } else
    res$total
}

#' Obtain the size of different corpora
#'
#' Obtains the size of a certain number of predefined corpora. The total size
#' of a corpus is important for calculating term frequencies.
#'
#' Corpus sizes are cached per session after they have first been obtained.
#' Thus, if the Phenoscape KB changes, a session needs to be restarted to
#' have those changes be reflected.
#'
#' @param corpus the name of the corpus, currently one of "taxon_annotations",
#'   "taxa", "gene_annotations", and "genes". (At present "gene_annotations" is
#'   pending support by the Phenoscape API.) Unambiguous abbreviations are
#'   acceptable.
#' @return the size of the specified corpus as an integer number.
#' @examples
#' corpus_size("taxa")
#' corpus_size("taxon_annotations")
#' @export
corpus_size <- local({
  .sizes <- list()
  function(corpus = c("taxon_annotations", "taxa", "gene_annotations", "genes")) {
    corpus <- match.arg(corpus)
    if (is.null(.sizes[[corpus]])) {
      if (corpus == "taxa" || corpus == "genes") {
        corpusID <- paste0("http://kb.phenoscape.org/sim/", corpus)
        res <- get_json_data(pkb_api("/similarity/corpus_size"),
                             query = list(corpus_graph = corpusID))
        .sizes[[corpus]] <- res$total
      } else if (corpus == "taxon_annotations") {
        res <- get_json_data(pkb_api("/taxon/annotations"), list(total = TRUE))
        .sizes[[corpus]] <- res$total
      } else {
        stop("corpus 'gene_annotations' is currently unsupported", call. = FALSE)
      }
    }
    .sizes[[corpus]]
  }
})
