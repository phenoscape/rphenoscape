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
#' Phenoscape KB, resulting in counts of zero for such terms.
#' @note
#' Term categories being accurate is vital for obtaining correct counts and
#' thus frequencies. In earlier (<=0.2.x) releases, auto-determining term
#' category was an option, but this is no longer supported, in part because it
#' was potentially time consuming and sometimes inaccurate, in particular for
#' the many post-composed subsumer terms returned by [subsumer_matrix()]. In the
#' KB v2.0 API, auto-determining the category of a post-composed term is no
#' longer supported. If the list of terms is legitimately of different categories,
#' determine (and possibly correct) categories beforehand using [term_category()].
#' @param x a vector or list of one or more terms, either as IRIs or as term
#'   objects.
#' @param as the category or categories of the input terms (see [term_category()]).
#'   Supported categories are "entity", "quality", and "phenotype". The value
#'   must either be a single category (applying to all terms), or a vector of
#'   categories (of same length as `x`). The default is "entity".
#' @param corpus the name of the corpus for which to determine frequencies.
#'   Supported values are "taxon_annotations", "taxa", "gene_annotations", and
#'   "genes". (At present, support for "gene_annotations" is pending support in
#'   the Phenoscape API.) The default is "taxon_annotations".
#' @param decodeIRI boolean. This parameter is deprecated (as of v0.3.x) and must be set
#'  to FALSE (the default). If TRUE is passed an error will be raised. In v0.2.x
#'  when TRUE this parameter would attempt to decode post-composed entity IRIs.
#'  Due to changes in the IRI returned by the Phenoscape KB v2.x API decoding
#'  post-composed entity IRIs is no longer possible. Prior to v0.3.x, the default
#'  value for this parameter was TRUE.
#' @param ... additional query parameters to be passed to the function querying
#'   for counts, see [pkb_args_to_query()]. Currently this is only used for
#'   corpus "taxon_annotations", and the only useful parameter is `includeRels`,
#'   which can be used to include historical and serial homologues in the counts.
#'   It can also be used to always include parts for entity terms.
#' @return a vector of frequencies as floating point numbers (between zero
#'   and 1.0), of the same length (and ordering) as the input list of terms.
#' @examples
#' terms <- c("pectoral fin", "pelvic fin", "dorsal fin", "paired fin")
#' IRIs <- sapply(terms, get_term_iri, as = "anatomy")
#' term_freqs(IRIs, as = "entity")
#' 
#' phens <- get_phenotypes(entity = "basihyal bone")
#' term_freqs(phens$id, as = "phenotype", corpus = "taxon_annotations")
#' term_freqs(phens$id, as = "phenotype", corpus = "taxa")
#' 
#' @export
term_freqs <- function(x,
                       as = c("entity", "quality", "phenotype"),
                       corpus = c("taxon_annotations", "taxa", "gene_annotations", "genes"),
                       decodeIRI = FALSE,
                       ...) {
  as <- match.arg(as, several.ok = TRUE)
  corpus <- match.arg(corpus)
  if (decodeIRI) stop("Decoding an IRI is no longer supported.")
  if (length(as) > 1 && length(as) != length(x))
    stop("'as' must be a single value, or have the same length as 'x'", call. = FALSE)

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
                    iri = x, termType = as, ...)
    freqs <- freqs / ctotal
  } else {
    stop("corpus '", corpus, "' is currently unsupported", call. = FALSE)
  }
  unname(freqs)
}

annotations_count <- function(iri, termType,
                              apiEndpoint = "/taxon/annotations",
                              ...) {
  query <- pkb_args_to_query(...)
  query$total <- TRUE
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
