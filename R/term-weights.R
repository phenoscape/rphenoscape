#' Obtains term frequencies for the Phenoscape KB
#'
#' Determines the frequencies for the given input list of terms, based on
#' the selected corpus.
#'
#' Depending on the corpus selected, the frequencies are queried directly
#' from pre-computed counts through the KB API, or are calculated based on
#' matching row counts obtained from query results. Currently, the Phenoscape KB
#' has precomputed counts for corpora "taxa" and "genes".
#' @note
#' Term categories being accurate is vital for obtaining correct counts and
#' thus frequencies. In earlier (<=0.2.x) releases, auto-determining term
#' category was an option, but this is no longer supported, in part because it
#' was potentially time consuming and sometimes inaccurate, in particular for
#' the many post-composed subsumer terms returned by [subsumer_matrix()]. In the
#' KB v2.0 API, auto-determining the category of a post-composed term is no
#' longer supported. If the list of terms is legitimately of different categories,
#' determine (and possibly correct) categories beforehand using [term_category()].
#' In earlier (<=0.2.x) releases the "taxon_annotations" corpus was supported, but
#' this is no longer the case due to the inability to determine an accurate
#' count for post-composed terms with the KB v2.0 API. This also means that the
#' only supported value for the `as` parameter is "phenotype" since "entity" and
#' "quality" were only supported for the "taxon_annotations" corpus.
#' @param x a vector or list of one or more terms, either as IRIs or as term
#'   objects.
#' @param as the category or categories of the input terms (see [term_category()]).
#'   Supported categories are "entity", "quality", and "phenotype". (At present,
#'   support for "entity" and "quality" has been disabled as of v0.3.0, pending full support from the KB API.)
#'   The value must either be a single category (applying to all terms), or a vector of
#'   categories (of same length as `x`). The default is "phenotype".
#' @param corpus the name of the corpus for which to determine frequencies.
#'   Supported values are "taxon_annotations", "taxa", "gene_annotations", and
#'   "genes". (At present, support for "gene_annotations" and "taxon_annotations" is disabled, pending support in
#'   the Phenoscape KB API.)
#'   The default is "taxa".
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
#' phens <- get_phenotypes(entity = "basihyal bone")
#' term_freqs(phens$id, as = "phenotype", corpus = "taxa")
#' term_freqs(phens$id, as = "phenotype", corpus = "states")
#' @export
term_freqs <- function(x,
                       as = c("phenotype", "entity"),
                       corpus = c("taxa", "states", "genes", "taxon_annotations", "gene_annotations"),
                       decodeIRI = FALSE,
                       ...) {
  as <- match.arg(as, several.ok = TRUE)
  corpus <- match.arg(corpus)
  if (decodeIRI) stop("Decoding an IRI is no longer supported.")
  if (length(as) > 1 && length(as) != length(x))
    stop("'as' must be a single value, or have the same length as 'x'", call. = FALSE)

  if (corpus == "taxa" || corpus == "states") {
    ctotal <- corpus_size(corpus = corpus)    
    if (any(as != "phenotype"))
      stop("corpus '", corpus, "' requires phenotype terms", call. = FALSE)
    ontology_terms_type <- as
    if (ontology_terms_type == "entity") {
      ontology_terms_type <- "anatomical_entity"
    }
    query <- list(terms = as.character(jsonlite::toJSON(x)),
                  corpus = corpus,
                  type = ontology_terms_type)
    freqs <- get_csv_data(pkb_api("/similarity/frequency"), query = query,
                          header = FALSE, row.names = 1, check.names = FALSE)
    reordering <- match(x, rownames(freqs))
    freqs <- freqs[reordering,] / ctotal
  } else {
    stop("corpus '", corpus, "' is currently unsupported", call. = FALSE)
  }
  unname(freqs)
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
#' @param corpus the name of the corpus, currently one of "taxa", or "states".
#' @return the size of the specified corpus as an integer number.
#' @examples
#' corpus_size("taxa")
#' corpus_size("states")
#' @export
corpus_size <- local({
  .sizes <- list()
  function(corpus = c("taxa", "states", "genes")) {
    corpus <- match.arg(corpus)
    res <- get_json_data(pkb_api("/similarity/corpus_size"),
                         query = list(corpus = corpus))
    res$total
  }
})
