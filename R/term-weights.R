#' Obtains term frequencies for the Phenoscape KB
#'
#' Determines the frequencies for the given input list of terms, based on
#' the selected corpus and the type of the terms.
#'
#' Depending on the corpus selected, the frequencies are queried directly
#' from pre-computed counts through the KB API, or are calculated based on
#' matching row counts obtained from query results. Currently, the Phenoscape KB
#' has precomputed counts for corpora "annotated-taxa","taxon-variation", "genes" and "states".
#'
#' @note
#' Term categories being accurate is vital for obtaining correct counts and
#' thus frequencies. In earlier (<=0.2.x) releases, auto-determining term
#' category was an option, but this is no longer supported, in part because it
#' was potentially time consuming and often inaccurate, in particular for
#' the many post-composed subsumer terms returned by [subsumer_matrix()]. In the
#' KB v2.0 API, auto-determining the category of a post-composed term is no
#' longer supported. If the list of terms is legitimately of different categories,
#' determine (and possibly correct) categories beforehand using [term_category()].
#' 
#' In earlier (<=0.2.x) releases the "taxon_annotations" corpus was supported, albeit
#' it's implementation was very slow because it relied on potentially multiple individudal
#' KB API queries for each term, and this in turn relied on the ability to break down
#' post-composed expressions into their component terms and expressions, which is (at least
#' currently) no longer possible. Once the KB API directly supports this corpus, it
#' can be enabled here again.
#' @param x a vector or list of one or more terms, either as IRIs or as term
#'   objects.
#' @param as the category or categories (a.k.a. type) of the input terms (see [term_category()]).
#'   Possible values are "anatomical_entity" (synonymous with "entity"), "quality", and
#'   "phenotype". Unambiguous abbreviations are acceptable. The value must either be a
#'   single category (applying to all terms), or a vector of categories (of same length as `x`).
#'   The default is "phenotype".
#' 
#'   Note that at present, support by the KB API for "quality" remains pending and has thus been
#'   disabled as of v0.3.0. Also, mixing different categories of terms is not yet supported, and
#'   doing so will thus raise an error.
#' @param corpus the name of the corpus for determining how to count, currently one of the following:
#'   - "taxon_annotations" (counts phenotype annotations to character states and thus taxa),
#'   - "states" (counts character states),
#'   - "taxon-variation" (counts taxa among variation profiles),
#'   - "annotated-taxa" (counts taxa with annotations),
#'   - "gene_annotations" (counts phenotype annotations to genes), and
#'   - "genes" (counts genes)
#'   
#'   Unambiguous abbreviations of corpus names are acceptable. The default is "taxon-variation".
#'   Note that at present "taxon_annotations" and "gene_annotations" are not yet
#'   supported by the KB API and will thus result in an error.
#'
#'   Note that previously "taxa" was allowed as a corpus, but is no longer supported.
#'   The "taxon-variation" corpus is the equivalent of the deprecated "taxa" corpus.
#' @param decodeIRI boolean. This parameter is deprecated (as of v0.3.x) and must be set
#'  to FALSE (the default). If TRUE is passed an error will be raised. In v0.2.x
#'  when TRUE this parameter would attempt to decode post-composed entity IRIs.
#'  Due to changes in the IRI returned by the Phenoscape KB v2.x API decoding
#'  post-composed entity IRIs is no longer possible. Prior to v0.3.x, the default
#'  value for this parameter was TRUE.
#' @param ... additional query parameters to be passed to the function querying
#'   for counts, see [pkb_args_to_query()]. This is currently (as of v0.3.0) not
#'   used.
#' @return a vector of frequencies as floating point numbers (between zero
#'   and 1.0), of the same length (and ordering) as the input list of terms.
#' @examples
#' phens <- get_phenotypes(entity = "basihyal bone")
#' # see which phenotypes we have:
#' phens$label
#' # frequencies by counting taxa:
#' freqs.t <- term_freqs(phens$id, as = "phenotype", corpus = "taxon-variation")
#' freqs.t
#' # we can convert this to absolute counts:
#' freqs.t * corpus_size("taxon-variation")
#' # frequencies by counting character states:
#' freqs.s <- term_freqs(phens$id, as = "phenotype", corpus = "states")
#' freqs.s
#' # and as absolute counts:
#' freqs.s * corpus_size("states")
#' # we can compare the absolute counts by computing a ratio
#' freqs.s * corpus_size("states") / (freqs.t * corpus_size("taxon-variation"))
#' @export
term_freqs <- function(x,
                       as = c("phenotype", "entity", "anatomical_entity", "quality"),
                       corpus = c("taxon-variation", "annotated-taxa", "taxon_annotations", "gene_annotations", "genes", "states"),
                       decodeIRI = FALSE,
                       ...) {
  as <- match.arg(as, several.ok = TRUE)
  corpus <- match.arg(corpus)
  if (decodeIRI) stop("Decoding an IRI is no longer supported.")
  if (length(as) > 1 && length(as) != length(x))
    stop("'as' must be a single value, or have the same length as 'x'", call. = FALSE)
  if (length(unique(as)) != 1)
    stop("'as' currently requires all values to be the same", call. = FALSE)
  if (corpus == "annotated-taxa" || corpus == "taxon-variation" || corpus == "genes" || corpus == "states") {
    ctotal <- corpus_size(corpus = corpus)
    # for now 'as' must contain the same value so use the first one since
    # /similarity/frequency type field only allows one value
    ontology_terms_type <- as[1]
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
#' of a corpus is important for calculating term frequencies. That is, for a given
#' corpus, the possible range for any term frequency is between 0 and the corpus size.
#'
#' Corpus sizes are cached per session after they have first been obtained.
#' Thus, if the Phenoscape KB changes, a session needs to be restarted to
#' have those changes be reflected.
#'
#' @param corpus the name of the corpus, currently one of the following:
#'   - "taxon_annotations" (counts phenotype annotations to character states and thus taxa),
#'   - "states" (counts character states),
#'   - "taxon-variation" (counts taxa among variation profiles),
#'   - "annotated-taxa" (counts taxa with annotations),
#'   - "gene_annotations" (counts phenotype annotations to genes), and
#'   - "genes" (counts genes)
#'   
#'   Note that at present "gene_annotations" is not yet supported by the Phenoscape API.
#'   Unambiguous abbreviations of corpus names are acceptable.
#'
#'   Note that previously "taxa" was allowed as a corpus, but is no longer supported.
#'   The "taxon-variation" corpus is the equivalent of the deprecated "taxa" corpus.
#' @return The total size of the specified corpus as an integer number.
#' @examples
#' corpus_size("taxon-variation")
#' corpus_size("annotated-taxa")
#' corpus_size("states")
#' corpus_size("genes")
#' @export
corpus_size <- local({
  .sizes <- list()
  function(corpus = c("taxon_annotations", "taxon-variation", "annotated-taxa", "gene_annotations", "genes", "states")) {
    corpus <- match.arg(corpus)
    if (is.null(.sizes[[corpus]])) {
      if (corpus == "taxon-variation" || corpus == "annotated-taxa" || corpus == "genes"|| corpus == "states") {
        res <- get_json_data(pkb_api("/similarity/corpus_size"),
                             query = list(corpus = corpus))
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
