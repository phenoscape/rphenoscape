#' Obtains term frequencies for the Phenoscape KB
#'
#' Determines the frequencies for the given input list of terms, based on
#' the selected corpus.
#'
#' Depending on the corpus selected, the frequencies are queried directly
#' from the Phenoscape API, or calculated based on query results. Currently,
#' the Phenoscape KB has precomputed frequencies for corpora "taxa" and
#' "genes".
#'
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
                       corpus = c("taxon_annotations", "taxa", "gene_annotations", "genes")) {
  as <- match.arg(as, several.ok = TRUE)
  corpus <- match.arg(corpus)

  if (as[1] == "auto")
    as <- term_category(x)
  else if (length(as) > 1 && length(as) != length(x))
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
    freqs <- mapply(function(iri, param) {
                      query <- list(total = TRUE)
                      query[[param]] <- iri
                      res <- get_json_data(pkb_api("/taxon/annotations"), query = query)
                      res$total
                    },
                    iri = x, param = as)
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
