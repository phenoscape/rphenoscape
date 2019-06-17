#' Obtains a subsumer matrix
#'
#' A subsumer matrix M for terms \eqn{j \in \{1, \dots, n\}}{j in {1, ..., n}}
#' has value \eqn{M_{i,j}=1}{M[i,j] = 1} iff class _i_ (which can be an anonymous class expression) subsumes term _j_, and zero
#' otherwise. Therefore, it will have _n_ columns, one for each term.
#'
#' In this implementation, for each row _i_
#' \eqn{\sum_{j=1}^{n}M_{i,j} > 0}{sum(M[i, 1:n] > 0}. That is, each row
#' will have at least one non-zero value, which means that the number of classes
#' _not_ subsuming a term will be highly incomplete, because the (usually
#' very many) classes not subsuming any of the terms will not be included. This
#' subsumer matrix is thus only useful for similarity metrics for which
#' non-subsuming terms can be ignored.
#'
#' @param terms character, the list of terms for which to compute the dependency
#'   matrix. Can be given as term IRIs or term labels, and the list can contain
#'   both. Terms given as labels will first be resolved to IRIs, assuming they
#'   are from an anatomy ontology.
#' @param .colnames character, how to name the columns of the resulting
#'   matrix.
#'   - `"ID"` (the default): use the term IDs (the last component of the
#'     term IRIs).
#'   - `"IRI"`: use the term IRIs.
#'   - `"label"`: use the terms' labels (see `.labels` parameter).
#' @param .labels character, the labels for terms where known. Only used if
#'   `.colnames = "label"`. If NULL (the default), labels will be looked up if `terms`
#'   are provided as IRIs; elements of the `terms` list that are not in IRI form
#'   are assumed to be the label. If a list, must have the same length and ordering
#'   as `terms`; any NA elements will be looked up (from the corresponding term
#'   IRI).
#' @param preserveOrder logical, whether to return columns in the same
#'   order as `terms`. The default is not to preserve the order.
#' @param verbose logical, whether to print informative messages about certain
#'   potentially time-consuming operations.
#' @return A data.frame representing the subsumer matrix 
#'
#'   The matrix will have additional attributes depending on the choice of how to
#'   name rows and columns. If `.colnames = "ID"` (the default), the matrix will have
#'   an attribute `prefixes` giving the URL prefixes removed from the term IRIs
#'   to yield the IDs, in the order of the rows. If `.colnames = "label"`, it will
#'   have attribute `term.iris`, giving the term IRIs for the rows (and columns).
#'   Note that these extra attributes will be lost upon subsetting the returned
#'   matrix.
#' @examples
#' \dontrun{
#' tl <- c("http://purl.obolibrary.org/obo/UBERON_0000981",
#'         "http://purl.obolibrary.org/obo/UBERON_0002103",
#'         "http://purl.obolibrary.org/obo/UBERON_0000976",
#'         "http://purl.obolibrary.org/obo/UBERON_0002102")
#' m <- subsumer_matrix(tl)
#' m <- # term IDs as column names
#' id_prefixes <- attr(m, "prefixes")
#' id_prefixes # 4x "http://purl.obolibrary.org/obo/"
#'
#' m <- subsumer_matrix(tl, .colnames = "label")
#' m # term labels as column names
#' mat_terms <- attr(m, "term.iris")
#' mat_terms # term IRIs in the same order as columns
#' }
#' @importFrom stringi stri_match_first_regex
#' @export
subsumer_matrix <- function(terms,
                            .colnames = c("ID", "IRI", "label"), .labels = NULL,
                            preserveOrder = FALSE,
                            verbose = FALSE) {
  .colnames <- match.arg(.colnames)
  if (.colnames == "label" && is.null(.labels)) {
    .labels <- ifelse(startsWith(terms, "http://") | startsWith(terms, "https://"),
                      NA,
                      terms)
  }
  term_iris <- 
    unname(sapply(terms,
                  function(x) pk_get_iri(x, as = "anatomy",
                                         exactOnly = TRUE, verbose = verbose)))
  if (any(is.na(term_iris))) {
    warnings()
    stop("Could not resolve all term names to IRIs.", call. = FALSE)
  }
  queryseq <- list(terms = as.character(jsonlite::toJSON(term_iris)))
  m <- get_csv_data(pkb_api("/similarity/matrix"), query = queryseq,
                    row.names = 1, header = TRUE, check.names = FALSE,
                    verbose = verbose)
  if (preserveOrder) {
    reordering <- match(term_iris, colnames(m))
    m <- m[, reordering]
  }
  if (.colnames == "ID") {
    parts <- stringi::stri_match_first_regex(colnames(m), "(^.+[/#])(.+$)")
    colnames(m) <- parts[,3]
    attr(m, "prefixes") <- parts[,2]
  } else if (.colnames == "label") {
    # do we need to fill in any labels?
    if (any(is.na(.labels))) {
      query_iris <- terms[is.na(.labels)]
      lbls <- get_term_label(query_iris, preserveOrder = TRUE, verbose = verbose)
      lbls <- ifelse(is.na(lbls$label), query_iris, lbls$label)
      .labels[is.na(.labels)] <- lbls
    }
    nameMap <- match(colnames(m), term_iris)
    attr(m, "term.iris") <- colnames(m)
    colnames(m) <- .labels[nameMap]
  }
  m
}

#' Compute semantic similarity metrics between terms
#' 
#' @description
#' The Tanimoto similarity ST is computed according to the definition for bit vectors
#' (see [Jaccard index at Wikipedia](https://en.wikipedia.org/wiki/Jaccard_index#Tanimoto's_definitions_of_similarity_and_distance)).
#' For weights \eqn{W_i \in \{0, 1\}}{W[i] in {0, 1}} it is the same as the
#' Jaccard similarity.
#' The Tanimoto similarity can be computed for any term vectors, but for 1 - ST
#' to be a proper distance metric satisfying the triangle inequality,
#' \eqn{M_{i,j} \in \{0, W_i\}}{M[i,j] in {0, W[i]}} must hold.
#'
#' @param subsumer_mat  matrix or data.frame, the vector-encoded matrix M of
#'   subsumers for which \eqn{M_{i,j} = W_i, W_i > 0}{M[i,j] = W[i], with W[i] > 0} (W = weights),
#'   if class _i_ subsumes term j, and 0 otherwise. A binary
#'   (\eqn{M_{i,j} \in \{0, 1\}}{M[i,j] in {0, 1}}) encoding (i.e., W\[_i_\] = 1)
#'   can be obtained from [subsumer_matrix()].
#' @param terms character, optionally the list of terms (as IRIs and/or labels)
#'   for which to generate a properly encoded subsumer matrix on the fly.
#' @param ... parameters to be passed on to [subsumer_matrix()]
#'   if a subsumer matrix is to be generated on the fly.
#' @return A matrix with M\[i,j\] = similarity of terms _i_ and _j_.
#' @examples
#' \dontrun{
#' sm <- jaccard_similarity(terms = c("pelvic fin", "pectoral fin",
#'                                    "forelimb", "hindlimb",
#'                                    "dorsal fin", "caudal fin"),
#'                          .colnames = "label")
#' sm
#'
#' # e.g., turn into distance matrix, cluster, and plot
#' plot(hclust(as.dist(1-sm)))
#' }
#' @rdname similarity
#' @export
tanimoto_similarity <- function(subsumer_mat = NA, terms = NULL, ...) {
  if (missing(subsumer_mat)) {
    subsumer_mat <- subsumer_matrix(terms = terms, ...)
  }
  # numerator matrix = subsumers in the intersection set of i and j
  smi <- crossprod(as.matrix(subsumer_mat))
  # the diagonal is the subsumers of each term i
  nsubsumers <- diag(smi)
  # denominator matrix: |A|^2 + |B|^2 - A\dot B
  denom <- -smi + nsubsumers # add as columns
  denom <- t(t(denom) + nsubsumers) # add as rows
  # Tanimoto similarity is the ratio
  smi / denom
}

#' @description
#' The Jaccard similarity is computed using the Tanimoto similarity definition
#' for bit vectors
#' (see [Jaccard index at Wikipedia](https://en.wikipedia.org/wiki/Jaccard_index#Tanimoto's_definitions_of_similarity_and_distance)).
#' For the results to be a valid Jaccard similarity, weights must be zero and
#' one. If any weights are different, a warning is issued. 
#'
#' @export
#' @rdname similarity
jaccard_similarity <- function(subsumer_mat = NA, terms = NULL, ...) {
  if (missing(subsumer_mat)) {
    subsumer_mat <- subsumer_matrix(terms = terms, ...)
  }
  if (any(subsumer_mat > 1)) {
    warning("Some weights in the subsumer matrix are greater than 1. ",
            "Jaccard similarity requires weights of zero or one.")
  }
  if (any(subsumer_mat < 1 & subsumer_mat > 0)) {
    warning("Some non-zero weights in the subsumer matrix are not equal to 1. ",
            "Jaccard similarity requires weights of zero or one.")
  }
  tanimoto_similarity(subsumer_mat = subsumer_mat)
}

#' @description
#' The Cosine similarity _SC_ is computed using the Euclidean dot product formula.
#' See [Cosine similarity on Wikipedia](https://en.wikipedia.org/wiki/Cosine_similarity#Definition).
#' The metric is valid for any term vectors (columns of the subsumer matrix), i.e.,
#' \eqn{M_{i,j} \in \{0, W_i\}}{M[i,j] in {0, W[i]}} is not required. Note that
#' 1 - _SC_ is not a proper distance metric, because it violates the triangle
#' inequality. First convert to angle to obtain a distance metric.
#'
#' @export
#' @rdname similarity
cosine_similarity <- function(subsumer_mat = NA, terms = NULL, ...) {
  if (missing(subsumer_mat)) {
    subsumer_mat <- subsumer_matrix(terms = terms, ...)
  }
  # numerator matrix = A \dot B
  smi <- crossprod(as.matrix(subsumer_mat))
  # diagonal = vector magnitudes squared = ||A||^2
  vecmag <- sqrt(diag(smi))
  # denominator matrix: ||A|| * ||B||
  res <- smi / vecmag # divide as columns
  res <- t(t(res) / vecmag) # divide as rows
  res
}
