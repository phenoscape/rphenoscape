#' Obtains a presence-absence dependency matrix
#'
#' Obtains a presence-absence dependency matrix for the given set of terms. The
#' resulting matrix M will have values 1 and 0, where M\[i,j\] = 1 iff the
#' presence if term _i_ implies the presence of term _j_. Note that it follows that
#' M\[i,j\] = 1 iff the absence of term _j_ implies the absence of term _i_.
#' Note also that the matrix is asymmetric; that is, M\[i,j\] = 1 does not imply
#' M\[j,i\] = 1 for \eqn{i\neq j}{i != j}. Terms _i_ and _j_ (\eqn{i\neq j}{i != j})
#' for which M\[i,j\] = M\[j,i\] = 1 are sometimes referred to as _super-dependent_.
#' @param terms character, the list of terms for which to compute the dependency
#'   matrix. Can be given as term IRIs or term labels, and the list can contain
#'   both. Terms given as labels will first be resolved to IRIs, assuming they
#'   are from an anatomy ontology.
#' @param .names character, how to name the rows and columns of the resulting
#'   matrix.
#'   - `"ID"` (the default): use the term IDs (the last component of the
#'     term IRIs).
#'   - `"IRI"`: use the term IRIs as names. Note that the column names will have
#'     `:`, `/`, and some other characters replaced with `.` (dot).
#'   - `"label"`: use the terms' labels (see `.labels` parameter).
#' @param .labels character, the labels for terms where known. Only used if
#'   `.names = "label"`. If NULL (the default), labels will be looked up if `terms`
#'   are provided as IRIs; elements of the `terms` list that are not in IRI form
#'   are assumed to be the label. If a list, must have the same length and ordering
#'   as `terms`; any NA elements will be looked up (from the corresponding term
#'   IRI).
#' @param preserveOrder logical, whether to return rows (and columns) in the same
#'   order as `terms`. The default is not to preserve the order.
#' @param verbose logical, whether to print informative messages about certain
#'   potentially time-consuming operations.
#' @return A data.fram M with M\[i,j\] = 1 iff the presence of term _i_ implies the
#'   presence of term _j_, and 0 otherwise.
#'
#'   The matrix will have additional attributes depending on the choice of how to
#'   name rows and columns. If `.names = "ID"` (the default), the matrix will have
#'   an attribute `prefixes` giving the URL prefixes removed from the term IRIs
#'   to yield the IDs, in the order of the rows. If `.names = "label"`, it will
#'   have attribute `term.iris`, giving the term IRIs for the rows (and columns).
#'   Note that these extra attributes will be lost upon subsetting the returned
#'   matrix.
#' @examples
#' \dontrun{
#' tl <- c("http://purl.obolibrary.org/obo/UBERON_0000981",
#'         "http://purl.obolibrary.org/obo/UBERON_0002103",
#'         "http://purl.obolibrary.org/obo/UBERON_0000976",
#'         "http://purl.obolibrary.org/obo/UBERON_0002102")
#' m <- pa_dep_matrix(tl)
#' m # term IDs as row and column names
#' id_prefixes <- attr(m, "prefixes")
#' id_prefixes # 4x "http://purl.obolibrary.org/obo/"
#'
#' m <- pa_dep_matrix(tl, .names = "label")
#' m # term labels as row and column names
#' mat_terms <- attr(m, "term.iris")
#' mat_terms # term IRIs in the same order as rows (and columns)
#' }
#' @importFrom stringi stri_match_first_regex
#' @export
pa_dep_matrix <- function(terms,
                          .names = c("ID", "IRI", "label"), .labels = NULL,
                          preserveOrder = FALSE,
                          verbose = FALSE) {
  .names <- match.arg(.names)
  if (.names == "label" && is.null(.labels)) {
    .labels <- ifelse(startsWith(terms, "http://") | startsWith(terms, "https://"),
                      NA,
                      terms)
  }
  term_iris <- 
    unname(sapply(terms,
                  function(x) pk_get_iri(x, as = "anatomy",
                                         exactOnly = TRUE, verbose = verbose)))
  queryseq <- list(terms = as.character(jsonlite::toJSON(term_iris)))
  m <- get_csv_data(pkb_api("/entity/dependency"), query = queryseq,
                    row.names = 1, header = TRUE,
                    verbose = verbose)
  if (preserveOrder) {
    reordering <- match(term_iris, row.names(m))
    m <- m[reordering, reordering]
  }
  if (.names == "ID") {
    parts <- stringi::stri_match_first_regex(row.names(m), "(^.+[/#])(.+$)")
    row.names(m) <- parts[,3]
    colnames(m) <- parts[,3]
    attr(m, "prefixes") <- parts[,2]
  } else if (.names == "label") {
    # do we need to fill in any labels?
    if (any(is.na(.labels))) {
      query_iris <- terms[is.na(.labels)]
      lbls <- get_term_label(query_iris, preserveOrder = TRUE, verbose = verbose)
      lbls <- ifelse(is.na(lbls$label), query_iris, lbls$label)
      .labels[is.na(.labels)] <- lbls
    }
    nameMap <- match(row.names(m), term_iris)
    attr(m, "term.iris") <- row.names(m)
    row.names(m) <- .labels[nameMap]
    colnames(m) <- row.names(m)
  }
  m
}
