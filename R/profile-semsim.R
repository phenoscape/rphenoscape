#' Semantic similarity between profiles
#'
#' Calculates the semantic similarity between profiles (groups) of terms.
#'
#' A profile refers to a group of terms, and profile similarity to the similarity
#' between these groups. Profile similarity can be calculated in two principal
#' ways, often referred to as group-wise and pair-wise. Both are supported here.
#' - In group-wise mode, the terms and their subsumers in a group are first combined
#'   into a single vector representing their union (also sometimes called the
#'   subgraph corresponding to a group). Then the pairwise algorithm is applied to
#'   the unions.
#' - In pairwise mode, the pairwise similarities between all terms are calculated,
#'   and then the pairwise scores between the terms in two groups are reduced
#'   to a single score giving the between-group score.
#'
#' @param pairwise the function for calculating (pairwise) semantic similarity
#'   between term vectors. This function must accept the subsumer matrix as its
#'   first argument. Additional named arguments (see `...`) may be passed to it
#'   as well. See [similarity] for semantic similarity metrics available in this
#'   package.
#' @param ... additional named arguments to be passed to the pairwise function.
#' @param subsumer_mat the subsumer matrix as data.frame. See [subsumer_matrix()]
#'   for more detail and the usual way for obtaining it.
#' @param f a [factor] (or an object coercible to factor) defining the group
#'   (profile) membership of the terms (= columns) in the subsumer matrix.
#'   Columns and rows of the resulting profile similarity matrix will take their
#'   names from the levels of the factor. The factor may have 2 or more levels.
#' @param reduce in pairwise mode, the function for reducing pairwise scores to
#'   a single between-group score. In each invocation, the function will be passed
#'   the submatrix of the pairwise similarity matrix that corresponds to the
#'   pairwise scores of terms from the two profiles to be compared. A function
#'   may aggregate the scores irrespective of the column and row structure of the
#'   submatrix (for example, by taking the mean), or aggregate first by columns
#'   or rows, followed by aggregating the results (for example, see [bestPairs()]).
#'   In the latter case, the result may not be symmetric (i.e., s(p1,p2) != s(p2,p1)).
#' @note
#' In pairwise mode, if the `reduce` function is asymmetric (as will typically be
#' the case for functions aggregating in two steps), the upper and lower triangle
#' of the profile similarity matrix will not be symmetric. If a symmetric similarity
#' matrix is desired, this can be achieved by computing `(X +  t(X)) / 2`, if X is
#' the profile similarity matrix.
#' @examples
#' tt <- sapply(c("pelvic fin", "pectoral fin",
#'                "forelimb", "hindlimb", "dorsal fin", "caudal fin"),
#'              pk_get_iri, as = "anatomy")
#'
#' # define groups (profiles) as factors:
#' pairedUnpaired <- c(rep("paired", times = 4), rep("unpaired", times = 2))
#' finsLimbs <- c("fins", "fins", "limbs", "limbs", "fins", "fins")
#' pairedFinLimb <- interaction(as.factor(pairedUnpaired), as.factor(finsLimbs))
#'
#' # compute subsumer matrix
#' subs.mat <- subsumer_matrix(tt, .colnames = "label", .labels = names(tt),
#'                             preserveOrder = TRUE)
#' # group-wise profile similarity:
#' profile_similarity(jaccard_similarity, subs.mat, f = pairedUnpaired)
#' profile_similarity(jaccard_similarity, subs.mat, f = finsLimbs)
#' profile_similarity(jaccard_similarity, subs.mat, f = pairedFinLimb)
#'
#' # pairwise, using mean (average pairwise score); result is symmetric
#' profile_similarity(jaccard_similarity, subs.mat, f = pairedFinLimb,
#'                    reduce = mean)
#' # the same, but excluding self-similarity of terms within groups
#' profile_similarity(jaccard_similarity, subs.mat, f = pairedFinLimb,
#'                    reduce = reduce.ignoringDiag)
#' # pairwise, using max; result is symmetric
#' profile_similarity(jaccard_similarity, subs.mat, f = pairedFinLimb,
#'                    reduce = max)
#' # pairwise, using average of best pairs; result is _not_ symmetric
#' sm <- profile_similarity(jaccard_similarity, subs.mat, f = pairedFinLimb,
#'                          reduce = bestPairs)
#' sm
#' # make symmtric, for example by averaging:
#' (sm + t(sm)) / 2
#' @export
profile_similarity <- function(pairwise, subsumer_mat, ..., f, reduce = NA) {
  stopifnot(is.function(pairwise),
            is.data.frame(subsumer_mat),
            ncol(subsumer_mat) == length(f))
  if (! is.factor(f)) f <- as.factor(f)

  # remove empty levels so they don't create any trouble
  group.sizes <- tapply(f, f, length)
  if (any(is.na(group.sizes))) levels(f)[is.na(group.sizes)] <- NA

  if (! is.function(reduce)) {
    # by default combine terms (and their subsumers) into group-wise subgraphs
    splits <- split.data.frame(t(subsumer_mat), f = f)
    subgraphs <- lapply(splits, function(m) {
      apply(m, 2, function(subsVec) {
        if (sum(subsVec) > 0) 1 else 0
      })
    })
    subsumer_mat <- do.call(cbind, subgraphs)
  }

  # run pairwise similarities
  sm <- pairwise(subsumer_mat = subsumer_mat, ...)

  if (is.function(reduce)) {
    sm.reduced <- sapply(levels(f), function(l1) {
      sapply(levels(f), function(l2) {
        reduce(as.matrix(sm[f == l1, f == l2]))
      })
    })
    sm <- sm.reduced
  }

  sm
}

#' @description
#' `bestPairs` aggregates pairwise scores by "best pairs" between two profiles.
#' That is, for profiles P1 and P2 with terms T1\[i\] (i = 1,...n) and T2\[j\]
#' (j = 1,...,m), respectively, for S(P1, P2) `bestPairs` will determine for
#' each i the score s(T1\[i\], T2\[j\]) that is "best" (see parameter `best`),
#' followed by aggregating the resulting n best-pair scores to a single value
#' (see parameter `aggregate`). The resulting scores are necessarily asymmetric,
#' i.e., S(P1, P2) != S(P2, P1) for P1 != P2.
#' 
#' @param X array or matrix, the submatrix of pairwise scores to aggregate
#' @param best the function for determing the best score. The default is [max()].
#' @param aggregate the function for aggregating scores (for `reduce.ignoringDiag`)
#'   or best pairwise scores (for `bestPairs`). For `reduce.ignoringDiag`, the
#'   function must also accept parameter `na.rm`, and if the value is TRUE, remove
#'   NA values before calculating the aggregate. The default is [mean()].
#' @rdname profile_similarity
#' @export
bestPairs <- function(X, best = max, aggregate = mean) {
  bestPairs <- apply(X, 2, best)
  aggregate(bestPairs)
}

#' @description
#' `reduce.ignoringDiag` is a convenience function for aggregating pairwise
#' similarity scores such that the diagonal of the pair-wise similarity matrix
#' is ignored. This may be desirable, for example, when average with-in group
#' similarity should exclude the similarity of terms to themselves.
#' @rdname profile_similarity
#' @export
reduce.ignoringDiag <- function(X, aggregate = mean) {
  if (all(colnames(X) == rownames(X)))
    diag(X) <- NA
  aggregate(X, na.rm = TRUE)
}
