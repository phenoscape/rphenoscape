#' @title Deprecated functions in \pkg{rphenoscape}.
#' @description The functions listed below are deprecated and will become defunct in
#'   the near future. When possible, alternative functions with identical or similar
#'   functionality are mentioned. Where the signature of the alternative (new) 
#'   function differs significantly from that of the deprecated one, help pages for
#'   the deprecated functions are available at \code{help("<function>-deprecated")}.
#' @name rphenoscape-deprecated
#' @keywords internal
NULL

#' @rdname rphenoscape-deprecated
#' @section \code{pk_get_iri}:
#' For `pk_get_iri()` use [get_term_iri()] instead.
#' @keywords internal
#' @export
pk_get_iri <- function(...) {
  .Deprecated("get_term_iri")
  get_term_iri(...)
}

#' @rdname rphenoscape-deprecated
#' @section \code{pk_get_study_list}:
#' For `pk_get_study_list()` use [get_studies()] instead. Note that `get_studies()`
#' returns an empty result set if nothing was found, in contrast to `pk_get_study_list`,
#' which returns FALSE in that case (and prints a message).
#' @keywords internal
#' @export
pk_get_study_list <- function(...) {
  .Deprecated("get_studies")
  res <- get_studies(...)
  if (length(res) == 0) {
    mssg(T, "No study found in database.")
    invisible(FALSE)
  } else
    res
}

#' @rdname rphenoscape-deprecated
#' @section \code{pk_taxon_class}:
#' For `pk_taxon_class()` use [term_classification()] with as="taxon" instead.
#' Note that `pk_taxon_class()` defaults verbose to TRUE instead of FALSE as
#' `term_classification()` does. Also `pk_taxon_class()` return value has the legacy `@id` name.
#' @keywords internal
#' @export
pk_taxon_class <- function(x, verbose=TRUE) {
  .Deprecated("term_classification")
  term_classification_raw(x, as = "taxon", verbose)
}

#' @rdname rphenoscape-deprecated
#' @section \code{pk_anatomical_class}:
#' For `pk_anatomical_class()` use [term_classification()] with as="anatomy" instead.
#' Note that `pk_anatomical_class()` defaults verbose to TRUE instead of FALSE as
#' `term_classification()` does. Also `pk_anatomical_class()` return value has the legacy `@id` name.
#' @keywords internal
#' @export
pk_anatomical_class <- function(x, verbose=TRUE) {
  .Deprecated("term_classification")
  term_classification_raw(x, as = "anatomy", verbose)
}

#' @rdname rphenoscape-deprecated
#' @section \code{pk_phenotype_class}:
#' For `pk_phenotype_class()` use [term_classification()] with as="pato" instead.
#' Note that `pk_phenotype_class()` defaults verbose to TRUE instead of FALSE as
#' `term_classification()` does. Also `pk_phenotype_class()` return value has the legacy `@id` name.
#' @keywords internal
#' @export
pk_phenotype_class <- function(x, verbose=TRUE) {
  .Deprecated("term_classification")
  term_classification_raw(x, as = "pato", verbose)
}