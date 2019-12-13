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

