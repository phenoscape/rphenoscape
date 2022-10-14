OWL_MANCHESTER_CLASS <- "owlmn"

#' OWL Objects
#'
#' Create objects of type "owl" that represent OWL expressions.
#'
#' @description
#' `as.owl` creates an object (or a list of objects) of type "owl".
#'   The object to be coerced must be a character vector.
#'   The character vector must be a valid  OWL expression appropriate for the format.
#'   The object returned will have a type appropriate for the format.
#'
#' @param owlstr a valid OWL expression for the chosen format
#' @param format the OWL syntax to which `owlstr` conforms - currently only "manchester" (for [OWL Manchester Syntax](https://www.w3.org/2007/OWL/draft/owl2-manchester-syntax/)) is allowed
#'   which applies the "owlmn" type to the returned object
#' @param usesLabels logical. If TRUE the owlstr parameter must contain a label-based class expression
#'  to be resolved into OWL Manchester expression composed of IRIs. Labels containing spaces must be
#'  single-quoted, e.g. 'basihyal bone'. Exact label matches will be resolved to term identifiers.
#'  Default is FALSE.
#'
#' @return
#' `as.owl` returns an object of type "owl" and a format specific type such as "owlmn",
#'   or a list of such objects. Note that (at least currently) these are in essence character strings, and are recognizable as such by R.
#'
#' @examples
#' as.owl("<http://purl.obolibrary.org/obo/VTO_0034991> 
#'         or <http://purl.obolibrary.org/obo/VTO_0037519>")
#' as.owl("Characiformes or Siluriformes", usesLabels = TRUE)
#' obj <- as.owl("'fin spine' and ('part of' some 'fin spine')", usesLabels = TRUE)
#' obj
#' is.character(obj)
#' @name owl
#' @rdname owl
#' @export
as.owl <- function(owlstr, format='manchester', usesLabels = FALSE) {
  # only manchester format allowed - class "owlmn"
  stopifnot(format == "manchester")
  res <- lapply(owlstr, function(elem) {
    if (is.owl(elem))
      elem
    else
      if (usesLabels)
        resolve_label_expression(owlstr)
      else
        Owl(elem, owlformatclass = OWL_MANCHESTER_CLASS)
  })
  if (length(res) == 1)
    res[[1]]
  else
    res
}

#' @description
#' `is.owl` tests whether an object is of type "owl"
#'
#' @param x an object to check
#'
#' @return
#' `is.owl` returns TRUE if the object is of type "owl" and FALSE
#'    otherwise.
#'
#' @rdname owl
#' @export
is.owl <- function(x) {
  inherits(x, "owl")
}

#' @description
#' `is.manchester_owl` tests whether an object is of type "owlmn"
#'
#' @param x an object to check
#'
#' @return
#' `is.manchester_owl` returns TRUE if the object is of type "owlmn" and FALSE
#'    otherwise.
#'
#' @rdname owl
#' @export
is.manchester_owl <- function(x) {
  inherits(x, OWL_MANCHESTER_CLASS)
}

Owl <- function(owlstr, owlformatclass) {
  stopifnot(is.character(owlstr))
  structure(owlstr, class = c("owl", owlformatclass, class(owlstr)))
}

#' Convert a label-based class expression in OWL Manchester syntax into an expression 
#' composed of IRIs.
#'
#' @param label_expression character, class expression in a label-based Manchester syntax,
#'   e.g. "'part of' some head"
#' @return A object of class "owl" with a resolved OWL Manchester expression composed of IRIs.
#'     Note that (at least currently) these are in essence character strings, and are recognizable as such by R.
resolve_label_expression <- function(label_expression) {
  owlstr <- get_plain_text(pkb_api("/term/resolve_label_expression"),
                           query=list(expression=label_expression))
  as.owl(owlstr)
}
