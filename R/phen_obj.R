#' Phenotype Objects
#'
#' Create and test objects of type "phenotype", and extract properties from them.
#'
#' @description
#' `as.phenotype` creates an object (or a list of objects) of type "phenotype".
#'   The object to be coerced can be a character vector (of IRIs), or a data.frame.
#'   In the latter case, there must be a column "id" with the IRIs of phenotypes.
#'   If the object is already of type "phenotype", it is passed through unchanged.
#'
#' @param x an object of type "phenotype" or coercible to it, or to be tested
#'  for being of type "phenotype"
#' @param ... additional parameters where applicable
#'
#' @return
#' `as.phenotype` returns and object of type "phenotype", or a list of such objects
#'    if the object to be coerced had multiple elements (if a vector) or rows (if a
#'    data.frame). A phenotype object has properties "id" (ID, i.e., IRI of the
#'    phenotype), "label" (label of the phenotype if one exists), "states" (a
#'    data.frame of the character states to which the phenotype is linked, see value
#'    for `charstates`), and "eqs" (the EQ expression components as a list with keys
#'    "entities", "qualities", and "related_entities").
#'
#' `charstates` returns a data.frame with columns "id" and "label" (for the
#'    character state), "character.id" and "character.label" (IRI and label
#'    of the character), and "study.id" and "study.label" (IRI and short label for
#'    the study to which the character and state belong).
#'
#' `chars` returns  a data.frame with collumns "character.id" and "character.label"
#'    (IRI and label of the character), and "study.id" and "study.label" (IRI and
#'    short label for the study to which the character and state belong).
#' @examples
#' # query for a set of phenotypes (IDs and their labels)
#' phens <- get_phenotypes(entity = "basihyal bone")
#' nrow(phens)
#' # turn one into a phenotype object
#' obj <- as.phenotype(phens[3, "id"])
#' class(obj)
#' obj
#'
#' # can also coerce entire list at once
#' objs <- as.phenotype(phens)
#' class(objs)
#' length(objs)
#' all(sapply(objs, is.phenotype))
#' objs[[3]]
#'
#' # extract character states and (non-redundant) characters
#' charstates(obj)
#' chars(obj)
#'
#' \dontrun{
#' # IDs that don't resolve still yield an object, but is not valid
#' obj <- as.phenotype("http://foo")
#' is.phenotype(obj)
#' is_valid_phenotype(obj)
#' }
#' @name phenotype
#' @rdname phenotype
#' @export
as.phenotype <- function(x, ...) {
  UseMethod("as.phenotype", x)
}

#' @export
as.phenotype.default <- function(x, ...) {
  res <- lapply(x, function(elem) {
    if (is.phenotype(elem))
      elem
    else {
      p <- Phenotype(elem)
      if (! is_valid_phenotype(p)) {
        p$label <- NA
        warning("Failed to retrieve phenotype for ID ", elem, call. = FALSE)
      }
      p
    }
  })
  if (length(res) == 1)
    res[[1]]
  else
    res
}

#' @export
as.phenotype.data.frame <- function(x, ...) {
  if (is.null(x$id)) stop("data frame must have 'id' column containing IRI", call. = FALSE)
  as.phenotype(x$id, ...)
}

Phenotype <- function(iri) {
  stopifnot(is.character(iri))
  res <- get_json_data(pkb_api("/phenotype/info"), query = list(iri = iri))
  names(res) <- sub("@", "", x = names(res))
  if (! is.null(res$states))
    names(res$states) <- sub("@", "", x = names(res$states))
  structure(res, class = c(class(res), "phenotype"))
}

#' @description
#' `is.phenotype` tests whether an object is of type "phenotype"
#'
#' @rdname phenotype
#' @export
is.phenotype <- function(x) {
  inherits(x, "phenotype")
}

#' @description
#' `is_valid_phenotype` tests which of the objects in the list are valid
#' phenotype objects, and returns a logical vector of the same length as `x`.
#' An object is a valid phenotype object if it is of type "phenotype" and its
#' ID has been found in the database.
#'
#' @rdname phenotype
#' @export
is_valid_phenotype <- function(x) {
  if (is.phenotype(x)) x <- list(x)
  sapply(x, function(p) {
    res <- FALSE
    if (is.phenotype(p)) {
      if (any(nzchar(p$label, keepNA = TRUE), na.rm = TRUE))
        res <- TRUE
      else
        res <- ! is.null(nrow(p$states))
    }
    res
  })
}

#' @description
#' `charstates` extracts the character states from the phenotype object (or an
#'   object coercible to phenotype)
#' @rdname phenotype
#' @export
charstates <- function(x) {
  if (! is.phenotype(x)) x <- as.phenotype(x)
  x$states
}

#' @description
#' `chars` extracts the characters from the phenotype object (or an object
#'   coercible to phenotype).
#' @rdname phenotype 
#' @export
chars <- function(x) {
  states <- charstates(x)
  if (nrow(states) > 0)
    unique(states[, ! colnames(states) %in% c("id", "label")])
  else
    states
}
