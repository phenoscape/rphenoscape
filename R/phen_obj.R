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
#' @param withTaxa logical. If TRUE taxa exhibiting the phenotype will be
#'  available through the phenotype object at key "taxa". Default is FALSE, because
#'  obtaining taxa requires an additional query per object.
#' @param ... additional parameters where applicable; ignored for printing
#'
#' @return
#' `as.phenotype` returns an object of type "phenotype", or a list of such objects
#'    if the object to be coerced had multiple elements (if a vector) or rows (if a
#'    data.frame). A phenotype object has properties "id" (ID, i.e., IRI of the
#'    phenotype), "label" (label of the phenotype if one exists), "states" (a
#'    data.frame of the character states to which the phenotype is linked, see value
#'    for `charstates`), and "eqs" (the EQ expression components as a list with keys
#'    "entities", "qualities", and "related_entities"). If `withTaxa` is TRUE,
#'    there will also be a key "taxa" (a data.frame with columns "id" and "label").
#'
#' @examples
#' # query for a set of phenotypes (IDs and their labels)
#' phens <- get_phenotypes(entity = "basihyal bone")
#' nrow(phens)
#' # turn one into a phenotype object
#' obj <- as.phenotype(phens[3, "id"])
#' class(obj)
#' obj
#'
#' # optionally include taxa exhibiting the phenotype
#' as.phenotype(phens[3,], withTaxa = TRUE)
#' # full list of taxa:
#' as.phenotype(phens[3,], withTaxa = TRUE)$taxa
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
#' # IDs that don't resolve still yield an object, but is not valid
#' suppressWarnings(obj <- as.phenotype("http://foo"))
#' is.phenotype(obj)
#' is_valid_phenotype(obj)
#' @name phenotype
#' @rdname phenotype
#' @export
as.phenotype <- function(x, withTaxa = FALSE, ...) {
  UseMethod("as.phenotype", x)
}

#' @export
as.phenotype.default <- function(x, ...) {
  res <- lapply(x, function(elem) {
    if (is.phenotype(elem))
      elem
    else {
      p <- Phenotype(elem, ...)
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

#' @rdname phenotype
#' @export
as.phenotype.data.frame <- function(x, ...) {
  if (is.null(x$id)) stop("data frame must have 'id' column containing IRI", call. = FALSE)
  as.phenotype(x$id, ...)
}

Phenotype <- function(iri, withTaxa = FALSE) {
  stopifnot(is.character(iri))
  res <- get_json_data(pkb_api("/phenotype/info"), query = list(iri = iri),
                       # some phenotypes can have very long URIs
                       forceGET = TRUE)
  names(res) <- sub("@", "", x = names(res))
  if (! is.null(res$states))
    names(res$states) <- sub("@", "", x = names(res$states))
  if (withTaxa) {
    taxa <- get_json_data(pkb_api("/taxon/with_phenotype"),
                          query = list(phenotype = iri, limit = 0), forceGET = TRUE)
    taxa <- taxa$results
    colnames(taxa) <- sub("@", "", colnames(taxa))
    res$taxa <- taxa
  }
  structure(res, class = c("phenotype", class(res)))
}

#' @description
#' `is.phenotype` tests whether an object is of type "phenotype"
#'
#' @return
#' `is.phenotype` returns TRUE if the object is of type "phenotype" and FALSE
#'    otherwise.
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
#' @return
#' `is_valid_phenotype` returns a logical vector of the same length as the input
#'    array of objects, with TRUE for those objects in the list that are of type
#'    "phenotype" and correspond to a phenotype in the database.
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
#' @return
#' `charstates` returns a data.frame. If called with a single phenotype object
#'    (or an object that coerces to one), the data.frame has columns "id" and "label" (for the
#'    character state), "character.id" and "character.label" (IRI and label
#'    of the character), and "study.id" and "study.label" (IRI and short label for
#'    the study to which the character and state belong). If called with a list of
#'    phenotype objects (or objects that coerce to such a list), the data.frame will
#'    include the character states from all phenotypes in the list. In this case,
#'    the character state columns will be "state.id" and "state.label", respectively,
#'    and there will be two additional columns, "phenotype.id" and "phenotype.label".
#'
#' @rdname phenotype
#' @export
charstates <- function(x) {
  UseMethod("charstates", x)
}

#' @export
charstates.phenotype <- function(x) {
  x$states
}

#' @export
charstates.default <- function(x) {
  charstates(as.phenotype(x))
}

#' @export
charstates.data.frame <- function(x) {
  charstates(as.phenotype(x))
}

#' @export
charstates.list <- function(x) {
  stopifnot(all(sapply(x, is.phenotype)))
  states <- lapply(x, charstates)
  counts <- sapply(states, nrow)
  res <- do.call("rbind", states) %>% dplyr::rename(state.id = "id",
                                                    state.label = "label")
  phens.id <- rep(sapply(x, function(p) p$id), times = counts)
  phens.label <- rep(sapply(x, function(p) p$label), times = counts)
  res <- cbind(phenotype.id = phens.id,
               phenotype.label = phens.label,
               res)
  res
}

#' @description
#' `chars` extracts the (non-redundant) characters from the phenotype object (or an object
#'   coercible to phenotype).
#'
#' @return
#' `chars` returns  a data.frame with collumns "character.id" and "character.label"
#'    (IRI and label of the character), and "study.id" and "study.label" (IRI and
#'    short label for the study to which the character and state belong).
#' @rdname phenotype 
#' @export
chars <- function(x) {
  states <- charstates(x)
  if (nrow(states) > 0)
    unique(states[, ! colnames(states) %in% c("id", "label")])
  else
    states
}

#' @description
#' `print` pretty-prints objects of type "phenotype"
#'
#' @rdname phenotype
#' @export
print.phenotype <- function(x, ...) {
  isValid <- is_valid_phenotype(x)
  cat("Phenotype '", if (is.na(x$label)) x$id else x$label, "'\n", sep = "")
  if (isValid) {
    if (length(x$states) > 0) {
      cat("Linked to states:\n")
      print(x$states[, c("label", "character.label", "study.label")])
    }
    l_e <- get_term_label(x$eqs$entities, preserveOrder = TRUE)
    l_q <- get_term_label(x$eqs$qualities, preserveOrder = TRUE)
    cat("\nEntities:\n    ",
        paste(l_e$label, " <", l_e$id, ">", collapse = "\n    ", sep = ""),
        "\nQualities:\n    ",
        paste(l_q$label, " <", l_q$id, ">", collapse = "\n    ", sep = ""),
        "\n",
        sep = "")
    if (length(x$eqs$related_entities) > 0) {
      l_r <- get_term_label(x$eqs$related_entities, preserveOrder = TRUE)
      cat("Towards:\n    ",
          paste(l_r$label, " <", l_r$id, ">", collapse = "\n    ", sep = ""),
          "\n", sep = "")
    } else
      cat("No related entities.\n")
    if ("taxa" %in% names(x)) {
      cat("Exhibited by taxa:\n")
      print(x$taxa, max = 10)
    } else
      cat("\nNo information about taxa exhibiting this phenotype.\n")
  } else
    cat("No states.\nNo EQs.\n")
  invisible(x)
}
