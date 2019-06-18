#' Drop parts of a nexml object
#'
#' @name nexml_drop
#' @description
#' `nexml_drop_otu` drops OTUs (taxa) from a [nexml][RNeXML::nexml] object.
#' Currently none of the OTUs to be dropped can be used in a tree that's part
#' of the `nexml` object. If they are, first drop the OTUs from the tree(s)
#' (for example, using [drop.tip()][ape::drop.tip] from package "ape"), then
#' replace the tree(s). Dropping OTUs will not only drop them from the OTUs
#' block, but will also drop matrix rows that referenced the OTUs to be dropped.
#' This may in turn leave some characters unused. Therefore, it is recommended
#' to drop unused characters afterwards.
#'
#' @param nexml the [nexml][RNeXML::nexml] object from which to drop parts
#' @param filter logical, or a function returning a logical. If the latter,
#'   it will be passed the list of selected values for filtering (see parameter
#'   `at`), and any additional parameters (see `...`). The logical vector selects
#'   the list elements to drop from the given block (see `block`).
#' @param at character, selects the values for filtering if `filter` is a
#'   function. If `NA` (the default), a list of the respective objects (`otu`
#'   and `char`, respectively) are passed. If a string and the value matches
#'   a slot of the objects (such as "id", "label"), a list of those slot values
#'   is passed. Otherwise, it is assumed to be a metadata property for those
#'   objects, and a list of the respective metadata values is selected (with
#'   `NA` for objects that do not have a matching metadata annotation).
#' @param block integer, the respective block index (for OTUs and character
#'   blocks, respectively).
#' @param ... for `nexml_drop_otu` and `nexml_drop_char`, additional parameters
#'   to be passed on to the filter function.
#'
#'   For `is_unused_char` and `is_unused_otu`, if used outside of serving as
#'   value for the `filter` parameter, a parameter `characters` with
#'   the characters block (or list thereof), and/or a parameter `nexml` with the
#'   nexml object, must be provided. If only `nexml` is passed, all characters
#'   blocks is assumed.
#'
#'   `is_unused_otu` also accepts `ignoreTrees` (default is `FALSE`). If set to
#'   `TRUE`, trees will be ignored for determining whether an OTU is unused.
#'   Otherwise, if `nexml` is _not_ passed to `is_unused_otu`, a warning will be
#'   issued that it cannot check trees for determining the use of OTUs.
#' @return The functions for dropping components return a [nexml][RNeXML::nexml]
#'   object with the filtered components dropped.
#'
#'   `is_unused_char` returns a logical vector with TRUE for each `char`
#'   (character) object that is not used in the matrix (or matrices) of the
#'   characters block(s), and FALSE  otherwise.
#'
#'   `is_unused_otu` returns a logical vector with TRUE for each `otu`
#'   object that is not used in either the matrix (or matrices) of the characters
#'   block(s), or, unless `ignoreTrees = TRUE`, for a node of a tree, and FALSE
#'   otherwise.
#' @rdname nexml_drop
#' @export
nexml_drop_otu <- function(nexml, filter, at = NA, block = 1, ...) {
  if (length(nexml@otus) < block) {
    warning("nexml object does not have OTUs block ", block, call. = FALSE)
    return(nexml)
  }

  otus <- nexml@otus[[block]]@otu
  if (length(otus) > 0) {
    toDrop <- nexml_filter_items(nexml, otus, filter, at, ...)
    ids_to_rm <- sapply(otus, slot, name = "id")[toDrop]
    if (length(ids_to_rm) > 0) {
      # ensure there is no tree using these
      if (length(nexml@trees) > 0) {
        otus_used <- sapply(nexml@trees, slot, name = "otus")
        node_otus <- c()
        for (trees in nexml@trees[otus_used == nexml@otus[[block]]@id]) {
          otu_list <- lapply(
            trees@tree,
            function(tree) sapply(tree@node,
                                  function(node)
                                    if (length(node@otu) > 0) node@otu else NA))
          node_otus <- c(node_otus, unlist(otu_list))
        }
        node_otus <- node_otus[!is.na(node_otus)]
        if (any(node_otus %in% ids_to_rm)) {
          stop("One or more of the OTUs to be removed are used in at least one tree, ",
               "First drop these from the tree(s).", call. = FALSE)
        }
      }
      # remove matrix rows referring to these
      charblocks <- sapply(nexml@characters,
                           function(x) x@otus == nexml@otus[[block]]@id)
      bls <- seq(1, length(nexml@characters))[charblocks]
      for (bl in bls) {
        row_otus <- sapply(nexml@characters[[bl]]@matrix@row, slot, name = "otu")
        nexml@characters[[bl]]@matrix@row <-
          new("ListOfrow",
              nexml@characters[[bl]]@matrix@row[! (row_otus %in% ids_to_rm)])
      }
      # drop the otus from the otus block
      nexml@otus[[block]]@otu <- new("ListOfotu", otus[!toDrop])
    }
  }
  nexml
}

#' @description
#' `nexml_drop_char` drops characters from a [nexml][RNeXML::nexml] object. Note
#' that dropping characters may make some OTUs unused if they result in empty
#' rows, and pruning empty rows (see parameter `pruneRows` is requested). In
#' this case, it is recommended to drop unused OTUs afterwards.
#'
#' @param pruneStates logical, whether to also prune (i.e., drop) states that
#'   are no longer used after dropping the selected characters. The default is
#'   TRUE.
#' @param pruneRows logical, whether to also prune (i.e., drop) matrix rows
#'   that have become empty (i.e., have no cells left) after dropping the
#'   selected characters. The default is FALSE.
#' @rdname nexml_drop
#' @export
nexml_drop_char <- function(nexml, filter, at = NA, block = 1, ...,
                            pruneStates = TRUE, pruneRows = FALSE) {
  if (length(nexml@characters) < block) {
    warning("nexml object does not have characters block ", block, call. = FALSE)
    return(nexml)
  }
  
  chars <- nexml@characters[[block]]@format@char
  rows <- nexml@characters[[block]]@matrix@row
  if (length(chars) > 0) {
    toDrop <- nexml_filter_items(nexml, chars, filter, at, ...)
    ids_to_rm <- sapply(chars, slot, name = "id")[toDrop]
    if (length(ids_to_rm) > 0) {
      # remove matrix cells referring to these
      rows <- lapply(
        rows,
        function(row) {
          chars_used <- sapply(row@cell, slot, name = "char")
          row@cell <- new("ListOfcell", row@cell[! (chars_used %in% ids_to_rm)])
          row
        })
      # drop the characters from the characters block
      nexml@characters[[block]]@format@char <- new("ListOfchar", chars[!toDrop])
    }
  }
  # prune unused states if requested
  if (pruneStates) {
    states <- nexml@characters[[block]]@format@states
    statesIds <- sapply(states, slot, name = "id")
    statesUsed <- lapply(nexml@characters[[block]]@format@char, slot, name = "states")
    statesUsed <- unique(unlist(statesUsed))
    nexml@characters[[block]]@format@states <-
      new("ListOfstates", states[! (statesIds %in% statesUsed)])
  }
  # prune rows with no cells if requested
  if (pruneRows && length(rows) > 0) {
    rows <- rows[sapply(rows, function(x) length(x@cell)) > 0]
  }
  nexml@characters[[block]]@matrix@row <- new("ListOfrow", rows)

  nexml
}

nexml_filter_items <- function(nexml, items, filter, at = NA, ...) {
  if (is.function(filter)) {
    if (is.character(at)) {
      # is it a slot of the item?
      if (at %in% slotNames(items[[1]])) {
        at <- sapply(items, slot, name = at)
      } else {
        # treat as a metadata property
        at <- sapply(items,
                     function(x) {
                       vals <- RNeXML::get_metadata_values(nexml, annotated = x,
                                                           props = at)
                       vals[1]
                     })
      }
      toKeep <- filter(at, ...)
    } else
      toKeep <- filter(items, ...)
  } else
    toKeep <- filter

  toKeep
}

#' @description
#' `is_unused_char` is a filter function for `nexml_drop_char` for dropping
#' unused characters.
#' @param charList a list of `char` objects. This will be passed as the values
#'   to filter on.
#' @rdname nexml_drop
#' @export
is_unused_char <- function(charList, ...) {
  argList <- list(...)
  characters <- argList$characters
  nexml <- argList$nexml
  if (is.null(characters) && is.null(nexml)) {
    # extract the nexml object (from which we can get characters blocks) from the
    # invocation
    origArgs <- as.list(sys.call((-1)))
    nexml <- origArgs$nexml
    if (is.null(nexml)) nexml <- origArgs[[2]]
    nex <- eval(nexml, parent.frame(1))
    # if unsuccessful go back to parent of nexml_drop_XXX()
    if (is.null(nex)) nex <- eval(nexml, parent.frame(3))
    nexml <- nex
  }
  if (is.null(characters)) {
    if (is.null(nexml))
      stop("Either parameter 'characters' or 'nexml' must be provided.")
    if (! is(nexml, "nexml"))
      stop("Object designated as nexml is not of class 'nexml'.")
    characters <- nexml@characters
  }
  if (! is.list(characters)) characters <- list(characters)
  charids <- sapply(charList, slot, name = "id")
  chars_used <- lapply(
    characters,
    function(charbl)
      lapply(charbl@matrix@row,
             function(x) sapply(x@cell, slot, name = "char")))
  chars_used <- unique(unlist(chars_used))
  ! (charids %in% chars_used)
}

#' @description
#' `is_unused_otu` is a filter function for `nexml_drop_otu` for dropping
#' unused OTUs.
#' @param otuList a list of `otu` objects. This will be passed as the values
#'   to filter on.
#' @rdname nexml_drop
#' @importFrom methods is slot
#' @importClassesFrom RNeXML nexml
#' @export
is_unused_otu <- function(otuList, ...) {
  argList <- list(...)
  characters <- argList$characters
  nexml <- argList$nexml
  if (is.null(nexml)) {
    # extract the nexml object from invocation of parent
    origArgs <- as.list(sys.call((-1)))
    nexml <- origArgs$nexml
    if (is.null(nexml)) nexml <- origArgs[[2]]
    nex <- eval(nexml, parent.frame(1))
    # if unsuccessful go back to parent of nexml_drop_XXX()
    if (is.null(nex)) nex <- eval(nexml, parent.frame(3))
    nexml <- nex
  }
  if (is.null(characters)) {
    if (is.null(nexml))
      stop("Either parameter 'characters' or 'nexml' must be provided.")
    if (! is(nexml, "nexml"))
      stop("Object designated as nexml is not of class 'nexml'.")
    characters <- nexml@characters
  }
  if (! is.list(characters)) characters <- list(characters)
  otuids <- sapply(otuList, slot, name = "id")
  otus_used <- lapply(
    characters,
    function(charbl) sapply(charbl@matrix@row, slot, name = "otu"))
  otus_used <- unlist(otus_used)
  ignoreTrees <- argList$ignoreTrees
  if (is.null(ignoreTrees)) ignoreTrees <- FALSE
  if (! (ignoreTrees || is(nexml, "nexml")))
    warning("Missing 'nexml' parameter, or value not of class 'nexml'. ",
            "Cannot check OTUs against trees.")
  else if (! (ignoreTrees || length(nexml@trees) == 0)) {
    node_otus <- lapply(
      nexml@trees,
      function(trees)
        lapply(trees@tree,
        function(tree) sapply(tree@node,
                              function(node)
                                if (length(node@otu) > 0) node@otu else NA)))
    node_otus <- unlist(node_otus)
    node_otus <- node_otus[!is.na(node_otus)]
    otus_used <- c(otus_used, node_otus)
  }

  otus_used <- unique(otus_used)
  ! (otuids %in% otus_used)
}