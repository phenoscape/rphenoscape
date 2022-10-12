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
#' @examples
#' nex <- RNeXML::nexml_read(system.file("examples", "ontotrace-result.xml", package = "rphenoscape"))
#' # drop by label matching
#' nexml_drop_otu(nex, filter = function(x) grepl(" sp.", x), at = "label")
#'
#' # can pipe dropping and then removing potentially unused characters or OTUs:
#' library(dplyr)
#' nexml_drop_char(nex, filter = function(x) grepl("pelvic", x), at = "label") %>%
#'   nexml_drop_otu(filter = is_unused_otu)
#'
#' \dontrun{
#' nex <- get_ontotrace_data(taxon = "Ictaluridae",
#'                           entity = "fin", variable_only = FALSE)
#' # ontotrace results store VTO IRIs in dwc:taxonID annotations:
#' nexml_drop_otu(nex,
#'                filter = function(x) !is_descendant("Ictalurus", x),
#'                at = "dwc:taxonID") %>%
#'   nexml_drop_char(filter = is_unused_char)
#' # anatomy IRIs are in obo:IAO_0000219 ("denotes") annotations:
#' nexml_drop_char(nex,
#'                filter = function(x)
#'                  !is_descendant("paired fin", x, includeRels = "part_of"),
#'                at = "obo:IAO_0000219") %>%
#'   nexml_drop_otu(filter = is_unused_otu)
#' }
#' @rdname nexml_drop
#' @importFrom methods new slot
#' @importClassesFrom RNeXML nexml
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

      # record a provenance chain
      nexml <- add_provenance_record(nexml)
    }
  }
  nexml
}

#' @description
#' `nexml_drop_char` drops characters from a [nexml][RNeXML::nexml] object. Note
#' that dropping characters may make some OTUs unused if they result in empty
#' rows and empty rows are pruned (see parameter `pruneRows`). It is therefore
#' recommended to drop unused OTUs afterwards.
#'
#' @param pruneStates logical, whether to also prune (i.e., drop) states that
#'   are no longer used after dropping the selected characters. The default is
#'   TRUE.
#' @param pruneRows logical, whether to also prune (i.e., drop) matrix rows
#'   that have become empty (i.e., have no cells left) after dropping the
#'   selected characters. The default is TRUE.
#' @rdname nexml_drop
#' @export
nexml_drop_char <- function(nexml, filter, at = NA, block = 1, ...,
                            pruneStates = TRUE, pruneRows = TRUE) {
  if (length(nexml@characters) < block) {
    warning("nexml object does not have characters block ", block, call. = FALSE)
    return(nexml)
  }
  
  chars <- nexml@characters[[block]]@format@char
  rows <- nexml@characters[[block]]@matrix@row
  isMod <- FALSE # track whether the object gets modified
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
      isMod <- TRUE
    }
  }
  # prune unused states if requested
  if (pruneStates) {
    states <- nexml@characters[[block]]@format@states
    statesIds <- sapply(states, slot, name = "id")
    statesUsed <- lapply(nexml@characters[[block]]@format@char, slot, name = "states")
    statesUsed <- unique(unlist(statesUsed))
    toKeep <- statesIds %in% statesUsed
    if (! all(toKeep)) {
      nexml@characters[[block]]@format@states <- new("ListOfstates", states[toKeep])
      isMod <- TRUE
    }
  }
  # prune rows with no cells if requested
  if (pruneRows && length(rows) > 0) {
    toKeep <- sapply(rows, function(x) length(x@cell)) > 0
    if (! all(toKeep)) {
      rows <- rows[toKeep]
      isMod <- TRUE
    }
  }
  # replace list of rows if there is a change
  if (isMod) {
    nexml@characters[[block]]@matrix@row <- new("ListOfrow", rows)
    # record a provenance chain
    nexml <- add_provenance_record(nexml)
  }

  nexml
}

#' @importFrom RNeXML get_metadata_values
#' @importFrom methods slotNames slot
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
                       vals <- get_metadata_values(nexml, annotated = x, props = at)
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
#' @importFrom methods is slot
#' @importClassesFrom RNeXML nexml
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

#' Generate and add provenance record to nexml
#'
#' If the content of a [nexml][RNeXML::nexml] object is modified, this
#' function can add a provenance record documenting the modification.
#' The provenance documentation is added to the toplevel metadata (i.e.,
#' at the ["nexml" level][RNeXML::get_metadata]).
#'
#' At present, for each invocation this implementation will do the following:
#' 1. If the top-level metadata for the nexml object contains
#'    `dc:description` annotations(s), they are moved to being nested
#'    within a `dcterms:provenance` annotation, and prefixed with
#'    "Original description:".
#' 2. A `dcterms:provenance` annotation is added, with nested properties
#'    `dc:creator` (see parameter `creator`), `dcterms:modified` (current
#'    time), and `dc:description`. The latter gives the command to document,
#'    see parameter `cmd`.
#' 3. A provenance record using [Prov-O](https://www.w3.org/TR/prov-o/)
#'    (a W3C recommendation) nested within `prov:wasGeneratedBy` is added.
#'    In RDF Turtle representation, the record would have the following structure
#'    (cf. [obo:IAO_0000591](http://purl.obolibrary.org/obo/IAO_0000591)):
#'    ```ttl
#'    :nexml prov:wasGeneratedBy [
#'      prov:endedAtTime "2019-06-20 15:09:08 GMT" ;
#'      prov:wasAssociatedWith [
#'        a obo:IAO_0000591 ;
#'        dc:title "rphenoscape" ;
#'        dcterms:hasVersion "<rphenoscape version>" ;
#'      ] ;
#'      prov:wasAssociatedWith [
#'        a prov:Person ;
#'        foaf:name "<creator>" ;
#'      ] ;
#'      prov:used [
#'        prov:value "<modifying command>" ;
#'      ] ;
#'    ] .
#'    ```
#' @param nexml the [nexml][RNeXML::nexml] object to which to add provenance
#'    documentation
#' @param cmd character, the command (such as a function invocation) to document
#'    in the provenance record. If `NA` (the default), the invocation of the
#'    function calling this one will be used as the command.
#' @param creator character, a value identifying the person running the
#'    software. The default is the system's `USER` environment variable.
#' @return A [nexml][RNeXML::nexml] object with provenance records added.
#' @importFrom utils packageName packageVersion
#' @importFrom RNeXML meta add_meta get_namespaces expand_prefix
#' @importClassesFrom RNeXML nexml
#' @export
add_provenance_record <- function(nexml, cmd = NA, creator = Sys.getenv("USER")) {
  now <- format(Sys.time(), tz = "GMT", usetz = TRUE)
  # if present, move original description to a provenance record
  metaProps <- sapply(nexml@meta, RNeXML::slot, name = "property")
  ns = get_namespaces(nexml)
  isDescr <- sapply(
    metaProps,
    function(x) expand_prefix(x, ns) == expand_prefix("dc:description", ns))
  if (any(isDescr)) {
    provRecs <- lapply(
      nexml@meta[isDescr],
      function(descr) {
        meta("dcterms:provenance",
             children = c(
               meta("dc:description",
                    paste("Original description:", descr@content))))
      })
    nexml@meta <- new("ListOfmeta", nexml@meta[! isDescr])
    nexml@meta <- c(nexml@meta, provRecs)
  }
  # generate a representation of the modifying operation if not provided
  if (is.na(cmd)) cmd <- paste0(format(sys.call(-1)), collapse = "\n")
  # we create two provenance records documenting the modification, one
  # using only Dublin Core vocabulary, and one using W3C's Prov-O vocabulary
  prov <- meta("dcterms:provenance",
               children = c(
                 meta("dcterms:modified", now),
                 meta("dc:creator", creator),
                 meta("dc:description", paste("Modified with:", cmd))))
  provO <- meta("prov:wasGeneratedBy",
                children = c(
                  meta("prov:endedAtTime", now),
                  meta("prov:wasAssociatedWith",
                       children = c(
                         meta(rel = "rdf:type", href = "obo:IAO_0000591"),
                         meta("dc:title", packageName()),
                         meta("dcterms:hasVersion",
                              as.character(packageVersion(packageName())))
                       )),
                  meta("prov:used", children = c(meta("prov:value", cmd))),
                  meta("prov:wasStartedBy",
                       children = c(
                         meta(rel = "rdf:type", href = "prov:Person"),
                         meta("foaf:name", creator)
                       ))
                ))
  nexml <- add_meta(c(prov, provO),
                    nexml = nexml,
                    namespaces = c(prov = "http://www.w3.org/ns/prov#",
                                   foaf = "http://xmlns.com/foaf/0.1/",
                                   obo = "http://purl.obolibrary.org/obo/",
                                   rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
  nexml
}


#' Obtains a character-state matrix from a nexml object
#'
#' @param nex a [nexml][RNeXML::nexml-class] object
#' @param otus_id logical, default TRUE, return a column with the otus block id.
#' @param states_as_labels logical, default FALSE, when TRUE returns states as labels instead of symbols.
#'   One can also use [state_symbols2labels()] to translate symbols to labels later.
#' @param verbose logical, default FALSE, If TRUE, messages informing about steps is printed.
#'
#' @return data.frame: The character-state matrix. The first column (taxa) contains taxon ids.
#' The second column (otu) contains otu ids. When the otus_id parameter is TRUE the third column (otus) will
#' contain otu block ids. The remaining columns are named for each anatomical label in the dataset and 
#' contain the associated state values.
#'
#' @seealso [get_char_matrix_meta()] to retrieve metadata for a nexml object.
#' @examples
#' # applied to a (synthetic) Ontotrace matrix:
#' nex <- get_ontotrace_data(taxon = c("Ictalurus", "Ameiurus"), entity = "fin spine")
#' get_char_matrix(nex, otus_id = FALSE)
#' 
#' # applied to a data matrix for a study
#' slist <- get_studies(taxon = "Ictalurus australis", entity = "fin spine")
#' nex <- get_study_data(slist$id[1])[[1]]
#' # for brevity show only 6 character data columns and first 5 rows
#' get_char_matrix(nex, otus_id = FALSE)[1:5,1:8]
#' # same, but states as labels (this can take a while)
#' get_char_matrix(nex, otus_id = FALSE, states_as_labels = TRUE)
#' 
#' @importFrom RNeXML get_characters
#' @export
get_char_matrix <- function(nex, otus_id = TRUE, states_as_labels = FALSE, verbose = FALSE) {
  
  m <- get_characters(nex, rownames_as_col = TRUE,
                      otu_id = TRUE, otus_id = otus_id)
  if (states_as_labels) {
    metacols = c(1,2)
    if (otus_id) metacols <- c(metacols, 3)
    state_symbols2labels(nex, charmat = m, metacolumns = metacols)
  } else
    m
}

#' Translates state symbols to labels in a character matrix
#'
#' Translates a character matrix using symbols for states to one using labels
#' instead. The character matrix must have been obtained from a [nexml][RNeXML::nexml-class]
#' object, usually using [get_char_matrix()], and the nexml object is required
#' for the translation.
#'
#' @note If the nexml object contains multiple character matrices, currently the character labels
#' must be distinct between them.
#' @param nex the [nexml][RNeXML::nexml-class] object from which the character matrix was obtained
#' @param charmat the character matrix (as a data.frame, usually obtained using [get_char_matrix()])
#' @param metacolumns the indexes of the columns in the matrix that contain metadata (such as taxa,
#'   identifiers, etc), not character states. If not all of these columns are before all character
#'   columns, then the returned matrix will move all of them to the front. The default is the first
#'   two columns.
#' @examples
#' # obtain a (synthetic) Ontotrace matrix:
#' nex <- get_ontotrace_data(taxon = c("Ictalurus", "Ameiurus"), entity = "fin spine")
#' m <- get_char_matrix(nex, otus_id = FALSE)
#' # by default it uses symbols
#' m
#' # translate symbols to labels without having to re-obtain the matrix
#' state_symbols2labels(nex, m)
#' # if we obtained the matrix with otus_id, then we have one more metadata column
#' m <- get_char_matrix(nex, otus_id = TRUE)
#' state_symbols2labels(nex, m, metacolumns = c(1,2,3))
#'
#' @importFrom dplyr inner_join  "%>%"
#' @export
state_symbols2labels <- function(nex, charmat, metacolumns = c(1,2)) {
  # create a modified matrix where the metadata columns are removed and the first row is column names
  mat <- rbind(colnames(charmat)[-metacolumns], charmat[, -metacolumns])

  # create lookup tables for character ID (by column name), state symbol, and polymorphic symbol
  states <- 
    (get_level(nex, "characters/format/states/state")[, c("symbol", "label", "state", "states", "characters")]
     %>% dplyr::inner_join(get_level(nex, "characters/format/char"),
                           by = c("states" = "states", "characters" = "characters"),
                           suffix = c(".state", ".char")))
  polymorph <- data.frame()
  polymorp_mems <- get_level(nex, "characters/format/states/polymorphic_state_set/member")
  if (nrow(polymorp_mems) > 0)
    polymorph <- dplyr::inner_join(polymorp_mems,
                                   get_level(nex, "characters/format/states/polymorphic_state_set"),
                                   by = c("polymorphic_state_set" = "polymorphic_state_set",
                                          "states" = "states",
                                          "characters" = "characters"))

  # utility function for translating one column
  # (where first row is the column name, i.e., character)
  translate_symbol <- function(col) {
    # find character matching column (character) label
    states_for_char <- states[states$label.char == col[1], ]
    if (nrow(states_for_char) == 0)
      states_for_char <- states[states$char == col[1], ]
    # map symbols to labels using the match table
    # (but remove the first element, which isn't a symbol)
    sapply(col[-1], function(x) {
      if(!is.na(x)) {
        labl <- states_for_char$label.state[states_for_char$symbol == x]
        if ((length(labl) == 0) && (nrow(polymorph) > 0)) {
          # if we can't match the symbol, try it as a polymorphic state symbol
          state_members <- polymorph[polymorph$symbol == x
                                     & polymorph$characters %in% states_for_char$characters, ]
          labl <- paste(states_for_char$label.state[states_for_char$state %in% state_members$state
                                                    & states_for_char$states %in% state_members$states],
                        collapse = " and ")
        }
        labl
      } else
        NA
    })
  }
  # translate each column using the utility function, convert to data.frame
  ret <- as.data.frame(apply(mat, 2, translate_symbol), stringsAsFactors = FALSE)
  # return after combining with the metadata columns
  return(cbind(charmat[, metacolumns], ret))
}

#' Obtains taxa and character metadata from a nexml object
#'
#' @param nex a [nexml][RNeXML::nexml-class] object
#'
#' @return A list of two data frames. The first list item, `id_taxa`, contains
#' a data frame with columns label, href (taxon IRI), otu (OTU ID) and otus (OTUs block ID) columns. The second list item,
#' `id_entities`, contains a data frame with columns label, href (character ID), and char (character ID in NeXML document).
#' For nexml objects obtained with [get_ontotrace_data()] the href column will contain the entity term IRI
#' rather than the character ID, and the char column will contain the entity term ID (the term IRI without the
#' HTTP path prefix).
#'
#' @examples
#' # apply to (synthetic) Ontotrace data matrix
#' nex <- get_ontotrace_data(taxon = c("Ictalurus", "Ameiurus"), entity = "fin spine")
#' get_char_matrix_meta(nex)
#'
#' # apply to data matrix from a study
#' slist <- get_studies(taxon = "Ictalurus australis", entity = "fin spine")
#' nex <- get_study_data(slist$id[1])[[1]]
#' metadata <- get_char_matrix_meta(nex)
#' # for brevity show only first 5 rows
#' metadata$id_taxa[1:5,]
#' metadata$id_entities[1:5,]
#'
#' @importFrom RNeXML get_taxa get_metadata get_level
#' @importFrom dplyr filter inner_join select rename "%>%"
#' @export
get_char_matrix_meta <- function(nex) {
  
  # NULLing out : for the R CMD CHECK
  property <- label <- href <- otu <- otus.x <- char <- NULL
  
  id_taxa <- get_taxa(nex)
  id_taxa_meta <- get_metadata(nex, "otu")
  
  id_taxa <- (id_taxa_meta
              %>% filter(property == meta_attr_taxon)
              %>% inner_join(id_taxa, by = c("otu" = "otu"))
              %>% select(label, href, otu, otus.x)
              %>% rename(otus = otus.x))
  
  id_entities <- get_level(nex, "characters/format/char")
  id_entities_meta <- get_metadata(nex, level="characters/format/char")
  
  id_entities <- (id_entities_meta
                  %>% filter(property == meta_attr_entity)
                  %>% inner_join(id_entities, by = c("char" = "char"))
                  %>% select(label, href, char))
  
  m_re <- list(id_taxa = id_taxa,
               id_entities = id_entities)
  
  return(m_re)
}
