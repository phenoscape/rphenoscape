#' Find terms matching a query text
#'
#' Searches the KB for terms matching the given text, and returns the result(s)
#' as a data frame (see Value).
#'
#' Matches can be filtered by type of term (class, property, etc), ontology in
#' which the term is defined, and by type of match (exact, partial, etc). The
#' term properties considered for matching can also be controlled.
#' @param query character, the search text
#' @param type character, the type of term to find, as a IRI or using common
#'   namespace prefixes such as "owl", "rdf", etc. The default is NA, which
#'   will use the remote API's default, currently "owl:Class".
#' @param definedBy character, the ontology in which a class has to be defined.
#'   Either IRIs, or a list of OBO ontology ID spaces (such as "UBERON", "ZFA",
#'   etc), which will be expanded to full IRIs using the OBO ontology IRI
#'   pattern. Alternatively, a call expression returning a logical vector, with
#'   a dot in the position where the `isDefinedBy` value is to be passed.
#'   The default is an expression that filters out rows with no `isDefinedBy`
#'   value. Can also be set to NA to suppress any filtering by ontology.
#' @param matchBy character, the term's (metadata) properties against which
#'   to match. Provide as IRIs or using common namespace prefixes. The default
#'   is NA, which will use the remote API's default, currently "rdfs:label",
#'   "oboInOwl:hasExactSynonym", "oboInOwl:NarrowSynonym", and
#'   "oboInOwl:hasBroadSynonym". To match only against label, use `c("rdfs:label")`.
#' @param matchTypes character, the types of matches (exact, partial, broad) to
#'   accept. By default (value NA), no filtering by match type is performed.
#'   Use `c("exact")` to accept exact matches only.
#' @param nomatch the value to return if there is no match returned by the
#'   remote API, or left after filtering.
#' @param limit the maximum number of matches to process. Set to NA for
#'   (virtually) no limit.
#' @param verbose logical; whether to issue a message if no matches found
#' @return A data frame with columns "id", "label", "isDefinedBy", and
#'   "matchType" and one row for each term match.
#' @export
#' @importFrom dplyr filter_at rename all_vars
#' @importFrom rlang expr
find_term <- function(query,
                      type = NA,
                      definedBy = quote(!is.na(.)),
                      matchBy = NA,
                      matchTypes = NA,
                      nomatch = NA,
                      limit = 100,
                      verbose = FALSE) {
  # construct the query string
  queryseq <- list(text = query)

  # if type is provided but not an IRI, convert it to one, using some common
  # cases
  if (! missing(type)) {
    type <- RNeXML::expand_prefix(type, semweb_ns(default = "owl"))
    queryseq <- c(queryseq, type = type)
  }
  
  if (is.character(definedBy)) {
    # if definedBy is a list, convert to IRIs those provided as abbreviations
    definedBy <- ontology_iri(definedBy)
    queryseq <- c(queryseq, definedBy = as.character(jsonlite::toJSON(definedBy)))
  }

  # if matchBy isn't already an IRI or a list thereof, convert it to one
  if (! missing(matchBy)) {
    matchBy <- RNeXML::expand_prefix(matchBy, semweb_ns(default = "rdfs"))
    queryseq <- c(queryseq, 
                  properties = as.character(jsonlite::toJSON(matchBy)))
  }
  
  # apply limit to result
  if (is.na(limit)) limit <- "0"
  queryseq <- c(queryseq, limit = limit)
  res <- get_json_data(pkb_api("/term/search"), query = queryseq)
  res <- res$results

  if (length(res) > 0 && nrow(res) > 0) {
    # it's possible that no matching term is defined in an ontology, in which
    # case that column will be missing altogether
    if (! ("isDefinedBy" %in% colnames(res)))
      res <- cbind(res, isDefinedBy = rep(NA, times = nrow(res)))

    # filter by where matches are defined, if not already part of the query
    if (is.call(definedBy)) {
      res <- dplyr::filter_at(res, "isDefinedBy", all_vars(!!definedBy))
    }
    # filter by match types
    if (! all(is.na(matchTypes))) {
      res <- dplyr::filter_at(res, "matchType", all_vars(. %in% matchTypes))
    }
    res <- dplyr::rename(res, id = "@id")
  }
  # nothing foud in the first place, or no matches left after filtering
  if (length(res) == 0 || nrow(res) == 0) {
    mssg(verbose, paste0("No matches for \"", query, "\". Please check your input and filters."))
    res <- nomatch
  }

  res
}

#' Find the IRI of the term matching a text query
#' 
#' Finds the term matching the query text, and returns its IRI. If the query
#' text is already a IRI, it is returned as is.
#'
#' This uses [find_term] to find matches, and assumes that the term of interest
#' is a class. If there is an exact match, its IRI will be returned. If there
#' isn't, by default partial, and as a last resort broad, matches will also be
#' considered, although this will result in a warning. 
#' @param text character. The search text to be resolved.
#' @param as character. The ontology or ontologies as which to find the term.
#'   Can be provided in several ways: (1) IRI of the ontology; (2) the ID space
#'   for OBO ontologies such as UBERON, VTO, etc; (3) `"anatomy"` and `"taxon"`
#'   as shorthand for all anatomy and taxon ontologies, respectively; (4) NA to
#'   disable any filtering by defining ontology. Options (1) and (2) can be
#'   combined. There is no default.
#' @param exactOnly logical. Whether to require an exact match. If TRUE, only
#'   the first exact match is returned. Default is FALSE.
#' @param nomatch the value to return if there is no match, by default NA.
#' @param verbose logical: optional. If TRUE, prints messages prior to potentially
#'   time-consuming operations. Default is FALSE.
#' @return The IRI if a match is found.
#' @export
get_term_iri <- function(text, as,
                         exactOnly = FALSE,
                         nomatch = NA,
                         verbose = FALSE) {
  # if the query string is already a HTTP URI, return it as the result
  if (startsWith(text, "http://") || startsWith(text, "https://")) return(text)

  mssg(verbose, paste("Querying the IRI for", text, sep = " "))
  if (length(as) == 1 && is.character(as)) {
    if (as == "taxon")
      as <- taxon_ontology_iris()
    else if (startsWith(as, "anatom"))
      as <- anatomy_ontology_iris()
  }
  if (exactOnly)
    iri_df <- find_term(query = text,
                        definedBy = as,
                        nomatch = nomatch,
                        matchTypes = c("exact"), limit = 20, verbose = verbose)
  else
    iri_df <- find_term(query = text,
                        definedBy = as,
                        nomatch = nomatch,
                        limit = 20, verbose = verbose)

  if (identical(iri_df, nomatch)) {
    warning("Could not find \"", text, "\" in the database. Please check your input.",
            call. = FALSE)
    return(invisible(nomatch))
  }
  if ('exact' %in% iri_df$matchType) {
    iri_df <- iri_df[iri_df$matchType == 'exact',]
    # if there are multiple exact matches, favor those defined in an ontology,
    # and then those with an exact label match
    if (nrow(iri_df) > 1) {
      isDefOnto <- !is.na(iri_df$isDefinedBy)
      if (any(isDefOnto)) iri_df <- iri_df[isDefOnto,]
    }
    if (nrow(iri_df) > 1) {
      matchLabel <- iri_df$label == text
      if (any(matchLabel)) iri_df <- iri_df[matchLabel,]
      if (nrow(iri_df) > 1) {
        warning("Multiple exact matches for \"", text, "\", returning first one:\n\t",
                paste0(iri_df$id, collapse = "\n\t"),
                call. = FALSE)
        iri_df <- iri_df[1,]
      }
    }
    id <- iri_df$id
  } else {
    if ('partial' %in% iri_df$matchType) {
      match_type <- 'partial'
    } else {
      match_type <- 'broad'
    }
    labs <-  iri_df$label[iri_df$matchType == match_type]
    ids <- iri_df$id[iri_df$matchType == match_type]
    deco <- ifelse(length(labs) == 1, 'only', 'first')
    warning_msg <- paste("No exact match for", text,
                         "can be found in database. Returning the",
                         deco, match_type,
                         "match", labs[1])
    if (deco == "first") {
      warning_msg <- paste0(warning_msg,
                           ". Other candidates are ",
                           paste(labs[2:length(labs)], collapse = ", "))
    }
    warning(warning_msg,
            call. = FALSE)
    id <- ids[1]
  }

  id
}

semweb_ns <- function(default = NA) {
  ns <- c(
    dc = "http://purl.org/dc/elements/1.1/",
    owl = "http://www.w3.org/2002/07/owl#",
    rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    rdfs = "http://www.w3.org/2000/01/rdf-schema#",
    xsd = "http://www.w3.org/2001/XMLSchema#",
    oboInOwl = "http://www.geneontology.org/formats/oboInOwl#"
  )
  if (! is.na(default)) ns <- c(unname(ns[default]), ns)
  ns
}

#' Get IRIs of ontologies with anatomy terms
#'
#' Obtains the IRIs of anatomical entity ontologies in the Phenoscape KB.
#' Note that this will normally include the Gene Ontology (GO), because it
#' contains a substantial number of anatomical entity terms in its subcellular
#' location branch.
#'
#' @return A character vector
#' @export
#' @importFrom dplyr filter_at all_vars
anatomy_ontology_iris <- local({
  .iris <- c();
  function() {
    if (length(.iris) == 0) {
      res <- find_term("anatomical structure",
                       matchBy = c("rdfs:label"),
                       matchTypes = c("exact", "partial"),
                       limit = 200)
      res <- dplyr::filter_at(res, "label",
                              all_vars(
                                startsWith(., "anatomical structure") 
                                & !grepl("quality", .)
                                & !grepl("abnormal", .)
                             )) 
      .iris <<- unique(res$isDefinedBy)
    }
    .iris
  }
})

#' Get IRIs of ontologies with taxonomy terms
#'
#' Obtains the IRIs of taxon ontologies in the Phenoscape KB.
#' @return A character vector
#' @export
#' @importFrom dplyr filter_at all_vars
taxon_ontology_iris <- local({
  .iris <- c();
  function() {
    if (length(.iris) == 0) {
      res <- find_term("Vertebrata",
                       matchBy = c("rdfs:label"),
                       matchTypes = c("exact"))
      .iris <<- unique(res$isDefinedBy)
    }
    .iris
  }
})

ontology_iri <- function(abbr) {
  ifelse(nchar(abbr) == 0 |
           startsWith(abbr, "http://") |
           startsWith(abbr, "https://"),
         abbr,
         paste0("http://purl.obolibrary.org/obo/", tolower(abbr), ".owl"))
}

taxon_id <- function() ontology_iri('VTO')
anatomical_id <- function() ontology_iri('UBERON')
phenotype_id <- function() ontology_iri('PATO')

#' @description
#' `partOf_iri` returns the IRI of the canonical "part_of" relationship in the
#'     database.
#' @rdname term_iri
#' @export
partOf_iri <- function() {
  term_iri("part_of",
           type = "owl:ObjectProperty", preferOntologies = c("BFO", "RO"))
}

#' @description
#' `hasPart_iri` returns the IRI of the canonical "has_part" relationship in the
#'     database.
#' @rdname term_iri
#' @export
hasPart_iri <- function() {
  term_iri("has_part", type = "owl:ObjectProperty", preferOntologies = c("BFO", "RO"))
}

#' @description
#' `developsFrom_iri` returns the IRI of the canonical "develops from" relationship in the
#'     database.
#' @rdname term_iri
#' @export
developsFrom_iri <- function() {
  term_iri("develops from", type = "owl:ObjectProperty", preferOntologies = c("BFO", "RO"))
}

#' Obtain IRI(s) for canonical terms and properties
#'
#' Provides cached access to IRIs of terms and properties that are frequently used.
#'
#' Requested IRIs are not hard-coded. Instead, they are dynamically retrieved from
#' the API when first queried, and (depending on parameters) are subsequently held
#' in cache in the session. Hence, if they were to change in the database, the R
#' session would need to be restarted for this change to be reflected in the result.
#'
#' For the frequently needed properties "part of", "has part" etc, one should use
#' the predefined functions ([partOf_iri()], [hasPart_iri()]), so that the correct
#' matches are cached and used internally by the package.
#' @param label character, the label of the term for which to obtain the IRI
#' @param type character, the type of the term, as a IRI or using common
#'   namespace prefixes such as "owl", "rdf", etc. For example, "owl:Class",
#'   or "owl:ObjectProperty". The default is "owl:Class".
#' @param preferOntologies character, an array of ontologies to use for preference.
#'   In the case of multiple matches, matches will be reordered in the order given,
#'   and if matches are from preferred and other ontologies, those from ontologies
#'   not in the preference list will be discarded. If provided and only matches
#'   from ontologies not in the preference list are found, they will not be cached.
#'   Can be provided as OBO ontology prefixes, or full ontology IRIs. The default
#'   is no preferences.
#' @param firstOnly boolean, whether to return only the first match in case of
#'   multiple matches. The default is TRUE.
#' @return Character, the IRI(s) for the requested term or property.
#' @examples
#' term_iri("part of", type = "owl:ObjectProperty", preferOntologies = c("RO", "BFO"))
#' term_iri("develops from", type = "owl:ObjectProperty", preferOntologies = c("RO"))
#' term_iri("anatomical structure", firstOnly = FALSE)
#' @export
term_iri <- local({
  .iris <- list();
  function(label, type = "owl:Class", preferOntologies = NULL, firstOnly = TRUE) {
    if (is.null(.iris[[label]])) {
      res <- find_term(label, type = type,
                       matchBy = c("rdfs:label"), matchTypes = c("exact"))
      if (nrow(res) > 1 && ! is.null(preferOntologies)) {
        # prioritize by ontology by reordering by ontology preference
        reordering <- match(ontology_iri(preferOntologies), res$isDefinedBy)
        if (any(! is.na(reordering))) {
          res <- res[reordering[! is.na(reordering)],]
        }
        if (firstOnly) res <- res[1,]
      }
      # cache only if either matches what was desired, or no preferences stated
      if (is.null(preferOntologies) ||
          res[1, "isDefinedBy"] %in% ontology_iri(preferOntologies))
        .iris[[label]] <<- res$id
      res$id
    } else
      .iris[[label]]
  }
})

# to silence R CMD check
. <- NULL
