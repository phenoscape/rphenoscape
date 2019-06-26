#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
get_json_data <- function(url, query, verbose = FALSE, ensureNames = NULL) {
  res <- httr::GET(url, httr::accept_json(), query = query)
  stop_for_pk_status(res)
  # if content-type is application/json, httr:content() doesn't assume UTF-8
  # encoding if charset isn't provided by the server, arguably erroneously
  # (because specifying a different charset would violate the spec)
  enc <- NULL
  if (startsWith(res$headers$`content-type`, "application/json") &&
      length(grep("charset", res$headers$`content-type`, fixed = TRUE)) == 0) {
    enc = "UTF-8"
  }
  out <- httr::content(res, as = "text", encoding = enc)

  mssg(verbose, "Parse JSON ...")
  res <- jsonlite::fromJSON(out, simplifyVector = TRUE, flatten = TRUE)
  if (is.list(res) && ! is.null(ensureNames)) {
    resList <- res
    if (! is.null(res$results)) resList <- res$results
    currNames <- names(resList)
    missNames <- ensureNames[! (ensureNames %in% currNames)]
    if (length(missNames) > 0) {
      # note that if resList is a data frame, this turns it into a list
      resFixed <- c(resList, rep(NA, times = length(missNames)))
      names(resFixed) <- c(currNames, missNames)
      # restore data frame if it was one originally
      if (is.data.frame(resList)) resFixed <- as.data.frame(resFixed)
      # restore return value
      if (is.null(res$results))
        res <- resFixed
      else
        res$results <- resFixed
    }
  }
  res
}
pk_GET <- get_json_data

#' @importFrom httr GET content
#' @importFrom utils read.csv
get_csv_data <- function(url, query, ..., verbose = FALSE) {
  if (nchar(jsonlite::toJSON(query)) >= 2048)
    res <- httr::POST(url, httr::accept("text/csv"), body = query, encode = "form")
  else
    res <- httr::GET(url, httr::accept("text/csv"), query = query)
  stop_for_pk_status(res)
  out <- httr::content(res, as = "text")

  mssg(verbose, "Parse CVS ...")
  read.csv(textConnection(out), ...)
}

#' @importFrom jsonlite toJSON
#' @importFrom httr GET POST content
#' @importFrom RNeXML nexml_read
get_nexml_data <- function(url, query, verbose = FALSE) {
  if (nchar(jsonlite::toJSON(query)) >= 2048)
    res <- httr::POST(url, body = query, encode = "form")
  else
    res <- httr::GET(url, query = query)
  stop_for_pk_status(res)
  # if passing parsed XML to RNeXML, it needs to be in classes of the XML
  # package, but httr::content now uses the xml2 package for parsing text/xml
  out <- httr::content(res, as = "text")

  mssg(verbose, "Parse NeXML ...")
  RNeXML::nexml_read(out)
}

pkb_api <- function(...) {
  path <- paste(..., sep = "/")
  if (! startsWith(path, "/")) path <- paste0("/", path)
  paste0("https://kb.phenoscape.org/api", path)
}

#' Creates a list of named query parameters
#'
#' Several Phenoscape KB API endpoints use a form-like parameter list for
#' filtering (limiting), or, in the case of relationships, expanding, the
#' query result. This function aids in preparing the query string for these
#' endpoints. It is internal to the package.
#' @param ... any combination of zero or more parameters from `entity`,
#'   `quality`, `taxon`, and `study`. Any provided with value `NA` will be
#'   ignored. Entity, quality, and taxon will be resolved to IRI if not
#'   already provided as such.
#' @param includeRels character, in which case it is the relationship(s) for
#'   entities to be included ("part of", "historical homologous to",
#'   "serially homologous to"; unambiguous prefix strings are acceptable); or
#'   logical, in which case `FALSE` means no relationships, and `TRUE` means
#'   all available relationships. For legacy reasons, `NA` is treated synonymous
#'   with `FALSE`. The default is `FALSE`.
#' @param verbose logical, whether to print messages when potentially time consuming
#'   operations are run.
#' @return A list of named query parameters suitatoble for several form-like
#'   query endpoints in the Phenoscape KB API.
pkb_args_to_query <- function(...,
                              includeRels = FALSE,
                              verbose = FALSE) {
  relChoices <- c("part of", "historical homologous to", "serially homologous to")
  paramNames <- c("part of" = "parts",
                  "historical homologous to" = "historical_homologs",
                  "serially homologous to" = "serial_homologs",
                  "entity" = "entity",
                  "quality" = "quality",
                  "taxon" = "in_taxon",
                  "study" = "publication")
  ont_lookups <- c("entity" = "anatomy",
                   "quality" = "PATO",
                   "taxon" = "taxon")
  # determine relationships and set corresponding query parameters
  if (all(is.na(includeRels))) includeRels <- FALSE # treat NA the same as FALSE
  if (is.logical(includeRels)) {
    if (includeRels)
      includeRels <- relChoices
    else
      includeRels <- c()
  } else {
    includeRels <- match.arg(includeRels, relChoices, several.ok = TRUE)
  }
  queryseq <- as.list(rep("true", times = length(includeRels)))
  if (length(includeRels) > 0) {
    names(queryseq) <- unname(paramNames[match(includeRels, names(paramNames))])
  }

  # entity, quality, taxon, study etc
  argList <- list(...)
  queryseq <- c(queryseq,
                sapply(names(argList[!is.na(argList)]),
                       function(x) {
                         # parameter name
                         param <- unname(paramNames[x])
                         # parameter value, IRI lookup if necessary
                         paramVal <- argList[[x]]
                         ont <- ont_lookups[x]
                         if (! is.na(ont))
                           paramVal <- pk_get_iri(paramVal, as = ont, verbose = verbose)
                         names(paramVal) <- param
                         paramVal
                       },
                       USE.NAMES = FALSE))
  queryseq
}