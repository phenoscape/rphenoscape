#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
get_json_data <- function(url, query, verbose = FALSE) {
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
  jsonlite::fromJSON(out, simplifyVector = TRUE, flatten = TRUE)
}
pk_GET <- get_json_data

#' @importFrom httr GET content
#' @importFrom utils read.csv
get_csv_data <- function(url, query, ..., verbose = FALSE) {
  if (nchar(jsonlite::toJSON(query)) >= 2048)
    res <- httr::POST(url, httr::accept("text/csv, text/plain"), body = query, encode = "form")
  else
    res <- httr::GET(url, httr::accept("text/csv, text/plain"), query = query)
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