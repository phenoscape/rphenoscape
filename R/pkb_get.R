#' Get data from an API endpoint
#'
#' Obtains and parses data from JSON, CSV, and NeXML-returning API endpoints,
#' respectively.
#'
#' These are package-internal functions.
#' @param url character, the URL of the API endpoint to query
#' @param query list, the query parameters and values in the form of a named list
#' @param verbose logical, whether to print message when parsing results
#' @param ensureNames character, which column or list names to ensure are included
#'   in the result to be returned. If result returned by the API endpoint does
#'   not include them, they will be added, with NA values.
#' @param forceGET logical, whether to force using the HTTP GET method for API
#'   access regardless of request length. The default is FALSE, meaning queries
#'   exceeding a certain size for transmission will automatically use HTTP POST
#'   for accessing the KB API.
#' @param ... for `get_csv_data`, additional parameters to be passed on to
#'   [read.csv()][utils::read.csv()]
#' @return
#'   For `get_json_data`, a data frame or list, depending on the result of
#'   [jsonlite::fromJSON()].
#'
#'   For `get_csv_data`, a data frame.
#'
#'   For `get_nexml_data`, a [nexml][RNeXML::nexml] object.
#' @rdname get_data
#' @name get_data
#' @aliases pk_GET
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
get_json_data <- function(url, query,
                          verbose = FALSE, ensureNames = NULL, forceGET = FALSE) {
  if (forceGET || nchar(jsonlite::toJSON(query)) < 3072)
    res <- httr::GET(url, httr::accept_json(), httr::user_agent(ua()),
                     query = query)
  else
    res <- httr::POST(url, httr::accept_json(), httr::user_agent(ua()),
                      body = query, encode = "form")
  # some endpoints return HTTP 404 for failure to find data, though some
  # also (erroneously) do so for other reasons
  contLen <- res$headers$`content-length`
  if (res$status_code == 404 && (! is.null(contLen)) && contLen > 0) {
    # confirm by checking the error message
    err <- httr::content(res, as = "text")
    if (endsWith(err, "resource could not be found.")) return(NULL)
  }
  stop_for_pk_status(res)
  # some endpoints return zero content for failure to find data
  if ((! is.null(contLen)) && contLen == 0) return(NULL)

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

#' @rdname get_data
#' @importFrom httr GET content
#' @importFrom utils read.csv
get_csv_data <- function(url, query, ..., verbose = FALSE, forceGET = FALSE) {
  if (forceGET || nchar(jsonlite::toJSON(query)) < 2048)
    res <- httr::GET(url, httr::accept("text/csv"), httr::user_agent(ua()),
                     query = query)
  else
    res <- httr::POST(url, httr::accept("text/csv"), httr::user_agent(ua()),
                      body = query, encode = "form")
  stop_for_pk_status(res)
  out <- httr::content(res, as = "text")

  mssg(verbose, "Parse CVS ...")
  read.csv(textConnection(out), ...)
}

#' @rdname get_data
#' @importFrom jsonlite toJSON
#' @importFrom httr GET POST content
#' @importFrom RNeXML nexml_read
get_nexml_data <- function(url, query, verbose = FALSE, forceGET = FALSE) {
  if (forceGET || nchar(jsonlite::toJSON(query)) < 2048)
    res <- httr::GET(url, httr::user_agent(ua()), query = query)
  else
    res <- httr::POST(url, httr::user_agent(ua()), body = query, encode = "form")
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
  paste0(phenoscape_api(), path)
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
  # remove parameters not meant for us
  if (length(argList) > 0) {
    argList <- argList[! startsWith(names(argList), ".")]
    queryseq <- c(queryseq,
                  sapply(names(argList[!is.na(argList)]),
                         function(x) {
                           # parameter name
                           param <- unname(paramNames[x])
                           # parameter value, IRI lookup if necessary
                           paramVal <- argList[[x]]
                           ont <- ont_lookups[x]
                           if (! is.na(ont))
                             paramVal <- pk_get_iri(paramVal, as = ont,
                                                    verbose = verbose)
                           names(paramVal) <- param
                           paramVal
                         },
                         USE.NAMES = FALSE))
  }
  queryseq
}

#' @importFrom utils packageName
#' @importFrom utils packageVersion
ua <- local({
  .ua <- NA;
  function() {
    if (is.na(.ua)) {
      pkg <- utils::packageName()
      versions <- c(paste0("r-curl/", utils::packageVersion("curl")),
                    paste0("httr/", utils::packageVersion("httr")),
                    paste0(pkg, "/", utils::packageVersion(pkg)))
      .ua <<- paste0(versions, collapse = " ")
    }
    .ua
  }
})

phenoscape_api <- local({
  .api <- NA;
  function() {
    if (is.na(.api)) {
      .api <<- Sys.getenv("PHENOSCAPE_API", "https://kb.phenoscape.org/api")
    }
    .api
  }
})