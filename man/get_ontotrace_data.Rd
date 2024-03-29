% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ontotrace.R
\name{get_ontotrace_data}
\alias{get_ontotrace_data}
\title{Obtain a synthetic presence/absence matrix}
\usage{
get_ontotrace_data(
  taxon,
  entity,
  relation = "part of",
  variable_only = TRUE,
  strict = TRUE,
  subsume = TRUE
)
}
\arguments{
\item{taxon}{character or object of type "owlmn", required. A vector of taxon names
or a single OWL Manchester expression object. \code{\link[=as.owl]{as.owl()}} can be used to create a
OWL Manchester expression object.}

\item{entity}{character or object of type "owlmn", required. A vector of entity names
or a single OWL Manchester expression object. \code{\link[=as.owl]{as.owl()}} can be used to create a
OWL Manchester expression object.}

\item{relation}{character string, optional.
The relationship to the entities to be included in the result. Must be
either "part of" or "develops from", or set to NA to disable.
Default is "part of".}

\item{variable_only}{logical, optional.
Whether to only include characters that are variable across the selected
set of taxa. Default is TRUE.}

\item{strict}{logical, optional. Whether or not to treat any failure to resolve
any taxon or entity names to IRI as input error. Resolution by partial or
other inexact match results in a warning, but is not considered a failure.
If FALSE, query execution will continue with the taxon and entity terms
that did resolve to IRI. Default is TRUE, meaning any resolution failure will
result in an error. \emph{All} taxon or entity names failing resolution to IRIs will
result in an error regardless.}

\item{subsume}{logical, optional. If TRUE (the default), taxon and entity parameters include their logical descendants.
Ignored if entity or taxon is a OWL Manchester expression object (which always include logical descendants).
If set to FALSE, \emph{only} data for the given taxa and entities will be returned.}
}
\value{
\link[RNeXML:nexml-class]{RNeXML::nexml} object
}
\description{
Queries the Phenoscape KB for a synthetic presence/absence character matrix
for the given taxa and anatomical entities, and returns the result as a
\link[RNeXML:nexml-class]{nexml} object (from the RNeXML package).
}
\details{
The character matrix includes both asserted and logically inferred states. The
query always includes all subclasses of both taxa and entities, and by default
also includes all parts of the entities. See parameter \code{relation} for changing
this. By default, only characters that are variable across the resulting taxa
are included; use \code{variable_only} to change this.
}
\examples{
\dontrun{
# one taxon (including subclasses), one entity (including subclasses and 
# by default its parts)
nex <- get_ontotrace_data(taxon = "Ictalurus", entity = "fin")

# same as above, except do not include parts or other relationships (fin
# presence/absence does not vary across Ictalurus, hence need to allow
# non-variable characters)
nex <- get_ontotrace_data(taxon = "Ictalurus", entity = "fin",
                          relation = NA, variable_only = FALSE)

# instead of parts, include entities in develops_from relationship to the query entity
nex <- get_ontotrace_data(taxon = "Ictalurus", entity = "paired fin bud",
                          relation = "develops from", variable_only = FALSE)

# query with multiple taxa, and/or multiple entities:
nex <- get_ontotrace_data(taxon = c("Ictalurus", "Ameiurus"),
                          entity = c("pectoral fin", "pelvic fin"))

# query disabling subsumption with multiple taxa, and/or multiple entities
get_ontotrace_data(
    taxon = c("Ictalurus australis", "Ictalurus balsanus"),
    entity = c("anterior dentation of pectoral fin spine", "pelvic splint"),
    subsume = FALSE)

# query using taxon and/or entity owl expressions
taxon_owl <- as.owl("<http://purl.obolibrary.org/obo/VTO_0036217>")
entity_owl <- as.owl("<http://purl.obolibrary.org/obo/UBERON_0008897> or
   (<http://purl.obolibrary.org/obo/BFO_0000050> some
    <http://purl.obolibrary.org/obo/UBERON_0008897>)")
nex <- get_ontotrace_data(taxon = taxon_owl, entity = entity_owl)

# query using taxon and/or entity owl expressions resolved from label expressions
taxon_owl <- as.owl("Ictalurus", usesLabels = TRUE)
entity_owl <- as.owl("'fin' or ('part of' some 'fin')", usesLabels = TRUE)
nex <- get_ontotrace_data(taxon = taxon_owl, entity = entity_owl)

# Use the RNeXML API to obtain the character matrix etc:
m <- RNeXML::get_characters(nex)
dim(m)      # number of taxa and characters
rownames(m) # taxon names
colnames(m) # characters (entity names)

}
}
