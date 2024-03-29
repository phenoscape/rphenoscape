% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutual_exclusivity.R
\name{mutual_exclusivity_pairwise}
\alias{mutual_exclusivity_pairwise}
\title{Determine whether two phenotypes are mutually exclusive}
\usage{
mutual_exclusivity_pairwise(
  phenotype.a,
  phenotype.b,
  studies = NULL,
  charstates = NULL,
  quality_opposites = NULL
)
}
\arguments{
\item{phenotype.a}{character or phenotype object, phenotype ID (IRI) if character, or
a phenotype object obtained by passing phenotype ID to as.phenotype() function.}

\item{phenotype.b}{character or phenotype object, phenotype ID (IRI) if character, or
a phenotype object obtained by passing phenotype ID to as.phenotype() function.}

\item{studies}{character, a vector of study IDs. This is an optional
parameter that acts as a filter in case the determination of
mutual exclusivity is to be based only on the evidence found in a particular set of studies.
The default is NULL, which means
that all studies present in the KB database will be considered for evidence.}

\item{charstates}{dataframe, a dataframe obtained from \code{\link[=charstates]{charstates()}} by
passing a list of the two phenotypes as argument. This optional parameter exists to speed up
the computation for the \code{\link[=mutually_exclusive]{mutually_exclusive()}} function since \code{mutually_exclusive()}
repeatedly calls mutual_exclusivity_pairwise(). The default is NULL. \emph{Note that passing this argument
but doing so incorrectly can result in wrong output.}}

\item{quality_opposites}{dataframe, an optional dataframe containing columns
"quality.a" and "quality.b" to denote pairs of phenotypic quality terms, in
the form of their term IRIs, to be considered opposites of each others.
See documentation under \code{\link[=mutually_exclusive]{mutually_exclusive()}} for more details.}
}
\value{
A character (string), the mutual exclusivity type among the two phenotypes.
See \code{\link[=mutually_exclusive]{mutually_exclusive()}} for documentation on the possible values, although note
that this function returns these as a character vector, not levels of an ordered factor.
}
\description{
Determines whether the data in the KB includes evidence for mutual exclusivity
between two given phenotypes.
}
\details{
See \code{\link[=mutually_exclusive]{mutually_exclusive()}} for details on which links in the data are considered
as strong or weak evidence for mutual exclusivity and compatibility.
}
