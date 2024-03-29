% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study.R
\name{get_study_data}
\alias{get_study_data}
\title{get_study_data}
\usage{
get_study_data(study_ids, verbose = FALSE)
}
\arguments{
\item{study_ids, }{a list of study IDs.}

\item{verbose}{logical: optional. If TRUE, prints messages prior to potentially
time-consuming operations. Default is FALSE.}
}
\value{
A list of \link[RNeXML:nexml-class]{nexml} objects
}
\description{
get_study_data
}
\examples{
\dontrun{
slist <- get_studies(taxon = "Ameiurus", entity = "pelvic splint")
nex_list <- get_study_data(slist$id)
}
}
