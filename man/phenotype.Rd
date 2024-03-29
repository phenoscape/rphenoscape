% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phen_obj.R
\name{phenotype}
\alias{phenotype}
\alias{as.phenotype}
\alias{as.phenotype.data.frame}
\alias{is.phenotype}
\alias{is_valid_phenotype}
\alias{charstates}
\alias{chars}
\alias{print.phenotype}
\title{Phenotype Objects}
\usage{
as.phenotype(x, withTaxa = FALSE, ...)

\method{as.phenotype}{data.frame}(x, ...)

is.phenotype(x)

is_valid_phenotype(x)

charstates(x)

chars(x)

\method{print}{phenotype}(x, ...)
}
\arguments{
\item{x}{an object of type "phenotype" or coercible to it, or to be tested
for being of type "phenotype"}

\item{withTaxa}{logical. If TRUE taxa exhibiting the phenotype will be
available through the phenotype object at key "taxa". Default is FALSE, because
obtaining taxa requires an additional query per object.}

\item{...}{additional parameters where applicable; ignored for printing}
}
\value{
\code{as.phenotype} returns an object of type "phenotype", or a list of such objects
if the object to be coerced had multiple elements (if a vector) or rows (if a
data.frame). A phenotype object has properties "id" (ID, i.e., IRI of the
phenotype), "label" (label of the phenotype if one exists), "states" (a
data.frame of the character states to which the phenotype is linked, see value
for \code{charstates}), and "eqs" (the EQ expression components as a list with keys
"entities", "qualities", and "related_entities"). If \code{withTaxa} is TRUE,
there will also be a key "taxa" (a data.frame with columns "id" and "label").

\code{is.phenotype} returns TRUE if the object is of type "phenotype" and FALSE
otherwise.

\code{is_valid_phenotype} returns a logical vector of the same length as the input
array of objects, with TRUE for those objects in the list that are of type
"phenotype" and correspond to a phenotype in the database.

\code{charstates} returns a data.frame. If called with a single phenotype object
(or an object that coerces to one), the data.frame has columns "id" and "label" (for the
character state), "character.id" and "character.label" (IRI and label
of the character), and "study.id" and "study.label" (IRI and short label for
the study to which the character and state belong). If called with a list of
phenotype objects (or objects that coerce to such a list), the data.frame will
include the character states from all phenotypes in the list. In this case,
the character state columns will be "state.id" and "state.label", respectively,
and there will be two additional columns, "phenotype.id" and "phenotype.label".

\code{chars} returns  a data.frame with collumns "character.id" and "character.label"
(IRI and label of the character), and "study.id" and "study.label" (IRI and
short label for the study to which the character and state belong).
}
\description{
\code{as.phenotype} creates an object (or a list of objects) of type "phenotype".
The object to be coerced can be a character vector (of IRIs), or a data.frame.
In the latter case, there must be a column "id" with the IRIs of phenotypes.
If the object is already of type "phenotype", it is passed through unchanged.

\code{is.phenotype} tests whether an object is of type "phenotype"

\code{is_valid_phenotype} tests which of the objects in the list are valid
phenotype objects, and returns a logical vector of the same length as \code{x}.
An object is a valid phenotype object if it is of type "phenotype" and its
ID has been found in the database.

\code{charstates} extracts the character states from the phenotype object (or an
object coercible to phenotype)

\code{chars} extracts the (non-redundant) characters from the phenotype object (or an object
coercible to phenotype).

\code{print} pretty-prints objects of type "phenotype"
}
\details{
Create and test objects of type "phenotype", and extract properties from them.
}
\examples{
# query for a set of phenotypes (IDs and their labels)
phens <- get_phenotypes(entity = "basihyal bone")
nrow(phens)
# turn one into a phenotype object
obj <- as.phenotype(phens[3, "id"])
class(obj)
obj

# optionally include taxa exhibiting the phenotype
as.phenotype(phens[3,], withTaxa = TRUE)
# full list of taxa:
as.phenotype(phens[3,], withTaxa = TRUE)$taxa

# can also coerce entire list at once
objs <- as.phenotype(phens)
class(objs)
length(objs)
all(sapply(objs, is.phenotype))
objs[[3]]

# extract character states and (non-redundant) characters
charstates(obj)
chars(obj)

# IDs that don't resolve still yield an object, but is not valid
suppressWarnings(obj <- as.phenotype("http://foo"))
is.phenotype(obj)
is_valid_phenotype(obj)
}
