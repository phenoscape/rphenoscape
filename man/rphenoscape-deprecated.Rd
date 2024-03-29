% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{rphenoscape-deprecated}
\alias{rphenoscape-deprecated}
\alias{pk_get_iri}
\alias{pk_get_study_list}
\alias{pk_taxon_class}
\alias{pk_anatomical_class}
\alias{pk_phenotype_class}
\alias{pk_get_ontotrace_xml}
\alias{pk_get_study_xml}
\alias{pk_is_descendant}
\alias{pk_is_ancestor}
\alias{pk_taxon_detail}
\alias{pk_anatomical_detail}
\alias{pk_phenotype_detail}
\alias{pk_gene_detail}
\alias{pk_is_extinct}
\alias{pk_get_ontotrace}
\alias{pk_get_ontotrace_meta}
\alias{pk_get_study}
\alias{pk_get_study_meta}
\title{Deprecated functions in \pkg{rphenoscape}.}
\usage{
pk_get_iri(...)

pk_get_study_list(...)

pk_taxon_class(x, verbose = TRUE)

pk_anatomical_class(x, verbose = TRUE)

pk_phenotype_class(x, verbose = TRUE)

pk_get_ontotrace_xml(
  taxon,
  entity,
  relation = "part of",
  variable_only = TRUE,
  strict = TRUE
)

pk_get_study_xml(study_ids)

pk_is_descendant(term, candidates, includeRels = c("none", "part_of"))

pk_is_ancestor(term, candidates, includeRels = c("none", "part_of"))

pk_taxon_detail(term, verbose = FALSE)

pk_anatomical_detail(term, verbose = FALSE)

pk_phenotype_detail(term, verbose = FALSE)

pk_gene_detail(term, taxon = NA, verbose = FALSE)

pk_is_extinct(taxon, verbose = FALSE)

pk_get_ontotrace(nex)

pk_get_ontotrace_meta(nex)

pk_get_study(nexmls)

pk_get_study_meta(nexmls)
}
\description{
The functions listed below are deprecated and will become defunct in
the near future. When possible, alternative functions with identical or similar
functionality are mentioned. Where the signature of the alternative (new)
function differs significantly from that of the deprecated one, help pages for
the deprecated functions are available at \code{help("<function>-deprecated")}.
}
\section{\code{pk_get_iri}}{

For \code{pk_get_iri()} use \code{\link[=get_term_iri]{get_term_iri()}} instead.
}

\section{\code{pk_get_study_list}}{

For \code{pk_get_study_list()} use \code{\link[=get_studies]{get_studies()}} instead. Note that \code{get_studies()}
returns an empty result set if nothing was found, in contrast to \code{pk_get_study_list},
which returns FALSE in that case (and prints a message).
}

\section{\code{pk_taxon_class}}{

For \code{pk_taxon_class()} use \code{\link[=term_classification]{term_classification()}} with as="taxon" instead.
Note that \code{pk_taxon_class()} defaults verbose to TRUE instead of FALSE as
\code{term_classification()} does. Also \code{pk_taxon_class()} return value has the legacy \verb{@id} name.
}

\section{\code{pk_anatomical_class}}{

For \code{pk_anatomical_class()} use \code{\link[=term_classification]{term_classification()}} with as="anatomy" instead.
Note that \code{pk_anatomical_class()} defaults verbose to TRUE instead of FALSE as
\code{term_classification()} does. Also \code{pk_anatomical_class()} return value has the legacy \verb{@id} name.
}

\section{\code{pk_phenotype_class}}{

For \code{pk_phenotype_class()} use \code{\link[=term_classification]{term_classification()}} with as="pato" instead.
Note that \code{pk_phenotype_class()} defaults verbose to TRUE instead of FALSE as
\code{term_classification()} does. Also \code{pk_phenotype_class()} return value has the legacy \verb{@id} name.
}

\section{\code{pk_get_ontotrace_xml}}{

For \code{pk_get_ontotrace_xml()} use \code{\link[=get_ontotrace_data]{get_ontotrace_data()}} instead.
}

\section{\code{pk_get_study_xml}}{

For \code{pk_get_study_xml()} use \code{\link[=get_study_data]{get_study_data()}} instead.
Note that \code{pk_get_study_xml()} maintains legacy functionality of displaying messages. \code{get_study_data()}
has a verbose argument to control displaying messages.
}

\section{\code{pk_is_descendant}}{

For \code{pk_is_descendant()} use \code{\link[=is_descendant]{is_descendant()}} instead.
}

\section{\code{pk_is_ancestor}}{

For \code{pk_is_ancestor()} use \code{\link[=is_ancestor]{is_ancestor()}} instead.
}

\section{\code{pk_taxon_detail}}{

For \code{pk_taxon_detail()} use \code{\link[=taxon_info]{taxon_info()}} instead.
}

\section{\code{pk_anatomical_detail}}{

For \code{pk_anatomical_detail()} use \code{\link[=anatomy_term_info]{anatomy_term_info()}} instead.
}

\section{\code{pk_phenotype_detail}}{

For \code{pk_phenotype_detail()} use \code{\link[=phenotypic_quality_term_info]{phenotypic_quality_term_info()}} instead.
}

\section{\code{pk_gene_detail}}{

For \code{pk_gene_detail()} use \code{\link[=gene_info]{gene_info()}} instead.
}

\section{\code{pk_is_extinct}}{

For \code{pk_is_extinct()} use \code{\link[=is_extinct]{is_extinct()}} instead.
}

\section{\code{pk_get_ontotrace}}{

For \code{pk_get_ontotrace()} use \code{\link[=get_char_matrix]{get_char_matrix()}} instead.
}

\section{\code{pk_get_ontotrace_meta}}{

For \code{pk_get_ontotrace_meta()} use \code{\link[=get_char_matrix_meta]{get_char_matrix_meta()}} instead.
}

\section{\code{pk_get_study}}{

For \code{pk_get_study()} use \code{\link[=get_char_matrix]{get_char_matrix()}} with lapply instead.
Note that \code{pk_get_study()} always displays messages instead of using the
verbose parameter as \code{get_char_matrix()} does.
}

\section{\code{pk_get_study_meta}}{

For \code{pk_get_study_meta()} use \code{\link[=get_char_matrix_meta]{get_char_matrix_meta()}} with lapply instead.
}

\keyword{internal}
