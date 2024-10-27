<!-- README.md is generated from README.rmd. Please edit that file -->

# RPhenoscape: Semantically rich phenotypic traits from the Phenoscape Knowledgebase

[![Build
Status](https://circleci.com/gh/phenoscape/rphenoscape.svg?style=svg)](https://app.circleci.com/pipelines/github/phenoscape/rphenoscape)

-   Author: Hong Xu, Hilmar Lapp, John Bradley
-   Maintainer: Hilmar Lapp

Most of the services provided with the [Phenoscape Knowledgebase
API](https://kb.phenoscape.org/api/v2/docs/) return data in JSON format,
plain text (usually tab-delimited), and NeXML. This package facilitates
interfacing with the Phenoscape Knowledgebase for searching ontology
terms, retrieving term info, and querying data matrices.

## Installation

The development version of RPhenoscape is available on
[Github](https://github.com/phenoscape/rphenoscape). It has not yet been
released to [CRAN](https://cran.r-project.org). To install RPhenoscape
from Github, use the `install_github()` function in the `remotes`
package (which can be installed from CRAN using `install.packages()`):

``` r
remotes::install_github("phenoscape/rphenoscape")
```

By default, this will skip building the package vignettes. To build and
install package vignettes as well, you must have recent versions of the
`knitr` and `rmarkdown` packages installed, and use additional
parameters, depending on the version of the `remotes` package you have
installed, to request building vignettes.

``` r
packageVersion("remotes")
```

    ## [1] '2.4.2'

If the version is 2.1.0 or higher, use the `build_vignettes` parameter
(which by default is FALSE):

``` r
remotes::install_github("phenoscape/rphenoscape", build_vignettes = TRUE)
```

If the version of `remotes` is lower, use the `build_opts` parameter:

``` r
remotes::install_github("phenoscape/rphenoscape", build_opts=c("--no-manual"))
```

Once installed, the package can be loaded (“attached”) as any other R
package:

``` r
library(rphenoscape)
```

### Upgrading from an earlier version

Until v1.x is released, upgrades from one minor (patch) release to
another, i.e., from v0.x.n to v0.x.m where m \> n, can be expected to be
fully backwards compatible. Upgrading to a new major release, i.e., from
v0.n.x to v0.m.y, where m \> n, should be expected to be at least in
part backwards incompatible.

A backwards incompatible change may result in deprecation warnings being
issued, or errors when using a no longer supported function. Whenever
possible, we will try to provide a deprecation path. Function or
arguments that are deprecated in one major release may be dropped and
thus result in an error in the next major release.

Specifically, upgrading from v0.2.x to v0.3.y includes a number of
backwards incompatible changes, including a large-scale streamlining and
restructuring of the function API.

## Package Usage Information

The functionality and operations of the RPhenoscape package can be
viewed on the dedicated package website:
<http://rphenoscape.phenoscape.org/>

[Use cases](https://github.com/phenoscape/rphenoscape/wiki/User-Stories)
described there include the following:

-   Using OntoTrace to obtain a character matrix
-   Obtaining the character matrices for studies published for a
    taxonomic clade
-   Subsetting matrices by taxonomic subgroup or anatomical part
-   Searching for details for a given taxon

By definition, most functions in this package connect to the [Phenoscape
Knowledgebase API](https://kb.phenoscape.org/api/v2/docs/). The base URL
path to the API endpoints can changed from the default by setting the
`PHENOSCAPE_API` environment variable. (See [R
Startup](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html)
and [RStudio support on
.Renviron](https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf)
on how to control environment variables at R session startup.)

------------------------------------------------------------------------

[![Phenoscape
Knowledgebase](https://wiki.phenoscape.org/wg/phenoscape/images/f/f6/Phenoscape_Logo.png)](http://kb.phenoscape.org)
