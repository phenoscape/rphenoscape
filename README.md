<!-- README.md is generated from README.rmd. Please edit that file -->
RPhenoscape: Semantically rich phenotypic traits from the Phenoscape Knowledgebase
==================================================================================

[![Build Status](https://travis-ci.org/phenoscape/rphenoscape.svg?branch=master)](https://travis-ci.org/phenoscape/rphenoscape)

-   Author: Hong Xu, Hilmar Lapp
-   Maintainer: Hilmar Lapp

Most of the services provided with the [Phenoscape Knowledgebase web API](http://kb.phenoscape.org/apidocs/) return data in JSON format, plain text (usually tab-delimited), and NeXML. This package facilitates interfacing with the Phenoscape Knowledgebase for searching ontology terms, retrieving term info, and querying data matrices.

Installation
------------

The development version of RPhenoscape is available on [Github](http://github.com/phenoscape/rphenoscape). It has not yet been released to [CRAN](https://cran.r-project.org). To install RPhenoscape from Github, use the `install_github()` function in the `remotes` package (which can be installed from CRAN using `install.packages()`):

``` r
remotes::install_github("phenoscape/rphenoscape")
```

By default, this will skip building the package vignettes. To build and install package vignettes as well, you must have recent versions of the `knitr` and `rmarkdown` packages installed, and use additional parameters, depending on the version of the `remotes` package you have installed, to request building vignettes.

``` r
packageVersion("remotes")
```

    ## [1] '2.0.4'

If the version is 2.1.0 or higher, use the `build_vignettes` parameter (which by default is FALSE):

``` r
remotes::install_github("phenoscape/rphenoscape", build_vignettes = TRUE)
```

If the version of `remotes` is lower, use the `build_opts` parameter:

``` r
remotes::install_github("phenoscape/rphenoscape", build_opts=c("--no-manual"))
```

Once installed, the package can be loaded ("attached") as any other R package:

``` r
library(rphenoscape)
```

Package Usage Information
-------------------------

The functionality and operations of the RPhenoscape package can be viewed on the dedicated package website: <http://rphenoscape.phenoscape.org/>

[Use cases](https://github.com/phenoscape/rphenoscape/wiki/User-Stories) described there include the following:

-   Using OntoTrace to obtain a character matrix
-   Obtaining the character matrices for studies published for a taxonomic clade
-   Subsetting matrices by taxonomic subgroup or anatomical part
-   Searching for details for a given taxon

------------------------------------------------------------------------

[![Phenoscape Knowledgebase](https://wiki.phenoscape.org/wg/phenoscape/images/f/f6/Phenoscape_Logo.png)](http://kb.phenoscape.org)
