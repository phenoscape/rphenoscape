<!-- README.md is generated from README.rmd. Please edit that file -->
rphenoscape: Semantically rich phenotypic traits from the Phenoscape Knowledgebase
==================================================================================

[![Build Status](https://travis-ci.org/phenoscape/rphenoscape.svg?branch=master)](https://travis-ci.org/phenoscape/rphenoscape)

-   Maintainer: Hong Xu
-   Author: Hong Xu, Hilmar Lapp

Most of the services provided with [Phenoscape Knowledgebase web API](http://kb.phenoscape.org/apidocs/) return data in JSON format, plain text (usually tab-delimited), and NeXML. This package facilitates the interfacing to the Phenoscape Knowledgebase for searching ontology terms, retrieving term info, and querying data matrices.

Getting Started
---------------

The development version of RPhenoscape is available on [Github](www.github.com/phenoscape/rphenoscape). With the `devtools` package installed on your system, RPhenoscape can be installed using:

``` r
devtools::install_github("phenoscape/rphenoscape", build_opts=c("--no-manual"))
library(rphenoscape)
```

The option `build_opts` ensures that the vignettes will be built and installed as well. This will require a recent version of the knitr and rmarkdown packages. You can install these beforehand, or include the option `dependencies=TRUE`. The latter will also install packages otherwise only needed for testing and for generating the help pages, which, if you don't develop packages yourself, may be much more than you need.

Package Usage Information
-------------------------

The functionality and operations of the RPhenoscape package can be viewed on the dedicated package website: <http://rphenoscape.phenoscape.org/>

Use cases described there include the following:

-   Using Ontotrace to obtain a character matrix
-   Obtaining the character matrices for studies published for a taxonomic clade
-   Subsetting matrices by taxonomic subgroup or anatomical part
-   Searching for details for a given taxon

------------------------------------------------------------------------

[![Phenoscape Knowledgebase](https://wiki.phenoscape.org/wg/phenoscape/images/f/f6/Phenoscape_Logo.png)](http://kb.phenoscape.org)
