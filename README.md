
# rphenoscape: R package to make phenotypic traits from the Phenoscape Knowledgebase available from within R.
* Maintainer: Hong Xu
* Author: Hong Xu

Most of the services provided with Phenoscape Knowledgebase web API return data in JSON format, plain text (usually tab-delimited), and NeXML. This package facilitates the interfacing to the the Phenoscape Knowledge for searching ontology terms, retrieving term info, and querying data matrices. 

## Getting Started
The development version of rphenoscape is available on [Github](www.github.com/xu-hong/rphenoscape). With the `devtools` package installed on your system, rphenoscape can be installed using:








```r
library(devtools)
install_github("rphenoscape", "xu-hong")
library(rphenoscape)
```

### Term Search 

Search for details for a given taxon:

```r
pk_taxon_detail("Coralliozetus")
```

```
Source: local data frame [1 x 5]

                                         @id         label extinct
                                       (chr)         (chr)   (lgl)
1 http://purl.obolibrary.org/obo/VTO_0042955 Coralliozetus   FALSE
Variables not shown: rank.@id (chr), rank.label (chr)
```

Search for details for a given anatomical structure:

```r
pk_anatomical_detail("basihyal bone")
```

```
Source: local data frame [1 x 3]

                                            @id         label
                                          (chr)         (chr)
1 http://purl.obolibrary.org/obo/UBERON_0011618 basihyal bone
Variables not shown: definition (chr)
```

Search for details for a given gene name:

```r
pk_gene_detail("socs5")
```

```
                                                @id  label matchType
1             http://xenbase.org/XB-GENEPAGE-479592  socs5     exact
2 http://www.informatics.jax.org/marker/MGI:2385459  Socs5     exact
3               http://zfin.org/ZDB-GENE-061013-408 socs5a   partial
4                http://zfin.org/ZDB-GENE-080722-18 socs5b   partial
                                       taxon.@id  taxon.label
1  http://purl.obolibrary.org/obo/NCBITaxon_8353      Xenopus
2 http://purl.obolibrary.org/obo/NCBITaxon_10090 Mus musculus
3  http://purl.obolibrary.org/obo/NCBITaxon_7955  Danio rerio
4  http://purl.obolibrary.org/obo/NCBITaxon_7955  Danio rerio
```

Miscellaneous methods:
Test if a given taxon is extinct:

```r
pk_is_extinct("Fisherichthys")
```

```
[1] TRUE
```
Get the ancestors/descendant of a taxon from a given list:

```r
pk_is_descendant("Halecostomi", c("Halecostomi", "Icteria", "Sciaenidae"))
```

```
[1] FALSE FALSE  TRUE
```

```r
pk_is_ancestor("Sciaenidae", c("Halecostomi", "Abeomelomys", "Sciaenidae"))
```

```
[1]  TRUE FALSE FALSE
```


## Ontotrace Matrix
## Study Matrix

