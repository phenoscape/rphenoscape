
# rphenoscape: R package to make phenotypic traits from the Phenoscape Knowledgebase available from within R.
* Maintainer: Hong Xu
* Author: Hong Xu, Hilmar Lapp

Most of the services provided with Phenoscape Knowledgebase web API return data in JSON format, plain text (usually tab-delimited), and NeXML. This package facilitates the interfacing to the the Phenoscape Knowledge for searching ontology terms, retrieving term info, and querying data matrices. 

## Getting Started
The development version of rphenoscape is available on [Github](www.github.com/xu-hong/rphenoscape). With the `devtools` package installed on your system, rphenoscape can be installed using:








```r
library(devtools)
install_github("rphenoscape", "xu-hong")
library(rphenoscape)
```

## Term Search 

Search for details for a given taxon:

```r
pk_taxon_detail("Coralliozetus")
```

```
## Source: local data frame [1 x 5]
## 
##                                          @id         label extinct
##                                        (chr)         (chr)   (lgl)
## 1 http://purl.obolibrary.org/obo/VTO_0042955 Coralliozetus   FALSE
## Variables not shown: rank.@id (chr), rank.label (chr)
```

Search for details for a given anatomical structure:

```r
pk_anatomical_detail("basihyal bone")
```

```
## Source: local data frame [1 x 3]
## 
##                                             @id         label
##                                           (chr)         (chr)
## 1 http://purl.obolibrary.org/obo/UBERON_0011618 basihyal bone
## Variables not shown: definition (chr)
```

Search for details for a given gene name:

```r
pk_gene_detail("socs5")
```

```
##                                                 @id  label matchType
## 1             http://xenbase.org/XB-GENEPAGE-479592  socs5     exact
## 2 http://www.informatics.jax.org/marker/MGI:2385459  Socs5     exact
## 3               http://zfin.org/ZDB-GENE-061013-408 socs5a   partial
## 4                http://zfin.org/ZDB-GENE-080722-18 socs5b   partial
##                                        taxon.@id  taxon.label
## 1  http://purl.obolibrary.org/obo/NCBITaxon_8353      Xenopus
## 2 http://purl.obolibrary.org/obo/NCBITaxon_10090 Mus musculus
## 3  http://purl.obolibrary.org/obo/NCBITaxon_7955  Danio rerio
## 4  http://purl.obolibrary.org/obo/NCBITaxon_7955  Danio rerio
```

#### Miscellaneous methods:
Resolve a given term to its IRI:

```r
pk_get_iri("Coralliozetus", "vto")
```

```
## [1] "http://purl.obolibrary.org/obo/VTO_0042955"
```

```r
pk_get_iri("basihyal bone", "uberon")
```

```
## [1] "http://purl.obolibrary.org/obo/UBERON_0011618"
```

Test if a taxon is extinct:

```r
pk_is_extinct("Fisherichthys")
```

```
## [1] TRUE
```
Get the ancestors/descendants of a taxon from a given list:

```r
pk_is_descendant("Halecostomi", c("Halecostomi", "Icteria", "Sciaenidae"))
```


```
## [1] FALSE FALSE  TRUE
```

```r
pk_is_ancestor("Sciaenidae", c("Halecostomi", "Abeomelomys", "Sciaenidae"))
```


```
## [1]  TRUE FALSE FALSE
```


## Ontotrace Matrix
First get the NeXML object of the search result.

```r
nex <- pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus"), entity = "fin spine")
```

Then retrieve wanted information from the NeXML object.  
Get OntoTrace Matrix:

```r
pk_get_ontotrace(nex)
```

```
## Source: local data frame [15 x 5]
## 
##                      taxa         otu
##                     (chr)       (chr)
## 1       Ameiurus brunneus VTO_0036273
## 2          Ameiurus catus VTO_0036275
## 3          Ameiurus melas VTO_0036272
## 4        Ameiurus natalis VTO_0036274
## 5      Ameiurus nebulosus VTO_0036278
## 6  Ameiurus platycephalus VTO_0036276
## 7   Ameiurus serracanthus VTO_0036277
## 8     Ictalurus australis VTO_0061495
## 9      Ictalurus balsanus VTO_0036221
## 10      Ictalurus dugesii VTO_0061497
## 11     Ictalurus furcatus VTO_0036223
## 12        Ictalurus lupus VTO_0036220
## 13    Ictalurus mexicanus VTO_0061498
## 14       Ictalurus pricei VTO_0036218
## 15    Ictalurus punctatus VTO_0036225
## Variables not shown: otus (chr), anterior dentation of pectoral fin spine
##   (int), anterior distal serration of pectoral fin spine (dbl)
```
Get meta data:

```r
pk_get_ontotrace_meta(nex)
```

```
## $id_taxa
## Source: local data frame [15 x 4]
## 
##                     label                                       href
##                     (chr)                                      (chr)
## 1     Ictalurus punctatus http://purl.obolibrary.org/obo/VTO_0036225
## 2      Ictalurus balsanus http://purl.obolibrary.org/obo/VTO_0036221
## 3       Ameiurus brunneus http://purl.obolibrary.org/obo/VTO_0036273
## 4      Ictalurus furcatus http://purl.obolibrary.org/obo/VTO_0036223
## 5     Ictalurus mexicanus http://purl.obolibrary.org/obo/VTO_0061498
## 6         Ictalurus lupus http://purl.obolibrary.org/obo/VTO_0036220
## 7      Ameiurus nebulosus http://purl.obolibrary.org/obo/VTO_0036278
## 8       Ictalurus dugesii http://purl.obolibrary.org/obo/VTO_0061497
## 9          Ameiurus melas http://purl.obolibrary.org/obo/VTO_0036272
## 10  Ameiurus serracanthus http://purl.obolibrary.org/obo/VTO_0036277
## 11 Ameiurus platycephalus http://purl.obolibrary.org/obo/VTO_0036276
## 12    Ictalurus australis http://purl.obolibrary.org/obo/VTO_0061495
## 13         Ameiurus catus http://purl.obolibrary.org/obo/VTO_0036275
## 14       Ictalurus pricei http://purl.obolibrary.org/obo/VTO_0036218
## 15       Ameiurus natalis http://purl.obolibrary.org/obo/VTO_0036274
## Variables not shown: otu (chr), otus (chr)
## 
## $id_entities
## Source: local data frame [2 x 3]
## 
##                                             label
##                                             (chr)
## 1 anterior distal serration of pectoral fin spine
## 2        anterior dentation of pectoral fin spine
## Variables not shown: href (chr), char (chr)
```

## Study Matrix
Retrieve the list of studies.

```r
slist <- pk_get_study_list(taxon = "Ictalurus", entity = "fin")
```
Get the ReXML object from the study id.

```r
nex_list <- pk_get_study_xml(slist$id)
```
Retrieve the study matrix and corresponding meta data.

```r
pk_get_study(nex_list)
pk_get_study_meta(nex_list)
```



