---
output:
  html_document:
    keep_md: yes
---

# rphenoscape: R package to make phenotypic traits from the Phenoscape Knowledgebase available from within R.

[![Build Status](https://travis-ci.org/phenoscape/rphenoscape.svg?branch=master)](https://travis-ci.org/phenoscape/rphenoscape)

* Maintainer: Hong Xu
* Author: Hong Xu, Hilmar Lapp


Most of the services provided with [Phenoscape Knowledgebase web API](http://kb.phenoscape.org/apidocs/) return data in JSON format, plain text (usually tab-delimited), and NeXML. This package facilitates the interfacing to the Phenoscape Knowledge for searching ontology terms, retrieving term info, and querying data matrices. 


## Getting Started
The development version of rphenoscape is available on [Github](www.github.com/phenoscape/rphenoscape). With the `devtools` package installed on your system, rphenoscape can be installed using:








```r
library(devtools)
install_github("phenoscape/rphenoscape")
library(rphenoscape)
```


## Character Matrix via Ontotrace
Use Ontotrace to obtain a character matrix for a taxonomic clade and anatomical region of interest. 

The [Phenoscape Knowledgebase web API](http://kb.phenoscape.org/apidocs/#/OntoTrace/get_ontotrace) returns the evolutionary character matrix in NeXML format. The first step is to get the NeXML object using ```pk_get_ontotrace_xml``` method. 

```r
nex <- pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus"), entity = "fin spine")
```

```
## Warning in .nextMethod(obj = obj, from = from): NAs introduced by coercion
```
Then retrieve wanted information from the NeXML object.  
Get character matrix:

```r
(m <- pk_get_ontotrace(nex))
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
The character matrix can be integrated with other data, such as meta data which include taxon identifiers, character identifiers, etc.
Get meta data:

```r
(meta <- pk_get_ontotrace_meta(nex))
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

## Character Matrices for Studies
To obtain the character matrices for studies published for a taxonomic clade and anatomical region of interest.

First step is to retrieve the list of studies given a taxonomic clade and anatomical structures (returned as ```data.frame```).

```r
(slist <- pk_get_study_list(taxon = "Ictalurus australis", entity = "fin"))
```

```
## Source: local data frame [1 x 2]
## 
##                                                                            id
##                                                                         (chr)
## 1 https://scholar.google.com/scholar?q=The+Phylogeny+of+Ictalurid+Catfishes%3
## Variables not shown: label (chr)
```

Based off the study ids retained from previous step, get the evolutionary character matrix for each study id (in NeXML-format) using ```pk_get_study_xml```.

```r
(nex_list <- pk_get_study_xml(slist$id))
```

```
## ....This might take a while....
## https://scholar.google.com/scholar?q=The+Phylogeny+of+Ictalurid+Catfishes%3A+A+Synthesis+of+Recent+Work&btnG=&hl=en&as_sdt=0%2C42
## Parse NeXML....
```

```
## $`https://scholar.google.com/scholar?q=The+Phylogeny+of+Ictalurid+Catfishes%3A+A+Synthesis+of+Recent+Work&btnG=&hl=en&as_sdt=0%2C42`
## A nexml object representing:
##  	 0 phylogenetic tree blocks, where: 
##  	 block 1 contains NULL phylogenetic trees
## 	 block 0 contains  phylogenetic trees 
##  	 393 meta elements 
##  	 1 character matrices 
##  	 22 taxonomic units 
##  Taxa: 	 Ameiurus natalis, Ictalurus pricei, Ameiurus catus, Noturus flavus, Prietella phreatophila, Ictalurus dugesii ... 
## 
##  NeXML generated by RNeXML using schema version: 0.9 
##  size: 6.1 Mb
```

From the list of ReXML objects, retrieve the character matrices.

```r
study_matrix <- pk_get_study(nex_list)
```

```
## Map symbols to labels...
```

```r
study_matrix[[1]][1:5, 1:5]
```

```
##                 taxa                                      otu
## 2  Ameiurus brunneus otu_f4d70b49-e98c-404f-8f79-d12650a553e5
## 3     Ameiurus catus otu_6bf26aae-5fdd-4de1-bb91-fae893a4f5b7
## 4     Ameiurus melas otu_ac97d70b-2d14-40ae-9ad3-8f32e50a043a
## 5   Ameiurus natalis otu_450a8b6a-bb62-4599-8c6a-a85b39f8c653
## 6 Ameiurus nebulosus otu_e8f08463-37ec-41b5-a629-4dacf1bc067e
##   Anal-fin rays, species mean count Anterior dentations of pectoral spine
## 2                           16.1-20                                 large
## 3                         20.1-25.5                              moderate
## 4                         20.1-25.5                                 small
## 5                           25.6-28                                 small
## 6                         20.1-25.5                              moderate
##   Anterior distal serrae of pectoral spine
## 2               <3 moderately sharp serrae
## 3              3-6 moderately sharp serrae
## 4             absent or scarcely developed
## 5              3-6 moderately sharp serrae
## 6               <3 moderately sharp serrae
```

Each character matrix can be integrated with other data, such as meta data which include taxon identifiers, character identifiers, etc.
Get meta data:

```r
study_metas <- pk_get_study_meta(nex_list)
study_metas[[1]]
```

```
## $id_taxa
## Source: local data frame [22 x 4]
## 
##                     label                                       href
##                     (chr)                                      (chr)
## 1        Ameiurus natalis http://purl.obolibrary.org/obo/VTO_0036274
## 2        Ictalurus pricei http://purl.obolibrary.org/obo/VTO_0036218
## 3          Ameiurus catus http://purl.obolibrary.org/obo/VTO_0036275
## 4          Noturus flavus http://purl.obolibrary.org/obo/VTO_0036263
## 5  Prietella phreatophila http://purl.obolibrary.org/obo/VTO_0036265
## 6       Ictalurus dugesii http://purl.obolibrary.org/obo/VTO_0061497
## 7      Ictalurus furcatus http://purl.obolibrary.org/obo/VTO_0036223
## 8     Pylodictis olivaris http://purl.obolibrary.org/obo/VTO_0036282
## 9  Ameiurus platycephalus http://purl.obolibrary.org/obo/VTO_0036276
## 10  Ameiurus serracanthus http://purl.obolibrary.org/obo/VTO_0036277
## ..                    ...                                        ...
## Variables not shown: otu (chr), otus (chr)
## 
## $id_entities
## Source: local data frame [115 x 2]
## 
##                                                         label
##                                                         (chr)
## 1                               Posterior flap of adipose fin
## 2                                              Gill membranes
## 3                                                 Orbital rim
## 4                                                  Caudal fin
## 5                                         Lateral line extent
## 6                                           Mesethmoid cornua
## 7                     Width of snout across mesethmoid cornua
## 8                                    Width of mesethmoid neck
## 9  Cross-sectional form of skull (juveniles and young adults)
## 10                       Neurocranial width at epiphyseal bar
## ..                                                        ...
## Variables not shown: char (chr)
```

## Obtain Other Data
### Subsetting a Matrix
A matrix obtained from Phenoscape can be subsetted (filtered) by taxonomic subgroup or anatomical part. For example, using ```pk_is_descendant``` and ```pk_is_ancestor``` methods, a matrix can be subsetted to a taxonomic subgroup that is the descendants/ancestors of a given taxon.

```r
m # original character matrix
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

```r
(is_desc <- pk_is_descendant('Ictalurus', m$taxa))
```

```
##  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
## [12]  TRUE  TRUE  TRUE  TRUE
```

```r
m[is_desc, ] #subsetting to the descendants of Ictalurus
```

```
## Source: local data frame [8 x 5]
## 
##                  taxa         otu                                  otus
##                 (chr)       (chr)                                 (chr)
## 1 Ictalurus australis VTO_0061495 t3e0ff131-e0c0-4137-bdb2-b04a0d6739c3
## 2  Ictalurus balsanus VTO_0036221 t3e0ff131-e0c0-4137-bdb2-b04a0d6739c3
## 3   Ictalurus dugesii VTO_0061497 t3e0ff131-e0c0-4137-bdb2-b04a0d6739c3
## 4  Ictalurus furcatus VTO_0036223 t3e0ff131-e0c0-4137-bdb2-b04a0d6739c3
## 5     Ictalurus lupus VTO_0036220 t3e0ff131-e0c0-4137-bdb2-b04a0d6739c3
## 6 Ictalurus mexicanus VTO_0061498 t3e0ff131-e0c0-4137-bdb2-b04a0d6739c3
## 7    Ictalurus pricei VTO_0036218 t3e0ff131-e0c0-4137-bdb2-b04a0d6739c3
## 8 Ictalurus punctatus VTO_0036225 t3e0ff131-e0c0-4137-bdb2-b04a0d6739c3
## Variables not shown: anterior dentation of pectoral fin spine (int),
##   anterior distal serration of pectoral fin spine (dbl)
```

### Term Search

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

[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](http://ropensci.org)

