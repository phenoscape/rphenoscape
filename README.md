
# rphenoscape: R package to make phenotypic traits from the Phenoscape Knowledgebase available from within R.

[![Build Status](https://travis-ci.org/xu-hong/rphenoscape.svg?branch=master)](https://travis-ci.org/xu-hong/rphenoscape)

* Maintainer: Hong Xu
* Author: Hong Xu, Hilmar Lapp

Most of the services provided with [Phenoscape Knowledgebase web API](http://docs.phenoscapekb.apiary.io) return data in JSON format, plain text (usually tab-delimited), and NeXML. This package facilitates the interfacing to the the Phenoscape Knowledge for searching ontology terms, retrieving term info, and querying data matrices. 


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

#### Miscellaneous methods:
Resolve a given term to its IRI:

```r
pk_get_iri("Coralliozetus", "vto")
```

```
[1] "http://purl.obolibrary.org/obo/VTO_0042955"
```

```r
pk_get_iri("basihyal bone", "uberon")
```

```
[1] "http://purl.obolibrary.org/obo/UBERON_0011618"
```

Test if a taxon is extinct:

```r
pk_is_extinct("Fisherichthys")
```

```
[1] TRUE
```
Get the ancestors/descendants of a taxon from a given list:

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
Source: local data frame [15 x 5]

                     taxa         otu                                  otus anterior dentation of pectoral fin spine
                    (chr)       (chr)                                 (chr)                                    (int)
1       Ameiurus brunneus VTO_0036273 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        1
2          Ameiurus catus VTO_0036275 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        1
3          Ameiurus melas VTO_0036272 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                       NA
4        Ameiurus natalis VTO_0036274 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                       NA
5      Ameiurus nebulosus VTO_0036278 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        1
6  Ameiurus platycephalus VTO_0036276 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        1
7   Ameiurus serracanthus VTO_0036277 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        1
8     Ictalurus australis VTO_0061495 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        1
9      Ictalurus balsanus VTO_0036221 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        0
10      Ictalurus dugesii VTO_0061497 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                       NA
11     Ictalurus furcatus VTO_0036223 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        0
12        Ictalurus lupus VTO_0036220 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        1
13    Ictalurus mexicanus VTO_0061498 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                       NA
14       Ictalurus pricei VTO_0036218 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        1
15    Ictalurus punctatus VTO_0036225 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e                                        1
Variables not shown: anterior distal serration of pectoral fin spine (dbl)
```
Get meta data:

```r
pk_get_ontotrace_meta(nex)
```

```
$id_taxa
Source: local data frame [15 x 4]

                    label                                       href         otu                                  otus
                    (chr)                                      (chr)       (chr)                                 (chr)
1     Ictalurus punctatus http://purl.obolibrary.org/obo/VTO_0036225 VTO_0036225 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
2      Ictalurus balsanus http://purl.obolibrary.org/obo/VTO_0036221 VTO_0036221 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
3       Ameiurus brunneus http://purl.obolibrary.org/obo/VTO_0036273 VTO_0036273 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
4      Ictalurus furcatus http://purl.obolibrary.org/obo/VTO_0036223 VTO_0036223 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
5     Ictalurus mexicanus http://purl.obolibrary.org/obo/VTO_0061498 VTO_0061498 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
6         Ictalurus lupus http://purl.obolibrary.org/obo/VTO_0036220 VTO_0036220 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
7      Ameiurus nebulosus http://purl.obolibrary.org/obo/VTO_0036278 VTO_0036278 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
8       Ictalurus dugesii http://purl.obolibrary.org/obo/VTO_0061497 VTO_0061497 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
9          Ameiurus melas http://purl.obolibrary.org/obo/VTO_0036272 VTO_0036272 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
10  Ameiurus serracanthus http://purl.obolibrary.org/obo/VTO_0036277 VTO_0036277 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
11 Ameiurus platycephalus http://purl.obolibrary.org/obo/VTO_0036276 VTO_0036276 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
12    Ictalurus australis http://purl.obolibrary.org/obo/VTO_0061495 VTO_0061495 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
13         Ameiurus catus http://purl.obolibrary.org/obo/VTO_0036275 VTO_0036275 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
14       Ictalurus pricei http://purl.obolibrary.org/obo/VTO_0036218 VTO_0036218 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e
15       Ameiurus natalis http://purl.obolibrary.org/obo/VTO_0036274 VTO_0036274 tc6b88f4b-7c8f-45a2-9048-0fba9b59809e

$id_entities
Source: local data frame [2 x 3]

                                            label                                          href           char
                                            (chr)                                         (chr)          (chr)
1 anterior distal serration of pectoral fin spine http://purl.obolibrary.org/obo/UBERON_2002002 UBERON_2002002
2        anterior dentation of pectoral fin spine http://purl.obolibrary.org/obo/UBERON_2002001 UBERON_2002001
```

## Study Matrix
Retrieve the list of studies (results returned as ```data.frame```).

```r
(slist <- pk_get_study_list(taxon = "Ictalurus australis", entity = "fin"))
```

```
Source: local data frame [1 x 2]

                                                                                                                                 id
                                                                                                                              (chr)
1 https://scholar.google.com/scholar?q=The+Phylogeny+of+Ictalurid+Catfishes%3A+A+Synthesis+of+Recent+Work&btnG=&hl=en&as_sdt=0%2C42
Variables not shown: label (chr)
```
Get the ReXML object for each study id.

```r
(nex_list <- pk_get_study_xml(slist$id))
```

```
$`https://scholar.google.com/scholar?q=The+Phylogeny+of+Ictalurid+Catfishes%3A+A+Synthesis+of+Recent+Work&btnG=&hl=en&as_sdt=0%2C42`
A nexml object representing:
 	 0 phylogenetic tree blocks, where: 
 	 block 1 contains NULL phylogenetic trees
	 block 0 contains  phylogenetic trees 
 	 393 meta elements 
 	 1 character matrices 
 	 22 taxonomic units 
 Taxa: 	 Ictalurus dugesii, Ictalurus furcatus, Ictalurus pricei, Noturus stigmosus, Ameiurus catus, Noturus insignis ... 

 NeXML generated by RNeXML using schema version: 0.9 
 size: 6.1 Mb 
```
From the list of ReXML objects, retrieve the study matrices and corresponding meta data.

```r
study_matrix <- pk_get_study(nex_list)
study_matrix[[1]][1:5, 1:5]
```

```
                taxa                                      otu Anal-fin rays, species mean count Anterior dentations of pectoral spine
2  Ameiurus brunneus otu_e643ce7b-c660-4077-8a9c-a283519a1164                           16.1-20                                 large
3     Ameiurus catus otu_55c104bc-4f44-4a0a-8fe8-118e21d01c9d                         20.1-25.5                              moderate
4     Ameiurus melas otu_c691d86a-1ceb-4cfb-883e-129e0b6cf32a                         20.1-25.5                                 small
5   Ameiurus natalis otu_bc7a771c-5e4f-4bc2-9b51-92e50b809aa5                           25.6-28                                 small
6 Ameiurus nebulosus otu_401dc007-e393-4793-8f1c-a68cc9881b10                         20.1-25.5                              moderate
  Anterior distal serrae of pectoral spine
2               <3 moderately sharp serrae
3              3-6 moderately sharp serrae
4             absent or scarcely developed
5              3-6 moderately sharp serrae
6               <3 moderately sharp serrae
```

```r
study_metas <- pk_get_study_meta(nex_list)
study_metas[[1]]
```

```
$id_taxa
Source: local data frame [22 x 4]

                   label                                       href                                      otu                                      otus
                   (chr)                                      (chr)                                    (chr)                                     (chr)
1      Ictalurus dugesii http://purl.obolibrary.org/obo/VTO_0061497 otu_969dc5cc-1442-40fe-82fb-aedcc3585009 otus_5811ca84-ed53-464b-b029-7607a690825d
2     Ictalurus furcatus http://purl.obolibrary.org/obo/VTO_0036223 otu_de5f6dbb-8562-4fab-9f9b-0d4d05c53564 otus_5811ca84-ed53-464b-b029-7607a690825d
3       Ictalurus pricei http://purl.obolibrary.org/obo/VTO_0036218 otu_fc946a1a-b595-4ed7-a490-5972128d87ee otus_5811ca84-ed53-464b-b029-7607a690825d
4      Noturus stigmosus http://purl.obolibrary.org/obo/VTO_0036261 otu_788af652-172c-4f71-a150-68e7ae246a75 otus_5811ca84-ed53-464b-b029-7607a690825d
5         Ameiurus catus http://purl.obolibrary.org/obo/VTO_0036275 otu_55c104bc-4f44-4a0a-8fe8-118e21d01c9d otus_5811ca84-ed53-464b-b029-7607a690825d
6       Noturus insignis http://purl.obolibrary.org/obo/VTO_0036237 otu_e43ad0c3-5b13-4cda-b09e-a99175be97c0 otus_5811ca84-ed53-464b-b029-7607a690825d
7       Satan eurystomus http://purl.obolibrary.org/obo/VTO_0061485 otu_a86c1c69-aa41-44e1-9049-13a91a2c929f otus_5811ca84-ed53-464b-b029-7607a690825d
8    Ictalurus mexicanus http://purl.obolibrary.org/obo/VTO_0061498 otu_51ac34c3-821c-44cb-baad-25cb5c71223c otus_5811ca84-ed53-464b-b029-7607a690825d
9  Ameiurus serracanthus http://purl.obolibrary.org/obo/VTO_0036277 otu_0328df35-b408-4d26-aad7-496dd6ce2416 otus_5811ca84-ed53-464b-b029-7607a690825d
10        Ameiurus melas http://purl.obolibrary.org/obo/VTO_0036272 otu_c691d86a-1ceb-4cfb-883e-129e0b6cf32a otus_5811ca84-ed53-464b-b029-7607a690825d
..                   ...                                        ...                                      ...                                       ...

$id_entities
Source: local data frame [115 x 2]

                                                        label                                           char
                                                        (chr)                                          (chr)
1                               Posterior flap of adipose fin character_2a113a10-5f9b-4c00-bf22-8d62ab42ead8
2                                              Gill membranes character_f797d917-9eba-43ae-bde0-a09d3b045705
3                                                 Orbital rim character_408c8fde-2483-4eae-94db-37fecb9aa635
4                                                  Caudal fin character_6561bc79-bda4-42c2-8c33-342ad24e809a
5                                         Lateral line extent character_da240f71-ee94-4480-ad44-0497d81e4d1a
6                                           Mesethmoid cornua character_8bb89750-2e8e-4777-912d-4c6c1896bcba
7                     Width of snout across mesethmoid cornua character_24ec5c28-82a2-4346-b0c9-5643c29c7a1e
8                                    Width of mesethmoid neck character_d06f6d32-7b65-497d-8b6f-9af0f1adb2bd
9  Cross-sectional form of skull (juveniles and young adults) character_2c735b5e-93cc-4f7b-bb32-1be4ce220216
10                       Neurocranial width at epiphyseal bar character_fa842fff-7817-4904-b423-016c6d8830c4
..                                                        ...                                            ...
```

[![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)

