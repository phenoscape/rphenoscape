<!-- README.md is generated from README.rmd. Please edit that file -->
rphenoscape: Semantically rich phenotypic traits from the Phenoscape Knowledgebase
==================================================================================

[![Build
Status](https://travis-ci.org/phenoscape/rphenoscape.svg?branch=master)](https://travis-ci.org/phenoscape/rphenoscape)

- Author: Hong Xu, Hilmar Lapp
- Maintainer: Hilmar Lapp

Most of the services provided with [Phenoscape Knowledgebase web
API](http://kb.phenoscape.org/apidocs/) return data in JSON format,
plain text (usually tab-delimited), and NeXML. This package facilitates
the interfacing to the Phenoscape Knowledge for searching ontology
terms, retrieving term info, and querying data matrices.

Getting Started
---------------

The development version of rphenoscape is available on
[Github](www.github.com/phenoscape/rphenoscape). With the `devtools`
package installed on your system, rphenoscape can be installed using:

``` r
library(devtools)
install_github("phenoscape/rphenoscape")
library(rphenoscape)
```

Character Matrix via Ontotrace
------------------------------

Use Ontotrace to obtain a character matrix for a taxonomic clade and
anatomical region of interest.

The [Phenoscape Knowledgebase web
API](http://kb.phenoscape.org/apidocs/#/OntoTrace/get_ontotrace) returns
the evolutionary character matrix in NeXML format. The first step is to
get the NeXML object using `pk_get_ontotrace_xml` method.

``` r
nex <- pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus"), entity = "fin spine")
```

Then retrieve wanted information from the NeXML object.  
Get character matrix:

``` r
(m <- pk_get_ontotrace(nex))
```

    ## # A tibble: 15 x 5
    ##    taxa     otu     otus        `anterior dentation… `anterior distal ser…
    ##    <chr>    <chr>   <chr>                      <int> <chr>                
    ##  1 Ameiuru… VTO_00… t1a37c269-…                    1 1                    
    ##  2 Ameiuru… VTO_00… t1a37c269-…                    1 1                    
    ##  3 Ameiuru… VTO_00… t1a37c269-…                   NA 0 and 1              
    ##  4 Ameiuru… VTO_00… t1a37c269-…                   NA 1                    
    ##  5 Ameiuru… VTO_00… t1a37c269-…                    1 1                    
    ##  6 Ameiuru… VTO_00… t1a37c269-…                    1 1                    
    ##  7 Ameiuru… VTO_00… t1a37c269-…                    1 1                    
    ##  8 Ictalur… VTO_00… t1a37c269-…                    1 1                    
    ##  9 Ictalur… VTO_00… t1a37c269-…                    0 0 and 1              
    ## 10 Ictalur… VTO_00… t1a37c269-…                   NA 1                    
    ## 11 Ictalur… VTO_00… t1a37c269-…                    0 1                    
    ## 12 Ictalur… VTO_00… t1a37c269-…                    1 1                    
    ## 13 Ictalur… VTO_00… t1a37c269-…                   NA 1                    
    ## 14 Ictalur… VTO_00… t1a37c269-…                    1 1                    
    ## 15 Ictalur… VTO_00… t1a37c269-…                    1 1

The character matrix can be integrated with other data, such as meta
data which include taxon identifiers, character identifiers, etc. Get
meta data:

``` r
(meta <- pk_get_ontotrace_meta(nex))
```

    ## $id_taxa
    ##                     label                                       href
    ## 1          Ameiurus melas http://purl.obolibrary.org/obo/VTO_0036272
    ## 2         Ictalurus lupus http://purl.obolibrary.org/obo/VTO_0036220
    ## 3  Ameiurus platycephalus http://purl.obolibrary.org/obo/VTO_0036276
    ## 4      Ictalurus balsanus http://purl.obolibrary.org/obo/VTO_0036221
    ## 5     Ictalurus punctatus http://purl.obolibrary.org/obo/VTO_0036225
    ## 6   Ameiurus serracanthus http://purl.obolibrary.org/obo/VTO_0036277
    ## 7       Ameiurus brunneus http://purl.obolibrary.org/obo/VTO_0036273
    ## 8     Ictalurus australis http://purl.obolibrary.org/obo/VTO_0061495
    ## 9        Ictalurus pricei http://purl.obolibrary.org/obo/VTO_0036218
    ## 10     Ameiurus nebulosus http://purl.obolibrary.org/obo/VTO_0036278
    ## 11       Ameiurus natalis http://purl.obolibrary.org/obo/VTO_0036274
    ## 12    Ictalurus mexicanus http://purl.obolibrary.org/obo/VTO_0061498
    ## 13     Ictalurus furcatus http://purl.obolibrary.org/obo/VTO_0036223
    ## 14         Ameiurus catus http://purl.obolibrary.org/obo/VTO_0036275
    ## 15      Ictalurus dugesii http://purl.obolibrary.org/obo/VTO_0061497
    ##            otu                                  otus
    ## 1  VTO_0036272 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 2  VTO_0036220 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 3  VTO_0036276 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 4  VTO_0036221 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 5  VTO_0036225 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 6  VTO_0036277 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 7  VTO_0036273 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 8  VTO_0061495 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 9  VTO_0036218 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 10 VTO_0036278 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 11 VTO_0036274 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 12 VTO_0061498 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 13 VTO_0036223 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 14 VTO_0036275 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 15 VTO_0061497 t1a37c269-a06a-4dc8-94c3-689227f41d36
    ## 
    ## $id_entities
    ##                                             label
    ## 1 anterior distal serration of pectoral fin spine
    ## 2        anterior dentation of pectoral fin spine
    ##                                            href           char
    ## 1 http://purl.obolibrary.org/obo/UBERON_2002002 UBERON_2002002
    ## 2 http://purl.obolibrary.org/obo/UBERON_2002001 UBERON_2002001

Character Matrices for Studies
------------------------------

To obtain the character matrices for studies published for a taxonomic
clade and anatomical region of interest.

First step is to retrieve the list of studies given a taxonomic clade
and anatomical structures (returned as `data.frame`).

``` r
(slist <- pk_get_study_list(taxon = "Ictalurus australis", entity = "fin"))
```

    ## # A tibble: 1 x 2
    ##   id                                                          label       
    ## * <chr>                                                       <chr>       
    ## 1 https://scholar.google.com/scholar?q=The+Phylogeny+of+Icta… Lundberg (1…

Based off the study ids retained from previous step, get the
evolutionary character matrix for each study id (in NeXML-format) using
`pk_get_study_xml`.

``` r
(nex_list <- pk_get_study_xml(slist$id))
```

    ## ....This might take a while....

    ## https://scholar.google.com/scholar?q=The+Phylogeny+of+Ictalurid+Catfishes%3A+A+Synthesis+of+Recent+Work&btnG=&hl=en&as_sdt=0%2C42

    ## Parse NeXML....

    ## $`https://scholar.google.com/scholar?q=The+Phylogeny+of+Ictalurid+Catfishes%3A+A+Synthesis+of+Recent+Work&btnG=&hl=en&as_sdt=0%2C42`
    ## A nexml object representing:
    ##       0 phylogenetic tree blocks, where: 
    ##       block 1 contains NULL phylogenetic trees
    ##   block 0 contains  phylogenetic trees 
    ##       393 meta elements 
    ##       1 character matrices 
    ##       22 taxonomic units 
    ##  Taxa:    Ameiurus natalis, Ictalurus mexicanus, Ameiurus serracanthus, Satan eurystomus, Ictalurus pricei, Ictalurus balsanus ... 
    ## 
    ##  NeXML generated by RNeXML using schema version: 0.9 
    ##  size: 6.6 Mb

From the list of ReXML objects, retrieve the character matrices.

``` r
study_matrix <- pk_get_study(nex_list)
```

    ## Map symbols to labels...

``` r
study_matrix[[1]][1:5, 1:5]
```

    ##                 taxa                                      otu
    ## 1  Ameiurus brunneus otu_92fd3ee4-d256-4762-8e87-a411e833b793
    ## 2     Ameiurus catus otu_e9354c41-617b-4bb8-99e7-d0ba1d27c999
    ## 3     Ameiurus melas otu_8e5fdacc-b0da-4776-913d-32c76d182896
    ## 4   Ameiurus natalis otu_ba1a0973-7bdb-46cc-b086-20593fa8164b
    ## 5 Ameiurus nebulosus otu_30e07998-0c02-4df6-9220-fa9fdac10282
    ##   Anal-fin rays, species mean count Anterior dentations of pectoral spine
    ## 1                           16.1-20                                 large
    ## 2                         20.1-25.5                              moderate
    ## 3                         20.1-25.5                                 small
    ## 4                           25.6-28                                 small
    ## 5                         20.1-25.5                              moderate
    ##   Anterior distal serrae of pectoral spine
    ## 1               <3 moderately sharp serrae
    ## 2              3-6 moderately sharp serrae
    ## 3             absent or scarcely developed
    ## 4              3-6 moderately sharp serrae
    ## 5               <3 moderately sharp serrae

Each character matrix can be integrated with other data, such as meta
data which include taxon identifiers, character identifiers, etc. Get
meta data:

``` r
study_metas <- pk_get_study_meta(nex_list)
study_metas[[1]]
```

    ## $id_taxa
    ##                      label                                       href
    ## 1         Ameiurus natalis http://purl.obolibrary.org/obo/VTO_0036274
    ## 2      Ictalurus mexicanus http://purl.obolibrary.org/obo/VTO_0061498
    ## 3    Ameiurus serracanthus http://purl.obolibrary.org/obo/VTO_0036277
    ## 4         Satan eurystomus http://purl.obolibrary.org/obo/VTO_0061485
    ## 5         Ictalurus pricei http://purl.obolibrary.org/obo/VTO_0036218
    ## 6       Ictalurus balsanus http://purl.obolibrary.org/obo/VTO_0036221
    ## 7      Ictalurus australis http://purl.obolibrary.org/obo/VTO_0061495
    ## 8      Ictalurus punctatus http://purl.obolibrary.org/obo/VTO_0036225
    ## 9      Pylodictis olivaris http://purl.obolibrary.org/obo/VTO_0036282
    ## 10         Ictalurus lupus http://purl.obolibrary.org/obo/VTO_0036220
    ## 11 Trogloglanis pattersoni http://purl.obolibrary.org/obo/VTO_0061501
    ## 12       Ameiurus brunneus http://purl.obolibrary.org/obo/VTO_0036273
    ## 13          Ameiurus catus http://purl.obolibrary.org/obo/VTO_0036275
    ## 14        Noturus insignis http://purl.obolibrary.org/obo/VTO_0036237
    ## 15  Prietella phreatophila http://purl.obolibrary.org/obo/VTO_0036265
    ## 16      Ictalurus furcatus http://purl.obolibrary.org/obo/VTO_0036223
    ## 17       Ictalurus dugesii http://purl.obolibrary.org/obo/VTO_0061497
    ## 18          Ameiurus melas http://purl.obolibrary.org/obo/VTO_0036272
    ## 19          Noturus flavus http://purl.obolibrary.org/obo/VTO_0036263
    ## 20       Noturus stigmosus http://purl.obolibrary.org/obo/VTO_0036261
    ## 21      Ameiurus nebulosus http://purl.obolibrary.org/obo/VTO_0036278
    ## 22  Ameiurus platycephalus http://purl.obolibrary.org/obo/VTO_0036276
    ##                                         otu
    ## 1  otu_ba1a0973-7bdb-46cc-b086-20593fa8164b
    ## 2  otu_e43bf61d-2bda-4692-9c6d-204e4eb1fa66
    ## 3  otu_244bdfbb-dbc5-4b6d-bc24-70f5ca538d90
    ## 4  otu_c542071d-0197-4b53-bb34-e0b1eed58518
    ## 5  otu_f032ca44-950b-4845-a3f8-37cfdd93d2d7
    ## 6  otu_71acac17-e1f7-4148-bad8-5cc31a7fa968
    ## 7  otu_2e5979e7-0f73-4fb0-95db-a2eded427c29
    ## 8  otu_8eed2182-5775-459b-b78e-6d9fee1a0b52
    ## 9  otu_d4bbe873-b6c8-442a-b25c-644c94ae722e
    ## 10 otu_155534f5-afee-41d6-9674-45a1a7fde5ea
    ## 11 otu_6ca6ac23-045d-43b3-9289-bcd99578de07
    ## 12 otu_92fd3ee4-d256-4762-8e87-a411e833b793
    ## 13 otu_e9354c41-617b-4bb8-99e7-d0ba1d27c999
    ## 14 otu_e91d31c1-3432-49cc-8d4e-04192db57a71
    ## 15 otu_c8601ec2-6d7a-40d3-bde5-654aa65857ef
    ## 16 otu_bdb25a18-6e61-4fda-afe1-e2043b96d17b
    ## 17 otu_a64a3429-fc1a-49cb-a736-4103708ae2d3
    ## 18 otu_8e5fdacc-b0da-4776-913d-32c76d182896
    ## 19 otu_562f4858-ebca-4250-b1b0-caf3cbcef0d0
    ## 20 otu_e23df5f7-f327-4376-b8b3-550d893d49a3
    ## 21 otu_30e07998-0c02-4df6-9220-fa9fdac10282
    ## 22 otu_5eef2721-367b-4c6b-a997-be6dad70bf67
    ##                                         otus
    ## 1  otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 2  otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 3  otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 4  otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 5  otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 6  otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 7  otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 8  otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 9  otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 10 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 11 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 12 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 13 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 14 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 15 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 16 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 17 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 18 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 19 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 20 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 21 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 22 otus_cbf4afd7-f92e-413a-8fcb-2e28ee3aa4ba
    ## 
    ## $id_entities
    ##                                                               label
    ## 1                                     Posterior flap of adipose fin
    ## 2                                                    Gill membranes
    ## 3                                                       Orbital rim
    ## 4                                                        Caudal fin
    ## 5                                               Lateral line extent
    ## 6                                                 Mesethmoid cornua
    ## 7                           Width of snout across mesethmoid cornua
    ## 8                                          Width of mesethmoid neck
    ## 9        Cross-sectional form of skull (juveniles and young adults)
    ## 10                             Neurocranial width at epiphyseal bar
    ## 11                            Posterior cranial fontanelle (adults)
    ## 12                Supraoccipital process and associated supraneural
    ## 13                                    Sagittal crests on skull roof
    ## 14                                      Transverse occipital crests
    ## 15                               Muscle crests on margin of frontal
    ## 16                     Sculpturing pattern of supraoccipital crests
    ## 17                                     Anterior extent of sphenotic
    ## 18                                                Sphenotic process
    ## 19                                   Extrascapular bone composition
    ## 20                                              Pterotic wing shape
    ## 21                                                 Pterotic surface
    ## 22                                                      Nasal shape
    ## 23                                               Infraorbital bones
    ## 24                                             Lateral ethmoid wing
    ## 25                             Superficial ophthalmic nerve foramen
    ## 26                                                  Orbital foramen
    ## 27                                                Parasphenoid stem
    ## 28                                  Ascending wings of parasphenoid
    ## 29    Posterior end of adductor arcus palatini scar on parasphenoid
    ## 30                                         Vertical wing of frontal
    ## 31                                      Pterosphenoid-prootic joint
    ## 32                                           Orbitosphenoid shelves
    ## 33                                        Prootic-exoccipital joint
    ## 34                                      Prootic-basioccipital joint
    ## 35                                                    Optic foramen
    ## 36                                                  Mandible length
    ## 37                    Mandibular sensory canal pores, species modes
    ## 38                           First mandibular sensory pore and tube
    ## 39                            Mandibular symphyseal ventral process
    ## 40                                   Anteroventral crest of dentary
    ## 41                                                 Coronoid process
    ## 42                                   Posterior process of articular
    ## 43                                                 Premaxilla width
    ## 44                                     Mesial process of premaxilla
    ## 45                                    Lateral process of premaxilla
    ## 46                               Premaxillary teeth-size and number
    ## 47                               Jaw teeth-shape and size gradation
    ## 48                                Jaw adductor musculature-A3 layer
    ## 49                                                  Palatine length
    ## 50                          Hyomandibular (otic region joint) shape
    ## 51                                Anterior process of hyomandibular
    ## 52      Opercular facet position on posterior edge of hyomandibular
    ## 53                                Hyomandibular-metapterygoid joint
    ## 54                                     Hyomandibular-quadrate joint
    ## 55  Scar on hyomandibular for A3 bundle of jaw adductor musculature
    ## 56         Crest on hyomandibular for levator arcus palatini muscle
    ## 57      Crest on hyomandibular for A2 bundle of jaw adductor muscle
    ## 58               Crest on hyomandibular for levator operculi muscle
    ## 59     Process on hyomandibular for adductor hyomandibularis muscle
    ## 60     External foramen of facial nerve canal through hyomandibular
    ## 61                                                 Symplectic canal
    ## 62             Jaw adductor muscle attachment surface on preopercle
    ## 63                                         Upper limb of preopercle
    ## 64                                                  Suprapreopercle
    ## 65                              Quadrate-metapterygoid joint length
    ## 66                         Quadrate-metapterygoid joint composition
    ## 67                                               Metapterygoid form
    ## 68                                               Endopterygoid size
    ## 69                                         Epihyal-ceratohyal joint
    ## 70                                        Dorsal edge of ceratohyal
    ## 71                                                    Epihyal shape
    ## 72                                Ceratohyal-ventral hypohyal joint
    ## 73                                                  Dorsal hypohyal
    ## 74                                                    Urohyal shape
    ## 75                                                    Opercle shape
    ## 76                      Scar on opercle for levator operculi muscle
    ## 77                                               Interopercle shape
    ## 78                                     Dorsal pits on first centrum
    ## 79                                  Ventral ridges on first centrum
    ## 80                          Development of superficial ossification
    ## 81                       Anterior limb of fourth transverse process
    ## 82           Vertical lamina between third and fourth neural spines
    ## 83                                   Neural complex = supraneural 3
    ## 84                                        Depth of Weberian complex
    ## 85                                                      Gas bladder
    ## 86                         Precaudal vertebrae, species mean number
    ## 87                            Caudal vertebrae, species mean number
    ## 88                                                         Hypurals
    ## 89                                                   Hypurapophysis
    ## 90                                  Caudal-fin branched ray numbers
    ## 91            Middle Nuchal plate of first dorsal-fin pterygiophore
    ## 92           Spine base condyles on second dorsal-fin pterygiophore
    ## 93                                     Dorsal-fin spine development
    ## 94                                Anal-fin rays, species mean count
    ## 95                      Pectoral-fin soft rays, species modal count
    ## 96                                             Pectoral-fin radials
    ## 97             Ornamentation pattern on posterior cleithral process
    ## 98                            Length of posterior cleithral process
    ## 99                                    Length of cleithral symphysis
    ## 100                    Coracoid midline sutures, species mean count
    ## 101                                        Ventral keel of coracoid
    ## 102               Secondary keel on coracoid below scapular foramen
    ## 103                               Extensor muscle fossa on coracoid
    ## 104                                           Transcapular ligament
    ## 105                           Subpterotic process of supracleithrum
    ## 106                                              Pectoral spine tip
    ## 107                        Anterior distal serrae of pectoral spine
    ## 108                           Anterior dentations of pectoral spine
    ## 109                                Anterior ridge of pectoral spine
    ## 110                                       Posterior dentation shape
    ## 111                                       Posterior dentation bases
    ## 112                                    Pectoral spine late ontogeny
    ## 113                         Posterolateral process of basipterygium
    ## 114                                                   Pelvic splint
    ## 115                           Pelvic-fin rays, species modal counts
    ##                                               char
    ## 1   character_244358e8-1b50-4f92-9c07-63d9a39065df
    ## 2   character_f4218791-b604-4abf-be17-24961ba95993
    ## 3   character_6b585607-b369-4aa1-b16c-089b004cab7b
    ## 4   character_c80cc74c-5bb3-49e6-8b3b-06c0e6aaf919
    ## 5   character_7a6c749f-bb26-4696-80b1-5bdfa158dee7
    ## 6   character_fbf0a72c-c7c9-4866-8f4d-554de3dbd319
    ## 7   character_b2500152-65dc-422f-b20a-ce8c87ffb085
    ## 8   character_039b73a8-6aa2-4245-9bd4-c7a5a8e6d28e
    ## 9   character_7a35ea4a-a3b0-4cca-96ec-e5f39a28c0a1
    ## 10  character_932d5284-49f8-4a99-aaa5-1ef403d7386e
    ## 11  character_47b28a43-624c-4d69-89e6-c8125a1a49c6
    ## 12  character_074c8f5d-8426-4cc3-9dce-feaef46e79c6
    ## 13  character_5c2ed580-30da-4e62-99f2-fb8fb4225b84
    ## 14  character_903bcb1b-931f-44e5-9277-4278949d79bb
    ## 15  character_0bdb912b-a8ff-407b-b6d7-8ad3c70a9101
    ## 16  character_5d652089-95c4-43cb-9595-7964f81316c5
    ## 17  character_4663adce-84c8-410b-a7b4-c0c8f1c3b964
    ## 18  character_82360082-e80d-4051-be0f-075c2cb4dae9
    ## 19  character_9474663f-ed7c-47d7-b9c8-2229118cc667
    ## 20  character_a8b72530-a06f-40f7-a0fd-37bf05babdce
    ## 21  character_79049dbc-fd23-4b76-b16e-5ab1b778f00d
    ## 22  character_dfa479f1-368e-4351-ab36-58d9a32e599a
    ## 23  character_99080a92-2718-4c17-8247-fbbc39e2c4ba
    ## 24  character_c98b9ee6-0f50-4669-a43f-1acf15206f27
    ## 25  character_189eb118-c25a-4956-bdd6-5657f1bc02ec
    ## 26  character_361797d2-ed8b-490c-b132-c27716314a43
    ## 27  character_46ad2530-8693-4e76-b21b-bfd875bb63b0
    ## 28  character_a904bae2-24a9-479d-a686-1369040a8324
    ## 29  character_a1b9135d-9ede-4abf-8e29-7abef5f78976
    ## 30  character_4eeb7adf-20c2-4de7-9e0d-16cd53c688ac
    ## 31  character_1d822ed9-723f-453d-8f37-69a2f41a4918
    ## 32  character_db2d8980-b8a1-4a3c-af7d-caad69ea4734
    ## 33  character_6734ab73-b7b0-4fd6-be62-7f5dd314d71b
    ## 34  character_563efbbe-0f0d-4130-b070-bb8b4e9e4034
    ## 35  character_c2058839-38ac-4cd9-8879-d4d52ea5776f
    ## 36  character_6e93d4a4-cc1b-4114-babf-8ee1e16e4c95
    ## 37  character_35929aca-b9cd-47dd-ac22-92bbddac7aa3
    ## 38  character_e808095d-f661-4071-a6b3-b117c52d4ba9
    ## 39  character_f8221f28-8219-4282-94e4-6735690c1448
    ## 40  character_e119ac10-d4f2-425b-8d5e-64d996cca6a7
    ## 41  character_d2b3c69e-732e-4dbf-b6ce-bbd9285a7c43
    ## 42  character_37df6ef1-21ce-4c66-a3ac-e8b41bf4f6ca
    ## 43  character_19ba6e31-ab45-461c-a973-1e74450baddf
    ## 44  character_2c2b503d-7bbf-418c-85ec-fea3612ebee2
    ## 45  character_7d11e6b1-d30d-4f45-9e35-5ce2b297b110
    ## 46  character_7d2e4f49-39ea-4dda-b43c-4ba43f16b315
    ## 47  character_57d12272-7040-42ec-9769-7c3dd2f24b3a
    ## 48  character_8e07f8b1-639f-4ba7-8fc8-6dbd50f0677d
    ## 49  character_46f634c4-e83c-48a5-a9f5-9d196ea797f8
    ## 50  character_5432e389-584e-4bb2-aeb2-4b35f4009924
    ## 51  character_c4e85984-0a2b-4a62-b358-953a4a73c842
    ## 52  character_563dbffc-3aa6-4bfd-8ef4-91ad2745c4b4
    ## 53  character_33065b4f-adac-4ef5-87f4-7e8344a7176d
    ## 54  character_2da55b0a-97e6-4e74-bfa5-dd49bf09594a
    ## 55  character_d642283b-d002-4406-91b4-00701dfba4d8
    ## 56  character_dbadf96e-8f50-471e-9da8-fea211e28189
    ## 57  character_3b553a49-9375-4983-8ff6-fbf78e28369b
    ## 58  character_6dd6703d-893c-458e-95e6-c05e6c828b90
    ## 59  character_32e523e1-375b-4908-8c59-4bd20f6a72e1
    ## 60  character_1c0e4fec-71cf-439c-9bc2-7952a1c2e317
    ## 61  character_16668bc7-6dd0-4717-a118-1f402110c1eb
    ## 62  character_7a0b07b7-e366-4824-9fd0-65f0aa8d93e8
    ## 63  character_dc4ec2a6-b35e-494f-81ee-2e72b4c3f79d
    ## 64  character_4f0f34fa-5a7c-4256-994a-8c7e03008dbe
    ## 65  character_f50e8efa-1922-462b-8a5a-a05c5587bb9e
    ## 66  character_b5ce3b0f-e37e-408a-973d-666e49fa42e2
    ## 67  character_7933468a-24ec-4dd5-a405-6f69c0db12f9
    ## 68  character_00b57d05-42ac-4f5a-aeec-4095aeaa09a3
    ## 69  character_e9005f68-c3f4-4e6c-ba58-5355fe3705d3
    ## 70  character_a01b4989-1502-40e4-908c-7b6c3d3d680d
    ## 71  character_0bc04687-6541-48f6-b092-ec90e25adc18
    ## 72  character_7edccf2d-eecc-40ac-bc95-e690964d0962
    ## 73  character_5595bacf-3c25-4385-be7c-3711d9f4dc4c
    ## 74  character_abd39c88-9650-48e0-9813-ba7a0feec653
    ## 75  character_f18cc724-fe0d-4a0d-afc8-b1e5bcb6dfbd
    ## 76  character_216ffee9-0caa-4ab0-81f4-73dddb0bdfab
    ## 77  character_4ccb84aa-0814-46b8-af8c-9efbff759f62
    ## 78  character_984e7cfd-15c9-4bab-bd11-85e0ff898b1c
    ## 79  character_14f23090-b922-42fd-90c2-6050aa736c3a
    ## 80  character_bf71aa51-96e1-4871-8588-326d2791a799
    ## 81  character_b0656d24-1d60-4bec-989a-2ba54d6bbb99
    ## 82  character_fee0a4ac-ea0f-4618-9b89-533c069dce8a
    ## 83  character_f8c9479d-cd0b-4ca8-ad10-160e5f66a5b6
    ## 84  character_1e182718-50f0-41d8-b3d2-d2ae5df9b685
    ## 85  character_b2a8ff9b-d007-43bf-9b4f-81aa05c6bc7f
    ## 86  character_9d765303-86ca-4fbb-bf03-2a1e948bb1ee
    ## 87  character_7d23e76f-d96b-45cc-bd43-c29063c15dd2
    ## 88  character_f0a65d14-3a75-44e2-b2cf-03a0a8042caa
    ## 89  character_740cb3aa-910c-4b5f-b7ad-792126e8ed4c
    ## 90  character_d38753bb-a586-455d-ae68-01f1c2111020
    ## 91  character_d3cceded-7633-4319-82f8-0744a659a020
    ## 92  character_6ef91243-3bc1-4048-bf7b-9b3c7b69844d
    ## 93  character_4f807535-5c06-4362-9277-5bf1532b3a66
    ## 94  character_23d6c870-d65e-4fee-9b4f-c5ba8176b82f
    ## 95  character_9c0cfa91-1be9-433a-8355-bca3fa141224
    ## 96  character_843100b9-a66e-4682-871b-84cf8384be1f
    ## 97  character_8e7ebd7d-f996-42e4-a645-eeff65e4f73d
    ## 98  character_4e90845f-9ca6-4306-934f-6ac62f17a109
    ## 99  character_3069ef6d-eebf-474f-97dc-6bbd3bcc3090
    ## 100 character_6c45bb16-844c-4857-9525-bfe2d115d8e7
    ## 101 character_696f7a14-e4a8-461e-b6f1-5389148bacf5
    ## 102 character_7156fe5a-a26d-4c77-85b1-bad91dadfffa
    ## 103 character_b1dea252-1757-48cf-a283-df99d57b441e
    ## 104 character_93e09790-5ed7-4285-9a97-4c347299ad5d
    ## 105 character_d52e8e56-18c0-4ddd-9cfc-f93fed82e9d5
    ## 106 character_538e8eab-f37e-47d6-a695-57a434f475ed
    ## 107 character_5c147c2b-0a0d-4585-8e4f-9860de57894a
    ## 108 character_0e3502b4-58a8-474e-8706-9dd2f225726a
    ## 109 character_4cb17e40-a2ff-4aee-947c-ea4492532125
    ## 110 character_745114b5-4819-4c71-aaa8-8e8abcd5ce36
    ## 111 character_16622ee8-d9dd-4985-a251-f603c624590d
    ## 112 character_26bbc5ff-4c1a-4b2f-8f06-480cbef97327
    ## 113 character_5b4dbb19-d143-4345-9e02-70056f6e94f1
    ## 114 character_dae6693c-4513-42bc-928d-5a2004ffcd0e
    ## 115 character_f7aa029a-47e7-4b09-ae66-e539440774da

Obtain Other Data
-----------------

### Subsetting a Matrix

A matrix obtained from Phenoscape can be subsetted (filtered) by
taxonomic subgroup or anatomical part. For example, using
`pk_is_descendant` and `pk_is_ancestor` methods, a matrix can be
subsetted to a taxonomic subgroup that is the descendants/ancestors of a
given taxon.

``` r
m # original character matrix
```

    ## # A tibble: 15 x 5
    ##    taxa     otu     otus        `anterior dentation… `anterior distal ser…
    ##    <chr>    <chr>   <chr>                      <int> <chr>                
    ##  1 Ameiuru… VTO_00… t1a37c269-…                    1 1                    
    ##  2 Ameiuru… VTO_00… t1a37c269-…                    1 1                    
    ##  3 Ameiuru… VTO_00… t1a37c269-…                   NA 0 and 1              
    ##  4 Ameiuru… VTO_00… t1a37c269-…                   NA 1                    
    ##  5 Ameiuru… VTO_00… t1a37c269-…                    1 1                    
    ##  6 Ameiuru… VTO_00… t1a37c269-…                    1 1                    
    ##  7 Ameiuru… VTO_00… t1a37c269-…                    1 1                    
    ##  8 Ictalur… VTO_00… t1a37c269-…                    1 1                    
    ##  9 Ictalur… VTO_00… t1a37c269-…                    0 0 and 1              
    ## 10 Ictalur… VTO_00… t1a37c269-…                   NA 1                    
    ## 11 Ictalur… VTO_00… t1a37c269-…                    0 1                    
    ## 12 Ictalur… VTO_00… t1a37c269-…                    1 1                    
    ## 13 Ictalur… VTO_00… t1a37c269-…                   NA 1                    
    ## 14 Ictalur… VTO_00… t1a37c269-…                    1 1                    
    ## 15 Ictalur… VTO_00… t1a37c269-…                    1 1

``` r
(is_desc <- pk_is_descendant('Ictalurus', m$taxa))
```

    ##  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## [12]  TRUE  TRUE  TRUE  TRUE

``` r
m[is_desc, ] #subsetting to the descendants of Ictalurus
```

    ## # A tibble: 8 x 5
    ##   taxa     otu     otus        `anterior dentation… `anterior distal serr…
    ##   <chr>    <chr>   <chr>                      <int> <chr>                 
    ## 1 Ictalur… VTO_00… t1a37c269-…                    1 1                     
    ## 2 Ictalur… VTO_00… t1a37c269-…                    0 0 and 1               
    ## 3 Ictalur… VTO_00… t1a37c269-…                   NA 1                     
    ## 4 Ictalur… VTO_00… t1a37c269-…                    0 1                     
    ## 5 Ictalur… VTO_00… t1a37c269-…                    1 1                     
    ## 6 Ictalur… VTO_00… t1a37c269-…                   NA 1                     
    ## 7 Ictalur… VTO_00… t1a37c269-…                    1 1                     
    ## 8 Ictalur… VTO_00… t1a37c269-…                    1 1

### Term Search

Search for details for a given taxon:

``` r
pk_taxon_detail("Coralliozetus")
```

    ## # A tibble: 1 x 5
    ##   `@id`                label    extinct `rank.@id`              rank.label
    ##   <chr>                <chr>    <lgl>   <chr>                   <chr>     
    ## 1 http://purl.obolibr… Coralli… FALSE   http://purl.obolibrary… genus

Search for details for a given anatomical structure:

``` r
pk_anatomical_detail("basihyal bone")
```

    ## # A tibble: 1 x 3
    ##   label     definition                             `@id`                  
    ##   <chr>     <chr>                                  <chr>                  
    ## 1 basihyal… Replacement bone that is median and i… http://purl.obolibrary…

#### Miscellaneous methods:

Resolve a given term to its IRI:

``` r
pk_get_iri("Coralliozetus", "vto")
```

    ## [1] "http://purl.obolibrary.org/obo/VTO_0042955"

``` r
pk_get_iri("basihyal bone", "uberon")
```

    ## [1] "http://purl.obolibrary.org/obo/UBERON_0011618"

Test if a taxon is extinct:

``` r
pk_is_extinct("Fisherichthys")
```

    ## [1] TRUE

[![ropensci\_footer](https://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
