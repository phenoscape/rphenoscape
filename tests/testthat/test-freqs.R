context("term frequencies")

test_that("extracting OBO prefix", {
  tt <- c("http://purl.obolibrary.org/obo/UBERON_0011618",
          "http://purl.obolibrary.org/obo/PATO_0002279",
          "http://purl.obolibrary.org/obo/VTO_0071642",
          "http://purl.obolibrary.org/obo/MP_0030825",
          "http://purl.obolibrary.org/obo/NCBITaxon_7955")
  onts <- obo_prefix(tt)
  testthat::expect_length(onts, length(tt))
  testthat::expect_false(any(is.na(onts)))
  testthat::expect_equivalent(onts, c("UBERON", "PATO", "VTO", "MP", "NCBITaxon"))

  tt1 <- c(tt, "foobar", "http://purl.org/phenoscape/expression?value=foobar")
  onts <- obo_prefix(tt1)
  testthat::expect_length(onts, length(tt1))
  testthat::expect_true(any(is.na(onts)))
  testthat::expect_false(all(is.na(onts)))
  testthat::expect_equivalent(onts[1:4], c("UBERON", "PATO", "VTO", "MP"))

  tt1 <- append(as.list(tt),
                list(as.phenotype("http://purl.obolibrary.org/obo/MP_0030825")))
  testthat::expect_length(tt1, length(tt) + 1)
  onts <- obo_prefix(tt1)
  testthat::expect_length(onts, length(tt1))
  testthat::expect_false(any(is.na(onts)))
  testthat::expect_equivalent(onts, c("UBERON", "PATO", "VTO", "MP", "NCBITaxon", "MP"))
})

test_that("determining term categories", {
  tt <- c("http://purl.obolibrary.org/obo/UBERON_0011618",
          "http://purl.obolibrary.org/obo/PATO_0002279",
          "http://purl.obolibrary.org/obo/VTO_0071642",
          "http://purl.obolibrary.org/obo/MP_0030825",
          "http://purl.obolibrary.org/obo/NCBITaxon_7955")
  cats <- c("entity", "quality", "taxon", "phenotype", "taxon")

  tcat <- term_category(tt)
  testthat::expect_length(tcat, length(tt))
  testthat::expect_false(any(is.na(tcat)))
  testthat::expect_equivalent(tcat, cats)

  tcat <- term_category(as.phenotype("http://purl.obolibrary.org/obo/MP_0030825"))
  testthat::expect_length(tcat, 1)
  testthat::expect_equivalent(tcat, c("phenotype"))

  tt1 <- append(as.list(tt),
                list(as.phenotype("http://purl.obolibrary.org/obo/MP_0030825")))
  tcat <- term_category(tt1)
  testthat::expect_length(tcat, length(tt) + 1)
  testthat::expect_false(any(is.na(tcat)))
  testthat::expect_equivalent(tcat, c(cats, "phenotype"))

  # category by upper ontology ancestor term 
  tcat <- term_category("http://purl.obolibrary.org/obo/CL_0000066")
  testthat::expect_false(is.na(tcat))
  testthat::expect_equal(tcat, "entity")
})

test_that("success rate for entity subsumer terms", {
  tt <- sapply(c("fin ray", "dorsal fin", "caudal fin"), get_term_iri, as = "anatomy")
  subs.mat <- subsumer_matrix(tt)
  tt.types <- term_category(rownames(subs.mat))
  # less than 10% of the terms should be indeterminate
  testthat::expect_lt(mean(is.na(tt.types)), .1)
  # remove NA types
  tt.types <- tt.types[!is.na(tt.types)]
  type.fracs <- table(tt.types)/length(tt.types)
  testthat::expect_lte(length(names(type.fracs)), 3)
  testthat::expect_gt(max(type.fracs), 0.9)
  testthat::expect_equal(names(type.fracs)[type.fracs == max(type.fracs)], "entity")
  testthat::expect_true(min(type.fracs) == max(type.fracs) || min(type.fracs) < 0.1)
})

test_that("obtaining corpus size", {
  s <- corpus_size("taxa")
  testthat::expect_gt(s, 100)
  testthat::expect_lt(s, 10000)

  s <- corpus_size("genes")
  testthat::expect_gt(s, 100)
  testthat::expect_lt(s, 100000)

  s <- corpus_size("states")
  testthat::expect_gt(s, 100)
  testthat::expect_lt(s, 100000)

  s <- corpus_size("taxon_annotations")
  testthat::expect_gt(s, 10000)
  testthat::expect_lt(s, 5000000)
  testthat::expect_equal(corpus_size(), s)

  testthat::expect_error(corpus_size("gene_annotations"))
  testthat::expect_error(corpus_size("foobar"))
})

test_that("obtaining/calculating term frequencies", {
  phens <- get_phenotypes(entity = "pectoral fin", quality = "present")
  wt <- term_freqs(phens$id, as = "phenotype")
  testthat::expect_is(wt, "numeric")
  testthat::expect_length(wt, length(phens$id))
  testthat::expect_true(all(wt >= 0))
  testthat::expect_true(all(wt <= 1))

  # check that the corpus defaults to "taxa" passing phenotype IRIS
  wt1 <- term_freqs(phens$id, as = "phenotype", corpus = "taxa")
  testthat::expect_identical(wt1, wt)

  # check that we can use genes corpus with term_freqs passing phenotype IRIS
  wt <- term_freqs(phens$id, as = "phenotype", corpus = "genes")
  testthat::expect_is(wt, "numeric")
  testthat::expect_length(wt, length(phens$id))
  testthat::expect_true(all(wt >= 0))
  testthat::expect_true(all(wt <= 1))

  # check that we can use states corpus with term_freqs passing phenotype IRIS
  wt <- term_freqs(phens$id, as = "phenotype", corpus = "states")
  testthat::expect_is(wt, "numeric")
  testthat::expect_length(wt, length(phens$id))
  testthat::expect_true(all(wt >= 0))
  testthat::expect_true(all(wt <= 1))

  entities <- find_term("basihyal bone")
  # check that we can use taxa corpus with term_freqs passing entity IRIS
  wt <- term_freqs(entities$id, as = "entity", corpus = "taxa")
  testthat::expect_is(wt, "numeric")
  testthat::expect_length(wt, length(entities$id))
  testthat::expect_true(all(wt >= 0))
  testthat::expect_true(all(wt <= 1))

  # check that we can use genes corpus with term_freqs passing entity IRIS
  wt <- term_freqs(entities$id, as = "entity", corpus = "genes")
  testthat::expect_is(wt, "numeric")
  testthat::expect_length(wt, length(entities$id))
  testthat::expect_true(all(wt >= 0))
  testthat::expect_true(all(wt <= 1))

  # check that we can use states corpus with term_freqs passing entity IRIS
  wt <- term_freqs(entities$id, as = "entity", corpus = "states")
  testthat::expect_is(wt, "numeric")
  testthat::expect_length(wt, length(entities$id))
  testthat::expect_true(all(wt >= 0))
  testthat::expect_true(all(wt <= 1))

  # check that we can use states corpus with term_freqs passing entity IRIS with as vector
  as_value = rep("entity", times=nrow(entities))
  wt <- term_freqs(entities$id, as = as_value, corpus = "states")
  testthat::expect_is(wt, "numeric")
  testthat::expect_length(wt, length(entities$id))
  testthat::expect_true(all(wt >= 0))
  testthat::expect_true(all(wt <= 1))

  # checking of error conditions
  testthat::expect_error(term_freqs(phens$id, as = "foobar"))
  testthat::expect_error(term_freqs(phens$id, corpus = "foobar"))
  testthat::expect_error(term_freqs(phens$id, as = c("phenotype", "phenotype")))
  testthat::expect_error(term_freqs(phens$id, as = c(rep("phenotype",
                                                         times = nrow(phens)-1),
                                                     "auto")))
  testthat::expect_error(term_freqs(phens$id, as = "quality", corpus = "taxa"))
  # check that mismatched as values raises an error
  as_value = rep("entity", times=nrow(entities))
  as_value[1] <- "phenotype"
  testthat::expect_error(term_freqs(entities$id, as = as_value, corpus = "states"))
})

test_that("term frequencies for post-comp subsumers of entities", {
  tt <- sapply(c("fin ray", "dorsal fin", "caudal fin"), get_term_iri, as = "anatomy")
  subs <- rownames(subsumer_matrix(tt))
  # reduce to post-comps and test a handful
  onts <- obo_prefix(subs)
  subs <- subs[is.na(onts)]
  # Expect an error since the taxon_annotations corpus is no longer supported.
  testthat::expect_error(term_freqs(subs, as = "entity", corpus = "taxon_annotations"), 
                         "corpus 'taxon_annotations' is currently unsupported")
})

