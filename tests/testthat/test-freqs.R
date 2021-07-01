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
  type.fracs <- table(tt.types)/nrow(subs.mat)
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

  s <- corpus_size("taxon_annotations")
  testthat::expect_gt(s, 10000)
  testthat::expect_lt(s, 5000000)
  testthat::expect_equal(corpus_size(), s)

  testthat::expect_error(corpus_size("gene_annotations"))
  testthat::expect_error(corpus_size("foobar"))
})

test_that("obtaining/calculating term frequencies", {
  tl <- c("pelvic fin", "pectoral fin", "forelimb", "hindlimb", "dorsal fin", "caudal fin")
  tt <- sapply(tl, get_term_iri, as = "anatomy", exactOnly = TRUE)

  wt <- term_freqs(tt, corpus = "taxon_annotations")
  testthat::expect_is(wt, "numeric")
  testthat::expect_length(wt, length(tt))
  testthat::expect_true(all(wt >= 0))
  testthat::expect_true(all(wt <= 1))

  wt1 <- term_freqs(tt, as = "auto", corpus = "taxon_annotations")
  testthat::expect_identical(wt1, wt)
  wt1 <- term_freqs(tt, as = "entity", corpus = "taxon_annotations")
  testthat::expect_identical(wt1, wt)
  wt1 <- term_freqs(tt)
  testthat::expect_identical(wt1, wt)
  wt1 <- term_freqs(tt, as = c(rep("entity", times = 5), "quality"))
  testthat::expect_false(all(wt1 == wt))
  testthat::expect_true(all(wt1[1:5] == wt[1:5]))
  testthat::expect_equal(wt1[6], 0)

  phens <- get_phenotypes(entity = "pelvic fin", quality = "shape")
  wt <- term_freqs(phens$id, as = "phenotype", corpus = "taxon_annotations")
  testthat::expect_length(wt, nrow(phens))
  testthat::expect_true(all(wt >= 0))
  testthat::expect_true(all(wt <= 1))
  wt1 <- term_freqs(phens$id, as = "phenotype", corpus = "taxa")
  testthat::expect_length(wt1, nrow(phens))
  testthat::expect_true(all(wt1 >= 0))
  testthat::expect_true(all(wt1 <= 1))
  # expect 80%+ of the taxa freqs to be > than the taxon annotations freqs
  testthat::expect_gt(mean(wt < wt1), .8)
  # can use defaults
  wt1 <- term_freqs(phens$id[1:3])
  testthat::expect_identical(wt1, wt[1:3])

  # checking of error conditions
  testthat::expect_error(term_freqs(phens$id, as = "foobar"))
  testthat::expect_error(term_freqs(phens$id, corpus = "foobar"))
  testthat::expect_error(term_freqs(phens$id, as = c("phenotype", "phenotype")))
  testthat::expect_error(term_freqs(phens$id, as = c(rep("phenotype",
                                                         times = nrow(phens)-1),
                                                     "auto")))
  testthat::expect_error(term_freqs(phens$id, as = "entity", corpus = "taxa"))
})

test_that("term frequencies for post-comp subsumers of entities", {
  tt <- sapply(c("fin ray", "dorsal fin", "caudal fin"), get_term_iri, as = "anatomy")
  subs <- rownames(subsumer_matrix(tt))
  # reduce to post-comps and test a handful
  onts <- obo_prefix(subs)
  subs <- subs[is.na(onts)]
  if (length(subs) > 5) subs <- subs[1:5]
  freqs <- term_freqs(subs, as = "entity", corpus = "taxon_annotations")
  testthat::expect_true(any(freqs > 0))
})

