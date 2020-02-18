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
