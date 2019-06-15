context("semantic similarity")

test_that("obtaining subsumer matrix works", {
  tt <- c("http://purl.obolibrary.org/obo/UBERON_0000981",
          "http://purl.obolibrary.org/obo/UBERON_0002103",
          "http://purl.obolibrary.org/obo/UBERON_0000976",
          "http://purl.obolibrary.org/obo/UBERON_0002102")
  tl <- c("femur", "hindlimb", "humerus", "forelimb")
  
  m <- subsumer_matrix(tt, .colnames = "IRI")
  testthat::expect_setequal(colnames(m), tt)
  testthat::expect_true(all(m == 1 | m == 0))
  testthat::expect_gt(dim(m)[1], dim(m)[2] * 10)

  m2 <- subsumer_matrix(tt, .colnames = "IRI", preserveOrder = TRUE)
  testthat::expect_equal(colnames(m2), tt)
  testthat::expect_equal(dim(m2), dim(m))

  m <- subsumer_matrix(tt)
  testthat::expect_true("prefixes" %in% names(attributes(m)))
  testthat::expect_true(all(attr(m, "prefixes") == "http://purl.obolibrary.org/obo/"))
  testthat::expect_setequal(paste0(attr(m, "prefixes"), colnames(m)), tt)
  
  m <- subsumer_matrix(tt, .colnames = "label")
  testthat::expect_true("term.iris" %in% names(attributes(m)))
  testthat::expect_setequal(attr(m, "term.iris"), tt)
  testthat::expect_setequal(colnames(m), tl)
  
  tl1 <- sub("limb", "", tl)
  tl1[3] <- NA
  m <- subsumer_matrix(tt, .colnames = "label", .labels = tl1, preserveOrder = TRUE)
  testthat::expect_false(any(is.na(colnames(m))))
  testthat::expect_equal(attr(m, "term.iris"), tt)
  testthat::expect_equal(colnames(m)[-3], tl1[-3])
  testthat::expect_equal(colnames(m)[3], "humerus")
})

test_that("similarity metrics work", {
  tl <- c("pelvic fin", "pectoral fin", "forelimb", "hindlimb", "dorsal fin", "caudal fin")
  tt <- sapply(tl, pk_get_iri, as = "anatomy", exactOnly = TRUE)

  sim.jc <- jaccard_similarity(terms = tt, .colnames = "label", .labels = tl)
  testthat::expect_equal(row.names(sim.jc), colnames(sim.jc))
  testthat::expect_setequal(colnames(sim.jc), tl)
  testthat::expect_equivalent(diag(sim.jc), rep(1.0, times = length(tl)))
  testthat::expect_equal(max(sim.jc), 1.0)
  testthat::expect_gt(sim.jc["pectoral fin", "forelimb"], sim.jc["pectoral fin", "dorsal fin"])

  sim.tnm <- tanimoto_similarity(terms = tt, .colnames = "label", .labels = tl)
  testthat::expect_equal(sim.tnm, sim.jc)

  sim.cos <- cosine_similarity(terms = tt, .colnames = "label", .labels = tl)
  testthat::expect_equal(row.names(sim.cos), colnames(sim.cos))
  testthat::expect_setequal(colnames(sim.cos), tl)
  testthat::expect_equivalent(diag(sim.cos), rep(1.0, times = length(tl)))
  testthat::expect_equal(max(sim.cos), 1.0)
  testthat::expect_gt(sim.cos["pectoral fin", "forelimb"], sim.jc["pectoral fin", "dorsal fin"])
})