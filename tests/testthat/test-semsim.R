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

test_that("edge-based similarity metrics work", {
  tl <- c("pelvic fin", "pectoral fin", "forelimb", "hindlimb", "dorsal fin", "caudal fin")
  tt <- sapply(tl, get_term_iri, as = "anatomy", exactOnly = TRUE)

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

test_that("Resnik similarity", {
  skip_on_cran()

  phens <- get_phenotypes("basihyal bone", taxon = "Cyprinidae")
  subs.mat <- subsumer_matrix(phens$id, .colnames = "label", .labels = phens$label,
                              preserveOrder = TRUE)
  sm.ic <- resnik_similarity(subs.mat,
                             wt_args = list(as = "phenotype", corpus = "taxon-variation"))
  testthat::expect_equal(dim(sm.ic), c(nrow(phens), nrow(phens)))
  testthat::expect_true(all(sm.ic > 0))
  testthat::expect_true(all(sm.ic <= -log10(1 / corpus_size("taxon-variation"))))
  termICs <- -log10(term_freqs(phens$id, as = "phenotype", corpus = "taxon-variation"))
  testthat::expect_equivalent(diag(sm.ic), termICs)
})

test_that("profile similarity with Jaccard", {
  tl <- c("pelvic fin", "pectoral fin", "forelimb", "hindlimb", "dorsal fin", "caudal fin")
  tt <- sapply(tl, get_term_iri, as = "anatomy", exactOnly = TRUE)

  tt.f1 <- c(rep("paired", times = 4), rep("unpaired", times = 2))
  tt.f2 <- c("fins", "fins", "limbs", "limbs", "fins", "fins")
  tt.f3 <- interaction(as.factor(tt.f1), as.factor(tt.f2))
  subs.mat <- subsumer_matrix(tt, .colnames = "label", .labels = names(tt),
                              preserveOrder = TRUE)

  sm <- profile_similarity(jaccard_similarity, subs.mat, f = tt.f1)
  testthat::expect_equal(colnames(sm), levels(as.factor(tt.f1)))
  testthat::expect_equal(rownames(sm), levels(as.factor(tt.f1)))
  testthat::expect_gt(min(sm), 0)
  testthat::expect_equal(max(sm), 1)
  testthat::expect_equivalent(diag(sm), rep(1.0, times = nlevels(as.factor(tt.f1))))

  sm <- profile_similarity(jaccard_similarity, subs.mat, f = tt.f3)
  testthat::expect_equal(colnames(sm), levels(tt.f3)[1:3])
  testthat::expect_equal(rownames(sm), levels(tt.f3)[1:3])
  testthat::expect_gt(min(sm), 0)
  testthat::expect_equal(max(sm), 1)
  testthat::expect_equivalent(diag(sm), rep(1.0, times = nlevels(tt.f3)-1))

  sm1 <- profile_similarity(jaccard_similarity, subs.mat, f = tt.f3, reduce = mean)
  testthat::expect_equal(colnames(sm1), levels(tt.f3)[1:3])
  testthat::expect_equal(rownames(sm1), levels(tt.f3)[1:3])
  testthat::expect_gt(min(sm1), 0)
  testthat::expect_lt(max(sm1), 1)
  testthat::expect_true(all(diag(sm) - diag(sm1) > 0))
  testthat::expect_gt(sum(sm - sm1), 0)

  sm2 <- profile_similarity(jaccard_similarity, subs.mat, f = tt.f3,
                            reduce = reduce.ignoringDiag)
  # ignoring within-group self matches should decrease the diagonal
  sm12 <- sm1 - sm2
  testthat::expect_true(all(diag(sm12) > 0))
  # and should decrease _only_ the diagonal
  diag(sm12) <- NA
  testthat::expect_equal(sum(sm12, na.rm = TRUE), 0)

  sm <- profile_similarity(jaccard_similarity, subs.mat, f = tt.f1,
                           reduce = bestPairs)
  # resulting matrix is asymmetric
  sm.diff <- sm - t(sm)
  testthat::expect_equivalent(diag(sm.diff), rep(0, times = nlevels(as.factor(tt.f1))))
  testthat::expect_false(all(sm.diff[upper.tri(sm.diff)] == 0))
  testthat::expect_false(all(sm.diff[lower.tri(sm.diff)] == 0))
})

test_that("profile similarity with Resnik", {
  skip_on_cran()

  phens <- get_phenotypes("maxilla", taxon = "Cyprinidae")
  subs.mat <- subsumer_matrix(phens$id, .colnames = "label", .labels = phens$label,
                              preserveOrder = TRUE)
  phens.f <- as.factor(sapply(as.phenotype(phens$id), function(phen) {
    if (length(phen$eqs$related_entities) > 0) "relational" else "monadic"
  }))

  freqs <- term_freqs(rownames(subs.mat), as = "phenotype", corpus = "taxon-variation")
  toKeep <- ! (is.na(freqs) | freqs == 0)
  freqs <- freqs[toKeep]
  subs.mat <- subs.mat[toKeep,]
  sm <- profile_similarity(resnik_similarity, subs.mat, wt = -log10(freqs),
                           f = phens.f)
  testthat::expect_equal(colnames(sm), levels(phens.f))
  testthat::expect_equal(rownames(sm), levels(phens.f))
  testthat::expect_gt(min(sm), 0)
  testthat::expect_lte(max(sm), -log10(1 / corpus_size("taxon-variation")))

  # for Resnik as metric, group-wise is the same as pair-wise with maxIC
  sm1 <- profile_similarity(resnik_similarity, subs.mat, wt = -log10(freqs),
                            f = phens.f, reduce = max)
  testthat::expect_equal(sm, sm1)
})
