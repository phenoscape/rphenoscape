context("dependency matrix API")

test_that("presence-absence dependency matrix", {
  tt <- c("http://purl.obolibrary.org/obo/UBERON_0000981",
          "http://purl.obolibrary.org/obo/UBERON_0002103",
          "http://purl.obolibrary.org/obo/UBERON_0000976",
          "http://purl.obolibrary.org/obo/UBERON_0002102")
  tl <- c("femur", "hindlimb", "humerus", "forelimb")

  m <- pa_dep_matrix(tt)
  testthat::expect_equal(row.names(m), colnames(m))
  testthat::expect_true("prefixes" %in% names(attributes(m)))
  testthat::expect_true(all(attr(m, "prefixes") == "http://purl.obolibrary.org/obo/"))
  testthat::expect_setequal(paste0(attr(m, "prefixes"), row.names(m)), tt)

  m <- pa_dep_matrix(tt, .names = "label")
  testthat::expect_equal(row.names(m), colnames(m))
  testthat::expect_true("term.iris" %in% names(attributes(m)))
  testthat::expect_setequal(attr(m, "term.iris"), tt)
  testthat::expect_setequal(row.names(m), tl)

  tl1 <- sub("limb", "", tl)
  tl1[3] <- NA
  m <- pa_dep_matrix(tt, .names = "label", .labels = tl1, preserveOrder = TRUE)
  testthat::expect_equal(row.names(m), colnames(m))
  testthat::expect_false(any(is.na(row.names(m))))
  testthat::expect_equal(attr(m, "term.iris"), tt)
  testthat::expect_equal(row.names(m)[-3], tl1[-3])
  testthat::expect_equal(row.names(m)[3], "humerus")
})
