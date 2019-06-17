context("phenotypes API")

test_that("retrieval of phenotypes works", {
  phens1 <- get_phenotypes(entity = "pelvic fin")

  testthat::expect_equal(colnames(phens1), c("id", "label"))
  testthat::expect_gt(nrow(phens1), 100)

  # by default, parts are already included
  testthat::expect_lt(nrow(get_phenotypes(entity = "pelvic fin", includeRels = FALSE)),
                      nrow(phens1))
  phens2 <- get_phenotypes(entity = "pelvic fin", includeRels = c("part"))
  testthat::expect_equal(nrow(phens2), nrow(phens1))
  testthat::expect_setequal(phens2$id, phens1$id)
  
  # but historical homologues are not
  phens2 <- get_phenotypes(entity = "pelvic fin", includeRels = c("part", "hist"))
  testthat::expect_gt(nrow(phens2), nrow(phens1))
  # nor are serial homologues
  testthat::expect_gt(nrow(get_phenotypes(entity = "pelvic fin",
                                          includeRels = c("part", "hist", "serial"))),
                      nrow(phens2))

  # filter by quality
  phens2 <- get_phenotypes(entity = "pelvic fin", quality = "shape")
  testthat::expect_gt(nrow(phens2), 0)
  testthat::expect_lt(nrow(phens2), nrow(phens1))

  # filter by quality and taxon
  phens3 <- get_phenotypes(entity = "pelvic fin", quality = "shape", taxon = "Siluriformes")
  testthat::expect_gt(nrow(phens3), 0)
  testthat::expect_lt(nrow(phens3), nrow(phens2))
})