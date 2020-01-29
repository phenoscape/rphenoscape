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

  # filter also by quality
  phens2 <- get_phenotypes(entity = "pelvic fin", quality = "shape")
  testthat::expect_gt(nrow(phens2), 0)
  testthat::expect_lt(nrow(phens2), nrow(phens1))

  # filter also by quality and taxon
  phens3 <- get_phenotypes(entity = "pelvic fin", quality = "shape", taxon = "Siluriformes")
  testthat::expect_gt(nrow(phens3), 0)
  testthat::expect_lt(nrow(phens3), nrow(phens2))
})

test_that("get_phenotypes() in apply() works", {
  tt <- c("pelvic fin", "antorbital")
  qual <- "shape"

  testthat::expect_silent(res <- lapply(tt, get_phenotypes))
  testthat::expect_is(res, "list")
  testthat::expect_length(res, 2)
  testthat::expect_silent(res <- lapply(tt, get_phenotypes, quality = qual))
  testthat::expect_is(res, "list")
  testthat::expect_length(res, 2)
})

test_that("requesting taxon phenotype associations works", {
  # filter by entity, quality and taxon
  phens <- get_phenotypes(entity = "pelvic fin", quality = "shape", taxon = "Siluriformes")
  # same but request (phenotype, taxon) tuples instead
  phens.tpl <- get_phenotypes(entity = "pelvic fin", quality = "shape", taxon = "Siluriformes",
                              .withTaxon = TRUE)
  # additional columns
  testthat::expect_gt(ncol(phens.tpl), ncol(phens))
  testthat::expect_true(all(c("id", "label") %in% colnames(phens.tpl)))
  testthat::expect_true(all(c("taxon.id", "taxon.label") %in% colnames(phens.tpl)))
  i <- seq(1, ncol(phens.tpl))
  # additional columns come last
  testthat::expect_gt(min(i[startsWith(colnames(phens.tpl), "taxon")]),
                      max(i[! startsWith(colnames(phens.tpl), "taxon")]))
  # should see many more tuples than phenotypes
  testthat::expect_gt(nrow(phens.tpl), 10 * nrow(phens))
  # but same number of phenotypes
  testthat::expect_equal(nrow(unique(phens.tpl[, c("id", "label")])), nrow(phens))
  # taxa are redundant as well
  testthat::expect_lt(length(unique(phens.tpl[, c("taxon.id")])), nrow(phens.tpl))
})

test_that("matching phenotypes against study filter", {
  # pelvic fin phenotypes
  phens <- get_phenotypes(entity = "basihyal bone")
  # studies for pelvic fin
  studies <- pk_get_study_list(entity = "basihyal bone")

  # match one against one study
  phens.match <- phenotype_matches(phens$id[1], studies = studies$id[1])
  testthat::expect_is(phens.match, "logical")
  testthat::expect_length(phens.match, 1)
  # match one against many studies
  phens.match <- phenotype_matches(phens$id[1], studies = studies$id)
  testthat::expect_is(phens.match, "logical")
  testthat::expect_length(phens.match, 1)
  testthat::expect_true(phens.match)
  # match many phenotypes against many studies
  phens.match <- phenotype_matches(phens$id, studies = studies$id)
  testthat::expect_is(phens.match, "logical")
  testthat::expect_length(phens.match, length(phens$id))
  testthat::expect_true(all(phens.match))
  phens.match <- phenotype_matches(phens$id, studies = studies$id[1:2])
  testthat::expect_is(phens.match, "logical")
  testthat::expect_length(phens.match, length(phens$id))
  testthat::expect_true(any(phens.match))
  testthat::expect_false(all(phens.match))

  # also accepts data.frame instead of vector for convenience
  phens.match <- phenotype_matches(phens, studies = studies)
  testthat::expect_is(phens.match, "logical")
  testthat::expect_length(phens.match, length(phens$id))
  testthat::expect_true(all(phens.match))

  # tolerates phenotype IDs that aren't found
  testthat::expect_warning(
    phens.match <- phenotype_matches(c("foobar", phens$id), studies = studies$id))
  testthat::expect_is(phens.match, "logical")
  testthat::expect_length(phens.match, length(phens$id) + 1)
  testthat::expect_false(all(phens.match))
  testthat::expect_true(all(phens.match[-1]))
})