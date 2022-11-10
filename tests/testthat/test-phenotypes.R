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
  # basihyal bone phenotypes
  phens <- get_phenotypes(entity = "basihyal bone")
  # studies for basihyal bone
  studies <- get_studies(entity = "basihyal bone")

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
  phens.match <- phenotype_matches(c("foobar", phens$id), studies = studies$id)
  testthat::expect_is(phens.match, "logical")
  testthat::expect_length(phens.match, length(phens$id) + 1)
  testthat::expect_false(all(phens.match))
  testthat::expect_true(all(phens.match[-1]))
})

test_that("creating phenotype objects and extracting properties", {
  # basihyal bone phenotypes
  phens <- get_phenotypes(entity = "basihyal bone")

  # not a phenotype object
  testthat::expect_false(is.phenotype(phens))
  testthat::expect_false(is.phenotype(phens[1,]))
  testthat::expect_false(is.phenotype(phens$id[1]))

  # create one phenotype object
  obj <- as.phenotype(phens[1, "id"])
  testthat::expect_is(obj, "phenotype")
  testthat::expect_true(is.phenotype(obj))
  testthat::expect_true(is_valid_phenotype(obj))

  # optionally include taxa
  testthat::expect_false("taxa" %in% names(obj))
  obj <- as.phenotype(phens[1, "id"], withTaxa = TRUE)
  testthat::expect_true(is.phenotype(obj))
  testthat::expect_true(is_valid_phenotype(obj))
  testthat::expect_true("taxa" %in% names(obj))
  testthat::expect_gt(nrow(obj$taxa), 1)

  # robust to unresolving IDs
  testthat::expect_warning(obj <- as.phenotype("foo"))
  testthat::expect_is(obj, "phenotype")
  testthat::expect_true(is.phenotype(obj))
  testthat::expect_false(is_valid_phenotype(obj))

  # also works with data.frame as input
  obj <- as.phenotype(phens[1,])
  testthat::expect_is(obj, "phenotype")
  testthat::expect_true(is_valid_phenotype(obj))

  # can run vectorized
  objs <- as.phenotype(phens$id)
  testthat::expect_length(objs, nrow(phens))
  testthat::expect_is(objs, "list")
  testthat::expect_true(all(sapply(objs, is.phenotype)))
  testthat::expect_length(is_valid_phenotype(objs), length(objs))
  testthat::expect_true(all(is_valid_phenotype(objs)))
  l <- sapply(objs, function(p) p$label)
  testthat::expect_true(all(l == phens$label))
  testthat::expect_true(all(sapply(objs, function(p) nrow(p$states)) > 0))
  testthat::expect_true(all(sapply(objs, function(p) length(p$eqs$entities)) > 0))
  testthat::expect_true(all(sapply(objs, function(p) length(p$eqs$qualities)) > 0))
})

test_that("extracting properties from phenotype objects", {
  # basihyal bone phenotypes
  phens.all <- get_phenotypes(entity = "basihyal bone")
  phens <- phens.all[1,]

  # create one phenotype object
  obj <- as.phenotype(phens$id)

  # obtain character states using ID
  states <- charstates(phens)
  testthat::expect_equivalent(states, obj$states)
  chs <- chars(phens)
  testthat::expect_is(chs, "data.frame")
  testthat::expect_lte(nrow(chs), nrow(states))
  testthat::expect_true(all(chs[, "character.id"] %in% states[, "character.id"]))

  # charstates() also can aggregate over a list
  states.all <- charstates(phens.all)
  testthat::expect_equal(nrow(states.all),
                         sum(sapply(phens.all$id,
                                    function(id) {
                                      p <- as.phenotype(id)
                                      nrow(p$states)
                                    })))
  testthat::expect_length(unique(states.all[,"phenotype.id"]), nrow(phens.all))
  states.p1 <- states.all[states.all[,"phenotype.id"] == phens$id, c(-1,-2)]
  testthat::expect_length(colnames(states.p1), length(colnames(states)))
  colnames(states.p1) <- colnames(states)
  testthat::expect_equivalent(states.p1, states)
})

test_that("pretty-printing phenotype objects", {
  # basihyal bone phenotypes
  phens <- get_phenotypes(entity = "basihyal bone")[1,]

  # create one phenotype object
  obj <- as.phenotype(phens$id)

  testthat::expect_output(print(obj), "basihyal bone")
  testthat::expect_output(print(obj),
                          paste("<", obj$eqs$entities[1], ">", sep = ""))
  testthat::expect_output(print(obj),
                          paste("<", obj$eqs$qualities[1], ">", sep = ""))

  # does not bomb for an invalid phenotype object
  testthat::expect_warning(foo <- as.phenotype("http://foo"))
  testthat::expect_output(print(foo), "'http://foo'")
  testthat::expect_output(print(foo), "No states")
  testthat::expect_output(print(foo), "No EQs")

  # does not bomb for phenotype without states
  testthat::expect_output(
    print(as.phenotype("http://purl.obolibrary.org/obo/MP_0030825")))
})
