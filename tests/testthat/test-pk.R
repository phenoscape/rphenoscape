context("term finding and info APIs")

test_that("Test term details", {
  skip_on_cran()
  b <- pk_phenotype_detail("shape")
  c <- pk_anatomical_detail("basihyal bone")

  g <- pk_gene_detail("socs5")
  gg <- pk_gene_detail("socs5", "Danio rerio")

  expect_is(b, 'data.frame')
  expect_equal(nrow(b), 1)
  expect_false(any(is.na(b)))
  expect_is(c, 'data.frame')
  expect_equal(nrow(c), 1)
  expect_false(any(is.na(c)))
  

  expect_warning(bb <- pk_phenotype_detail("shape tt"))
  expect_warning(cc <- pk_anatomical_detail("fin tt"))
  expect_true(is.na(bb))
  expect_true(is.na(cc))

  expect_is(g, "data.frame")
  expect_gt(length(unique(g$taxon.label)), 2)
  expect_setequal(unique(g$matchType), c("exact", "partial"))
  expect_is(gg, "data.frame")
  expect_length(unique(gg$taxon.label), 1)
  expect_length(unique(gg$label), 2)
})

test_that("taxon details works", {
  skip_on_cran()
  a <- pk_taxon_detail("Coralliozetus")

  expect_is(a, 'data.frame')
  expect_gt(nrow(a), 0)
  expect_setequal(colnames(a),
                  c("id", "label", "extinct", "rank.id", "rank.label", "common_name"))

  expect_warning(aa <- pk_taxon_detail("coral tt"))
  expect_warning(bb <- pk_taxon_detail("http://foobar.com/taxon"))
  expect_warning(cc <- pk_taxon_detail(c("coral tt", "Danio rerio")))
  expect_true(is.na(aa))
  expect_true(is.na(bb))
  expect_true(any(is.na(cc)))
  expect_false(all(is.na(cc)))
  expect_equal(nrow(cc), 2)
  expect_true(all(is.na(cc[1,])))

  dd <- pk_taxon_detail(c("Danio", "Danio rerio"))
  expect_equal(nrow(dd), 2)
  expect_true(any(is.na(dd$common_name)))
  expect_false(all(is.na(dd$common_name)))
})

test_that("is_extinct works", {
  skip_on_cran()

  ext <- pk_is_extinct("Fisherichthys")
  expect_is(ext, "logical")
  expect_equal(names(ext), c("Fisherichthys"))
  # make one an unresolvable typo
  expect_warning(ext <- pk_is_extinct(c("Fisheririchthys", "Tiktaalik")))
  expect_is(ext, "logical")
  expect_length(ext, 2)
  expect_true(any(is.na(ext)))
  expect_false(all(is.na(ext)))
  expect_equal(names(ext), c("Fisheririchthys", "Tiktaalik"))
})

test_that("Test retrieving IRI", {
  skip_on_cran()

  i <- get_term_iri("Coralliozetus", "vto")
  expect_equal(i, "http://purl.obolibrary.org/obo/VTO_0042955")
  expect_warning(ii <- get_term_iri("Coralliozetus TT", "vto"))
  expect_warning(iii <- get_term_iri("Coralliozetus", "pato"))
  expect_true(is.na(ii))
  expect_true(is.na(iii))

  tiris <- find_term("pelvic fin", definedBy = NA, matchTypes = c("exact"))
  expect_gte(nrow(tiris), 1)
  expect_silent(tiri <- get_term_iri("pelvic fin", as = NA, exactOnly = TRUE))
  expect_equal(tiri, "http://purl.obolibrary.org/obo/UBERON_0000152")

  tiris <- find_term("part_of", definedBy = NA)
  expect_true("isDefinedBy" %in% colnames(tiris))
  expect_true(all(is.na(tiris$isDefinedBy)))

  expect_warning(tiri <- get_term_iri("anatomical structure", as = NA, exactOnly = TRUE))
  expect_is(tiri, "character")
  expect_true(startsWith(tiri, "http://purl.obolibrary.org/obo/"))
})

test_that("Deprecated function forr retrieving IRI", {
  skip_on_cran()

  expect_warning(tt <- pk_get_iri("Coralliozetus", "vto"))
  expect_equal(tt, "http://purl.obolibrary.org/obo/VTO_0042955")
  expect_equal(tt, get_term_iri("Coralliozetus", "vto"))

  expect_warning(tt <- pk_get_iri("pelvic fin", as = NA, exactOnly = TRUE))
  expect_equal(tt, "http://purl.obolibrary.org/obo/UBERON_0000152")
  expect_equal(tt, get_term_iri("pelvic fin", as = NA, exactOnly = TRUE))
})

test_that("Test getting labels", {
  tt <- c("http://purl.obolibrary.org/obo/UBERON_0000981",
          "http://purl.obolibrary.org/obo/UBERON_0002103",
          "http://purl.obolibrary.org/obo/UBERON_0000976",
          "http://purl.obolibrary.org/obo/UBERON_0002102")
  lbls <- get_term_label(tt)
  testthat::expect_equal(length(tt), nrow(lbls))
  testthat::expect_false(any(is.na(lbls$label)))
  testthat::expect_setequal(tt, lbls$id)

  lbls <- get_term_label(tt, preserveOrder = TRUE)
  testthat::expect_equal(tt, lbls$id)

  testthat::expect_silent(lbls <- get_term_label(c(tt, "http://foo")))
  testthat::expect_equal(sum(is.na(lbls$label)), 1)
  testthat::expect_equal(lbls$id[is.na(lbls$label)], "http://foo")

  lbls <- get_term_label(tt[1])
  testthat::expect_equal(nrow(lbls), 1)
  testthat::expect_false(is.na(lbls$label))

  lbls <- get_term_label("http://foobar")
  testthat::expect_equal(nrow(lbls), 1)
  testthat::expect_equal(lbls$id, "http://foobar")
  testthat::expect_true(is.na(lbls$label))
})

test_that("labels for pre-generated post-comps", {
  phen <- sample(get_phenotypes("basihyal bone")$id, size = 1)
  subs <- sample(rownames(subsumer_matrix(phen)), size = 30)
  subs.l <- get_term_label(subs, preserveOrder = TRUE)
  testthat::expect_lte(sum(is.na(subs.l$label)), 1)

  subs <- sample(rownames(subsumer_matrix(c("femur"))), 30)
  subs.l <- get_term_label(subs, preserveOrder = TRUE)
  # Unfortunately, there are some regular ontologies for which the database
  # does not consistently have labels. Filter those out.
  ontFilter <- Reduce(
    function(v1, v2) v1 | startsWith(subs.l$id, v2),
    paste0("http://purl.obolibrary.org/obo/", c("CARO"), "_"),
    init = rep(FALSE, times = length(subs.l$id))
  )
  subs.l <- subs.l[! (is.na(subs.l$label) & ontFilter),]
  testthat::expect_lte(sum(is.na(subs.l$label)), 1)
})

test_that("Test getting study information", {
    skip_on_cran()
    # backwards compatible mode, defaults to including part_of
    slist1 <- pk_get_study_list(taxon = "Siluridae", entity = "fin")
    expect_is(slist1, "data.frame")
    expect_gt(nrow(slist1), 0)

    # only subsumption, no parts or other relationships
    slist2 <- pk_get_study_list(taxon = "Siluridae", entity = "fin", includeRels = FALSE)
    expect_is(slist2, "data.frame")
    expect_gt(nrow(slist2), 0)
    expect_gt(nrow(slist1), nrow(slist2))

    # all supported relationships
    slist3 <- pk_get_study_list(taxon = "Siluridae", entity = "fin", includeRels = TRUE)
    expect_is(slist3, "data.frame")
    expect_gt(nrow(slist3), 0)
    expect_gt(nrow(slist3), nrow(slist2))
    expect_gte(nrow(slist3), nrow(slist1))

    # subsumption and part_of relationships
    slist4 <- pk_get_study_list(taxon = "Siluridae", entity = "fin", includeRels = c("part of"))
    expect_is(slist4, "data.frame")
    expect_gt(nrow(slist4), 0)
    expect_gt(nrow(slist4), nrow(slist2))
    expect_equal(nrow(slist4), nrow(slist1))

    # using prefixes for relationship names works
    slist5 <- pk_get_study_list(taxon = "Siluridae", entity = "fin",
                                includeRels = c("part", "historical", "serial"))
    expect_is(slist5, "data.frame")
    expect_gt(nrow(slist5), 0)
    expect_gt(nrow(slist5), nrow(slist2))
    expect_equal(nrow(slist5), nrow(slist3))

    # filtering by quality works as well
    slist6 <- pk_get_study_list(taxon = "Siluridae", entity = "fin", quality = "size")
    expect_is(slist6, "data.frame")
    expect_gt(nrow(slist6), 0)
    expect_lt(nrow(slist6), nrow(slist4))

    # can also obtain all studies for taxon
    slist7.1 <- pk_get_study_list(taxon = "Siluriformes")
    slist7.2 <- pk_get_study_list(taxon = "Siluriformes", includeRels = FALSE)
    expect_is(slist7.1, "data.frame")
    expect_gt(nrow(slist7.1), 0)
    expect_gt(nrow(slist7.1), 2 * nrow(slist3))
    expect_equal(nrow(slist7.1), nrow(slist7.2))

    # can also obtain all studies for entity
    slist8.1 <- pk_get_study_list(entity = "pelvic fin")
    slist8.2 <- pk_get_study_list(entity = "pelvic fin", includeRels = FALSE)
    slist8.3 <- pk_get_study_list(entity = "pelvic fin", includeRels = c("serial","historical"))
    expect_is(slist8.1, "data.frame")
    expect_gt(nrow(slist8.1), nrow(slist3))
    expect_gt(nrow(slist8.1), nrow(slist7.1))
    expect_gt(nrow(slist8.1), nrow(slist8.2))
    expect_gt(nrow(slist8.3), nrow(slist8.2))

    # can also filter by phenotype
    phens <- get_phenotypes(entity = "pelvic fin")
    slist8.4 <- pk_get_study_list(phenotype = phens$id[1])
    expect_is(slist8.4, "data.frame")
    expect_gt(nrow(slist8.4), 0)
    expect_lt(nrow(slist8.4), nrow(slist8.1))

    # can also obtain all studies by leaving off all filters
    slist9 <- pk_get_study_list()
    expect_is(slist9, "data.frame")
    expect_gt(nrow(slist9), 0)
    expect_gt(nrow(slist9), 20 * nrow(slist3))

    s1 <- get_study_data(slist1[1,"id"])
    expect_is(s1[[1]], 'nexml')

    ss1 <- pk_get_study(s1)
    expect_is(ss1[[1]], 'data.frame')

    sss1 <- pk_get_study_meta(s1)
    expect_is(sss1[[1]], 'list')
    expect_is(sss1[[1]]$id_taxa, 'data.frame')

})



