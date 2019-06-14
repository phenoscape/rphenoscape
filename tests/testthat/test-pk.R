context("term finding and info APIs")

test_that("Test term search", {
  skip_on_cran()
  a <- pk_taxon_detail("Coralliozetus")
  b <- pk_phenotype_detail("shape")
  c <- pk_anatomical_detail("basihyal bone")

  g <- pk_gene_detail("socs5")
  gg <- pk_gene_detail("socs5", "Danio rerio")

  expect_is(a, 'data.frame')
  expect_is(b, 'data.frame')
  expect_is(c, 'data.frame')


  expect_warning(aa <- pk_taxon_detail("coral tt"))
  expect_warning(bb <- pk_phenotype_detail("shape tt"))
  expect_warning(cc <- pk_anatomical_detail("fin tt"))
  expect_true(is.na(aa))
  expect_true(is.na(bb))
  expect_true(is.na(cc))

  expect_is(g, "data.frame")
  expect_is(gg, "data.frame")
})

test_that("Test retrieving IRI", {
  skip_on_cran()

  i <- pk_get_iri("Coralliozetus", "vto")
  expect_equal(i, "http://purl.obolibrary.org/obo/VTO_0042955")
  expect_warning(ii <- pk_get_iri("Coralliozetus TT", "vto"))
  expect_warning(iii <- pk_get_iri("Coralliozetus", "pato"))
  expect_true(is.na(ii))
  expect_true(is.na(iii))

  tiris <- find_term("pelvic fin", definedBy = NA, matchTypes = c("exact"))
  expect_gt(nrow(tiris), 1)
  expect_silent(tiri <- pk_get_iri("pelvic fin", as = NA, exactOnly = TRUE))
  expect_equal(tiri, "http://purl.obolibrary.org/obo/UBERON_0000152")

  tiris <- find_term("part_of", definedBy = NA)
  expect_true("isDefinedBy" %in% colnames(tiris))
  expect_true(all(is.na(tiris$isDefinedBy)))

  expect_warning(tiri <- pk_get_iri("anatomical structure", as = NA, exactOnly = TRUE))
  expect_is(tiri, "character")
  expect_true(startsWith(tiri, "http://purl.obolibrary.org/obo/"))
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

  testthat::expect_warning(lbls <- get_term_label(c(tt, "http://foo")))
  testthat::expect_equal(sum(is.na(lbls$label)), 1)
  testthat::expect_equal(lbls$id[is.na(lbls$label)], "http://foo")
})

test_that("Test getting classification information", {
  skip_on_cran()
  t <- pk_taxon_class("Fisherichthys")
  tt <- pk_taxon_class("Fisherichthys folmeri")

  a <- pk_anatomical_class("fin")
  p <- pk_phenotype_class("shape")

  expect_output(str(t), 'List of 5')
  expect_output(str(tt), 'List of 5')
  expect_warning(ttt <- pk_taxon_class("Fisherichthys TT"))
  expect_true(is.na(ttt))

  expect_output(str(a), 'List of 5')
  expect_warning(aa <- pk_anatomical_class("fin FF"))
  expect_true(is.na(aa))

  expect_output(str(p), 'List of 5')
  expect_warning(pp <- pk_phenotype_class("shape SS"))
  expect_true(is.na(pp))

})

test_that("Test Descendant/Ancestor", {
  skip_on_cran()

  # taxon terms:
  expect_equal(pk_is_descendant("Halecostomi", c("Halecostomi", "Icteria", "Sciaenidae")),
               c(FALSE, FALSE, TRUE))
  expect_equal(pk_is_ancestor("Sciaenidae", c("Halecostomi", "Abeomelomys", "Sciaenidae")),
               c(TRUE, FALSE, FALSE))

  # anatomical entities:
  expect_equal(pk_is_descendant("paired fin", c("pectoral fin", "pelvic fin", "dorsal fin")),
               c(TRUE, TRUE, FALSE))
  expect_equal(pk_is_ancestor("pelvic fin", c("paired fin", "hindlimb", "fin")),
               c(TRUE, FALSE, TRUE))

  # phenotypic quality
  expect_equal(pk_is_ancestor("triangular", c("shape", "color", "amount")),
               c(TRUE, FALSE, FALSE))
  expect_equal(pk_is_descendant("shape", c("T-shaped", "star shaped", "yellow")),
               c(TRUE, TRUE, FALSE))
})

#
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

    # can also obtain all studies by leaving off all filters
    slist9 <- pk_get_study_list()
    expect_is(slist9, "data.frame")
    expect_gt(nrow(slist9), 0)
    expect_gt(nrow(slist9), 20 * nrow(slist3))

    s1 <- pk_get_study_xml(slist1[1,"id"])
    expect_is(s1[[1]], 'nexml')

    ss1 <- pk_get_study(s1)
    expect_is(ss1[[1]], 'data.frame')

    sss1 <- pk_get_study_meta(s1)
    expect_is(sss1[[1]], 'list')
    expect_is(sss1[[1]]$id_taxa, 'data.frame')

})



