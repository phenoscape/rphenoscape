context("finding studies")

test_that("Test getting study information", {
  skip_on_cran()
  # backwards compatible mode, defaults to including part_of
  slist1 <- get_studies(taxon = "Siluridae", entity = "fin")
  expect_is(slist1, "data.frame")
  expect_gt(nrow(slist1), 0)
  
  # only subsumption, no parts or other relationships
  slist2 <- get_studies(taxon = "Siluridae", entity = "fin", includeRels = FALSE)
  expect_is(slist2, "data.frame")
  expect_gt(nrow(slist2), 0)
  expect_gt(nrow(slist1), nrow(slist2))
  
  # all supported relationships
  slist3 <- get_studies(taxon = "Siluridae", entity = "fin", includeRels = TRUE)
  expect_is(slist3, "data.frame")
  expect_gt(nrow(slist3), 0)
  expect_gt(nrow(slist3), nrow(slist2))
  expect_gte(nrow(slist3), nrow(slist1))
  
  # subsumption and part_of relationships
  slist4 <- get_studies(taxon = "Siluridae", entity = "fin", includeRels = c("part of"))
  expect_is(slist4, "data.frame")
  expect_gt(nrow(slist4), 0)
  expect_gt(nrow(slist4), nrow(slist2))
  expect_equal(nrow(slist4), nrow(slist1))
  
  # using prefixes for relationship names works
  slist5 <- get_studies(taxon = "Siluridae", entity = "fin",
                        includeRels = c("part", "historical", "serial"))
  expect_is(slist5, "data.frame")
  expect_gt(nrow(slist5), 0)
  expect_gt(nrow(slist5), nrow(slist2))
  expect_equal(nrow(slist5), nrow(slist3))
  
  # filtering by quality works as well
  slist6 <- get_studies(taxon = "Siluridae", entity = "fin", quality = "size")
  expect_is(slist6, "data.frame")
  expect_gt(nrow(slist6), 0)
  expect_lt(nrow(slist6), nrow(slist4))
  
  # can also obtain all studies for taxon
  slist7.1 <- get_studies(taxon = "Siluriformes")
  slist7.2 <- get_studies(taxon = "Siluriformes", includeRels = FALSE)
  expect_is(slist7.1, "data.frame")
  expect_gt(nrow(slist7.1), 0)
  expect_gt(nrow(slist7.1), 2 * nrow(slist3))
  expect_equal(nrow(slist7.1), nrow(slist7.2))
  
  # can also obtain all studies for entity
  slist8.1 <- get_studies(entity = "pelvic fin")
  slist8.2 <- get_studies(entity = "pelvic fin", includeRels = FALSE)
  slist8.3 <- get_studies(entity = "pelvic fin", includeRels = c("serial","historical"))
  expect_is(slist8.1, "data.frame")
  expect_gt(nrow(slist8.1), nrow(slist3))
  expect_gt(nrow(slist8.1), nrow(slist7.1))
  expect_gt(nrow(slist8.1), nrow(slist8.2))
  expect_gt(nrow(slist8.3), nrow(slist8.2))
  
  # can also obtain all studies by leaving off all filters
  slist9 <- get_studies()
  expect_is(slist9, "data.frame")
  expect_gt(nrow(slist9), 0)
  expect_gt(nrow(slist9), 20 * nrow(slist3))

  # test support for legacy method
  expect_warning(ll <- pk_get_study_list(taxon = "Siluridae", entity = "fin"))
  expect_identical(ll, get_studies(taxon = "Siluridae", entity = "fin"))
  expect_warning(ll <- pk_get_study_list(taxon = "Siluridae", entity = "fin", includeRels = FALSE))
  expect_identical(ll, get_studies(taxon = "Siluridae", entity = "fin", includeRels = FALSE))
  expect_warning(ll <- pk_get_study_list(taxon = "Siluridae", entity = "fin", quality = "size"))
  expect_identical(ll, get_studies(taxon = "Siluridae", entity = "fin", quality = "size"))
  expect_warning(ll <- pk_get_study_list(entity = "pelvic fin", includeRels = c("serial","historical")))
  expect_identical(ll, get_studies(entity = "pelvic fin", includeRels = c("serial","historical")))
  # different treatment of empty result sets:
  expect_warning(ll <- pk_get_study_list(taxon = "Grus grus"))
  expect_is(ll, "logical")
  ll <- get_studies(taxon = "Grus grus")
  expect_is(ll, "list")
  expect_length(ll, 0)


  s1 <- pk_get_study_xml(slist1[1,"id"])
  expect_is(s1[[1]], 'nexml')
  
  ss1 <- pk_get_study(s1)
  expect_is(ss1[[1]], 'data.frame')
  
  sss1 <- pk_get_study_meta(s1)
  expect_is(sss1[[1]], 'list')
  expect_is(sss1[[1]]$id_taxa, 'data.frame')
  
})
