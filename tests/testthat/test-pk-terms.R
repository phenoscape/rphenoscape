context("rphenoscape test")

test_that("Test term search", {
  a <- pk_taxa_detail("coral")
  b <- pk_phenotype_detail("shape")
  c <- pk_anatomical_detail("fin")

  expect_output(str(a), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
  expect_output(str(b), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
  expect_output(str(c), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
})

test_that("Test retriving IRI", {
  i <- pk_get_iri("Coralliozetus", "vto")
  ii <- pk_get_iri("Coralliozetus TT", "vto")
  iii <- pk_get_iri("Coralliozetus", "pato")
  expect_output(str(i), 'data.frame')
  expect_equal(i[1, '@id'], "http://purl.obolibrary.org/obo/VTO_0042955")
  expect_output(str(ii), 'list')
  expect_error(ii[1, '@id'])
  expect_output(str(iii), 'list')
  expect_error(iii[1, '@id'])

})
