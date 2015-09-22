context("rphenoscape test")

test_that("Test term search", {
  a <- pk_taxon_detail("coral")
  b <- pk_phenotype_detail("shape")
  c <- pk_anatomical_detail("fin")

  aa <- pk_taxon_detail("coral tt")
  bb <- pk_phenotype_detail("shape tt")
  cc <- pk_anatomical_detail("fin tt")

  expect_output(str(a), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
  expect_output(str(b), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
  expect_output(str(c), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")

  expect_equal(aa, FALSE)
  expect_equal(bb, FALSE)
  expect_equal(cc, FALSE)
})

test_that("Test retriving IRI", {
  i <- pk_get_iri("Coralliozetus", "vto")
  ii <- pk_get_iri("Coralliozetus TT", "vto")
  iii <- pk_get_iri("Coralliozetus", "pato")

  expect_equal(i, "http://purl.obolibrary.org/obo/VTO_0042955")
  expect_equal(ii, FALSE)
  expect_equal(iii, FALSE)
})


test_that("Test getting classification information", {
  t <- pk_taxon_class("Fisherichthys")
  tt <- pk_taxon_class("Fisherichthys folmeri")
  ttt <- pk_taxon_class("Fisherichthys TT")

  a <- pk_anatomical_class("fin")
  aa <- pk_anatomical_class("fin FF")

  p <- pk_phenotype_class("shape")
  pp <- pk_phenotype_class("shape SS")

  expect_output(str(t), 'List of 5')
  expect_output(str(tt), 'List of 5')
  expect_equal(ttt, FALSE)

  expect_output(str(a), 'List of 5')
  expect_equal(aa, FALSE)

  expect_output(str(p), 'List of 5')
  expect_equal(pp, FALSE)

})

test_that("Test OnTrace", {
  m <- pk_ontotrace("Ictalurus", "fin")

  expect_equal(m, TRUE)
})
