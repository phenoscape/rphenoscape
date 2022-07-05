context("finding terms")

test_that("Test that anatomy_ontology_iris doesn't return PATO or ZP", {
  skip_on_cran()
  iris <- anatomy_ontology_iris()
  expect_false("http://purl.obolibrary.org/obo/pato.owl" %in% iris)
  expect_false("http://purl.obolibrary.org/obo/zp.owl" %in% iris)
})