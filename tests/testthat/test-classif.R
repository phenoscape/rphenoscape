context("classification, ancestors, descendants")

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

test_that("descendants/ancestors", {
  skip_on_cran()
  
  # taxon terms:
  expect_equal(pk_is_descendant("Halecostomi", c("Halecostomi", "Icteria", "Sciaenidae")),
               c(FALSE, FALSE, TRUE))
  expect_equal(pk_is_ancestor("Sciaenidae", c("Halecostomi", "Abeomelomys", "Sciaenidae")),
               c(TRUE, FALSE, FALSE))
  
  # anatomical entities:
  expect_equal(pk_is_descendant("paired fin", c("pectoral fin", "pelvic fin", "dorsal fin")),
               c(TRUE, TRUE, FALSE))
  expect_equal(pk_is_descendant("paired fin", c("pelvic fin", "pelvic fin ray")),
               c(TRUE, FALSE))
  expect_equal(pk_is_descendant("paired fin", c("pelvic fin", "pelvic fin ray"),
                                includeRels = "part_of"),
               c(TRUE, TRUE))
  expect_equal(pk_is_ancestor("pelvic fin", c("paired fin", "hindlimb", "fin")),
               c(TRUE, FALSE, TRUE))
  expect_equal(pk_is_ancestor("pelvic fin ray", c("paired fin", "fin")),
               c(FALSE, FALSE))
  expect_equal(pk_is_ancestor("pelvic fin ray", c("paired fin", "fin"),
                              includeRels = "part_of"),
               c(TRUE, TRUE))

  # phenotypic quality
  expect_equal(pk_is_ancestor("triangular", c("shape", "color", "amount")),
               c(TRUE, FALSE, FALSE))
  expect_equal(pk_is_descendant("shape", c("T-shaped", "star shaped", "yellow")),
               c(TRUE, TRUE, FALSE))
})
