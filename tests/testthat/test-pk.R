context("rphenoscape test")

test_that("Test term search", {
  a <- pk_taxon_detail("Coralliozetus")
  b <- pk_phenotype_detail("shape")
  c <- pk_anatomical_detail("basihyal bone")

  aa <- pk_taxon_detail("coral tt")
  bb <- pk_phenotype_detail("shape tt")
  cc <- pk_anatomical_detail("fin tt")


  g <- pk_gene_detail("socs5")
  gg <- pk_gene_detail("socs5", "Danio rerio")

  expect_output(str(a), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
  expect_output(str(b), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
  expect_output(str(c), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")

  expect_equal(aa, FALSE)
  expect_equal(bb, FALSE)
  expect_equal(cc, FALSE)

  expect_output(str(g), "data.frame")
  expect_output(str(gg), "data.frame")
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

test_that("Test Descendant/Ancestor", {
  fl <- pk_is_descendant("Halecostomi", c("Halecostomi", "Icteria", "Sciaenidae"))
  tl <- pk_is_ancestor("Sciaenidae", c("Halecostomi", "Abeomelomys", "Sciaenidae"))

  expect_equal(fl, c(F, F, T))
  expect_equal(tl, c(T, F, F))
})

test_that("Test OnToTrace", {
  single <- pk_ontotrace(taxon = "Ictalurus", entity = "fin")
  multi <- pk_ontotrace(taxon = c("Ictalurus", "Ameiurus"), entity = c("fin spine", "pelvic splint"))
#   rel <- pk_ontotrace(taxon = c("Ictalurus", "Ameiurus"),
#                       entity = c("fin spine", "pelvic splint"),
#                       relation = "develops from")

  single1 <- pk_ontotrace(taxon = "Ictalurus", entity = "fin", get_metadata = TRUE)

  err1 <- function() pk_ontotrace(taxon = "Ictalurus TT", entity = "fin", relation = "other relation")
  err2 <- function() pk_ontotrace(taxon = c("Ictalurus", "Ameiurus XXX"), entity = c("fin", "spine"))
  err3 <- function() pk_ontotrace("Ictalurus TT", "fin")


  expect_output(str(single), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
  expect_output(str(multi), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
#  expect_output(str(rel), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")

  expect_output(str(single1), "List of 3")

# should the value be numeric?
#   expect_equal(all(apply(single[,-1], 2, is.numeric)), TRUE)
#   expect_equal(all(apply(multi[,-1], 2, is.numeric)), TRUE)
#   expect_equal(all(apply(rel[,-1], 2, is.numeric)), TRUE)

  expect_error(err1())
  expect_error(err2())
  expect_error(err3())
})


test_that("Test getting study information", {
  s <- pk_search_studies("Ictalurus", "fin")
  ss <- pk_search_studies("Ictalurus FF", "fin")
  sss <- pk_search_studies("coral", "fin")

  expect_output(str(s), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
  expect_equal(ss, FALSE)
  expect_equal(sss, FALSE)

})



#
#
# test_that("Test OnTrace, nexml_read", {
#   rns <- test_read_ns()
#   expect_output(str(rns), "data.frame")
#
# })
#
# test_that("Test OnTrace, nexml_validate", {
#   vns <- test_validate_ns()
#   expect_equal(vns, TRUE)
# })


