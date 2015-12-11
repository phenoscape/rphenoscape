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
  single_nex <- pk_get_ontotrace_xml(taxon = "Ictalurus", entity = "fin")
  multi_nex <- pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus"), entity = c("fin spine", "pelvic splint"))

  expect_output(class(single_nex), 'nexml')
  expect_output(class(multi_nex), 'nexml')

  err1 <- function() pk_get_ontotrace_xml(taxon = "Ictalurus TT", entity = "fin", relation = "other relation")
  err2 <- function() pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus XXX"), entity = c("fin", "spine"))
  err3 <- function() pk_get_ontotrace_xml("Ictalurus TT", "fin")

  expect_error(err1())
  expect_error(err2())
  expect_error(err3())

  single_mat <- pk_get_ontotrace(single_nex)
  multi_mat <- pk_get_ontotrace(multi_nex)

  expect_output(str(single_mat), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
  expect_output(str(multi_mat), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")

  single_met <- pk_get_ontotrace_meta(single_nex)
  expect_output(str(single_met), "List of 2")


})

#
test_that("Test getting study information", {
    slist <- pk_get_study_list(taxon = "Ameiurus", entity = "pelvic splint")
    s1 <- pk_get_study_xml('https://scholar.google.com/scholar?q=The+Phylogeny+of+Ictalurid+Catfishes%3A+A+Synthesis+of+Recent+Work&btnG=&hl=en&as_sdt=0%2C42')
    ss1 <- pk_get_study(s1)
    sss1 <- pk_get_study_meta(s1)

    expect_output(str(slist), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
    expect_output(class(s1[[1]]), 'nexml')
    expect_output(class(ss1[[1]]), 'data.frame')
    expect_output(class(sss1[[1]]), 'list')
    expect_output(str(sss1[[1]]$id_taxa), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")

})



