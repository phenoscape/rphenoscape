context("Ontotrace API")

test_that("Ontotrace basics", {
  skip_on_cran()
  single_nex <- pk_get_ontotrace_xml(taxon = "Ictalurus", entity = "fin")
  multi_nex <- pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus"), entity = c("fin spine", "pelvic splint"))
  
  testthat::expect_s4_class(single_nex, 'nexml')
  testthat::expect_s4_class(multi_nex, 'nexml')
  
  testthat::expect_error(
    pk_get_ontotrace_xml(taxon = "Ictalurus TT", entity = "fin",
                         relation = "other relation"))
  testthat::expect_warning(
    nx <- pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus XXX"),
                               entity = c("fin", "spine"),
                               strict = FALSE))
  sumnx <- RNeXML::summary(nx)
  testthat::expect_equivalent(sumnx$nblocks, c(1, 1, 1))
  testthat::expect_gt(sumnx$notus, 1)
  testthat::expect_gt(sumnx$ncharacters, 1)
  
  single_mat <- pk_get_ontotrace(single_nex)
  multi_mat <- pk_get_ontotrace(multi_nex)
  
  testthat::expect_is(single_mat, 'data.frame')
  testthat::expect_is(multi_mat, 'data.frame')
  
  single_met <- pk_get_ontotrace_meta(single_nex)
  
  testthat::expect_is(single_met, 'list')
  
})

test_that("relationship expressions", {
  skip_on_cran()
  nex1 <- pk_get_ontotrace_xml(taxon = "Ictalurus",
                               entity = "fin",
                               variable_only = FALSE)
  nex2 <- pk_get_ontotrace_xml(taxon = "Ictalurus",
                               entity = "fin",
                               relation = "part of",
                               variable_only = FALSE)
  nex3 <- pk_get_ontotrace_xml(taxon = "Ictalurus",
                               entity = "fin",
                               relation = NA,
                               variable_only = FALSE)
  nex1_m <- RNeXML::get_characters(nex1)
  nex2_m <- RNeXML::get_characters(nex2)
  nex3_m <- RNeXML::get_characters(nex3)
  
  testthat::expect_identical(dim(nex1_m), dim(nex2_m))
  testthat::expect_identical(nrow(nex1_m), nrow(nex3_m))
  testthat::expect_lt(ncol(nex3_m), ncol(nex2_m))
  
  nex4 <- pk_get_ontotrace_xml(taxon = "Ictalurus",
                               entity = "paired fin bud",
                               relation = "develops from",
                               variable_only = FALSE)
  nex4_m <- RNeXML::get_characters(nex4)
  
  testthat::expect_identical(nrow(nex1_m), nrow(nex4_m))
  testthat::expect_lt(ncol(nex4_m), ncol(nex2_m))
  testthat::expect_lt(ncol(nex3_m), ncol(nex4_m))
  
})
