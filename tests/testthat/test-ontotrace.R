context("Ontotrace API")

test_that("Ontotrace basics", {
  skip_on_cran()
  single_nex <- get_ontotrace_data(taxon = "Ictalurus", entity = "fin")
  multi_nex <- get_ontotrace_data(taxon = c("Ictalurus", "Ameiurus"), entity = c("fin spine", "pelvic splint"))
  
  testthat::expect_s4_class(single_nex, 'nexml')
  testthat::expect_s4_class(multi_nex, 'nexml')
  
  testthat::expect_error(
    get_ontotrace_data(taxon = "Ictalurus TT", entity = "fin",
                       relation = "other relation"))
  testthat::expect_warning(
    nx <- get_ontotrace_data(taxon = c("Ictalurus", "Ameiurus XXX"),
                             entity = c("fin", "spine"),
                             strict = FALSE))
  sumnx <- RNeXML::summary(nx)
  testthat::expect_equivalent(sumnx$nblocks, c(1, 1, 1))
  testthat::expect_gt(sumnx$notus, 1)
  testthat::expect_gt(sumnx$ncharacters, 1)
  
  single_mat <- get_char_matrix(single_nex)
  multi_mat <- get_char_matrix(multi_nex)
  
  testthat::expect_is(single_mat, 'data.frame')
  testthat::expect_is(multi_mat, 'data.frame')
  
  single_met <- get_char_matrix_meta(single_nex)
  
  testthat::expect_is(single_met, 'list')
  
})

test_that("relationship expressions", {
  skip_on_cran()
  nex1 <- get_ontotrace_data(taxon = "Ictalurus",
                             entity = "fin",
                             variable_only = FALSE)
  nex2 <- get_ontotrace_data(taxon = "Ictalurus",
                             entity = "fin",
                             relation = "part of",
                             variable_only = FALSE)
  nex3 <- get_ontotrace_data(taxon = "Ictalurus",
                             entity = "fin",
                             relation = NA,
                             variable_only = FALSE)
  nex1_m <- RNeXML::get_characters(nex1)
  nex2_m <- RNeXML::get_characters(nex2)
  nex3_m <- RNeXML::get_characters(nex3)
  
  testthat::expect_identical(dim(nex1_m), dim(nex2_m))
  testthat::expect_identical(nrow(nex1_m), nrow(nex3_m))
  testthat::expect_lt(ncol(nex3_m), ncol(nex2_m))
  
  nex4 <- get_ontotrace_data(taxon = "Ictalurus",
                             entity = "paired fin bud",
                             relation = "develops from",
                             variable_only = FALSE)
  nex4_m <- RNeXML::get_characters(nex4)
  
  testthat::expect_identical(nrow(nex1_m), nrow(nex4_m))
  testthat::expect_lt(ncol(nex4_m), ncol(nex2_m))
  testthat::expect_lt(ncol(nex3_m), ncol(nex4_m))
  
})

test_that("Deprecated ontotrace function", {
  skip_on_cran()
  expect_warning(single_nex <- pk_get_ontotrace_xml(taxon = "Ictalurus", entity = "fin"))
  expect_warning(multi_nex <- pk_get_ontotrace_xml(taxon = c("Ictalurus", "Ameiurus"), entity = c("fin spine", "pelvic splint")))
  testthat::expect_s4_class(single_nex, 'nexml')
  testthat::expect_s4_class(multi_nex, 'nexml')
})

test_that("Ontotrace with OWL expressions", {
  skip_on_cran()

  # using label resolve
  taxon <- as.owl("Ictalurus", usesLabels = TRUE)
  entity <- as.owl("'paired fin bud' or ('develops from' some 'paired fin bud')", usesLabels = TRUE)
  nex1 <- get_ontotrace_data(taxon, entity)
  testthat::expect_gte(length(nex1@otus[[1]]@otu), 9)

  # using hard coded owl expresssions
  taxon <- as.owl("<http://purl.obolibrary.org/obo/VTO_0036217>")
  entity <- as.owl("<http://purl.obolibrary.org/obo/UBERON_0002531> 
                   or (<http://purl.obolibrary.org/obo/RO_0002202> some <http://purl.obolibrary.org/obo/UBERON_0002531>)")
  nex1 <- get_ontotrace_data(taxon, entity)
  testthat::expect_gte(length(nex1@otus[[1]]@otu), 9)
})

test_that("Ontotrace without subsumption", {
  skip_on_cran()
  nex1 <- get_ontotrace_data(
    taxon = c("http://purl.obolibrary.org/obo/VTO_0061495", # Ictalurus australis
              "http://purl.obolibrary.org/obo/VTO_0036223"), # Ictalurus furcatus
    entity = c("http://purl.obolibrary.org/obo/UBERON_2002001"), # anterior dentation of pectoral fin spine
    subsume = FALSE)
  testthat::expect_lte(length(nex1@otus[[1]]@otu), 2)
  testthat::expect_lte(length(nex1@characters[[1]]@format@char), 1)
})

test_that("Deprecated ontotrace matrix functions", {
  skip_on_cran()
  single_nex <- get_ontotrace_data(taxon = "Ictalurus", entity = "fin")

  char_mat <- get_char_matrix(single_nex)
  expect_warning(dep_char_mat <- pk_get_ontotrace(single_nex))
  testthat::expect_equal(char_mat, dep_char_mat)

  char_mat_meta <- get_char_matrix_meta(single_nex)
  expect_warning(dep_char_mat_meta <- pk_get_ontotrace_meta(single_nex))
  testthat::expect_equal(char_mat_meta, dep_char_mat_meta)
})
