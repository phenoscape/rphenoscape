context("classification, ancestors, descendants")

test_that("Test getting classification information", {
  skip_on_cran()
  t <- term_classification("Fisherichthys", as="taxon")
  tt <- term_classification("Fisherichthys folmeri", as="taxon")

  a <- term_classification("fin", as="anatomy")
  p <- term_classification("shape", as="pato")

  expect_true("id" %in% names(t))
  expect_is(t, "list")
  expect_length(t, 5)
  expect_is(tt, "list")
  expect_length(tt, 5)
  expect_warning(ttt <- term_classification("Fisherichthys TT", as="taxon"))
  expect_true(is.na(ttt))

  expect_true("id" %in% names(a))
  expect_is(a, "list")
  expect_length(a, 5)
  expect_warning(aa <- term_classification("fin FF", as="anatomy"))
  expect_true(is.na(aa))

  expect_true("id" %in% names(p))
  expect_is(p, "list")
  expect_length(p, 5)

  expect_warning(pp <- term_classification("shape SS", as="pato"))
  expect_true(is.na(pp))

  # test support for legacy methods
  expect_warning(t <- pk_taxon_class("Fisherichthys"))
  expect_true("@id" %in% names(t))
  expect_is(t, "list")
  expect_length(t, 5)
  expect_warning(a <- pk_anatomical_class("fin"))
  expect_true("@id" %in% names(a))
  expect_is(a, "list")
  expect_length(a, 5)
  expect_warning(p <- pk_phenotype_class("shape"))
  expect_true("@id" %in% names(p))
  expect_is(p, "list")
  expect_length(p, 5)
})

test_that("descendants/ancestors", {
  skip_on_cran()
  
  # taxon terms:
  expect_equal(is_descendant("Halecostomi", c("Halecostomi", "Icteria", "Sciaenidae")),
               c(FALSE, FALSE, TRUE))
  expect_equal(is_ancestor("Sciaenidae", c("Halecostomi", "Abeomelomys", "Sciaenidae")),
               c(TRUE, FALSE, FALSE))
  
  # anatomical entities:
  expect_equal(is_descendant("paired fin", c("pectoral fin", "pelvic fin", "dorsal fin")),
               c(TRUE, TRUE, FALSE))
  expect_equal(is_descendant("paired fin", c("pelvic fin", "pelvic fin ray")),
               c(TRUE, FALSE))
  expect_equal(is_descendant("paired fin", c("pelvic fin", "pelvic fin ray"),
                                includeRels = "part_of"),
               c(TRUE, TRUE))
  expect_equal(is_ancestor("pelvic fin", c("paired fin", "hindlimb", "fin")),
               c(TRUE, FALSE, TRUE))
  expect_equal(is_ancestor("pelvic fin ray", c("paired fin", "fin")),
               c(FALSE, FALSE))
  expect_equal(is_ancestor("pelvic fin ray", c("paired fin", "fin"),
                              includeRels = "part_of"),
               c(TRUE, TRUE))

  # phenotypic quality
  expect_equal(is_ancestor("triangular", c("shape", "color", "amount")),
               c(TRUE, FALSE, FALSE))
  expect_equal(is_descendant("shape", c("T-shaped", "star shaped", "yellow")),
               c(TRUE, TRUE, FALSE))
})

test_that("legacy descendants/ancestors", {
  skip_on_cran()

  # taxon terms:
  result <- expect_warning(pk_is_descendant("Halecostomi", c("Halecostomi", "Icteria", "Sciaenidae")))
  expect_equal(result, c(FALSE, FALSE, TRUE))
  result <- expect_warning(pk_is_ancestor("Sciaenidae", c("Halecostomi", "Abeomelomys", "Sciaenidae")))
  expect_equal(result, c(TRUE, FALSE, FALSE))

  # anatomical entities:
  result <- expect_warning(pk_is_descendant("paired fin", c("pectoral fin", "pelvic fin", "dorsal fin")))
  expect_equal(result, c(TRUE, TRUE, FALSE))
  result <- expect_warning(pk_is_ancestor("pelvic fin", c("paired fin", "hindlimb", "fin")))
  expect_equal(result, c(TRUE, FALSE, TRUE))

  # phenotypic quality
  result <- expect_warning(pk_is_ancestor("triangular", c("shape", "color", "amount")))
  expect_equal(result, c(TRUE, FALSE, FALSE))
  result <- expect_warning(pk_is_descendant("shape", c("T-shaped", "star shaped", "yellow")))
  expect_equal(result, c(TRUE, TRUE, FALSE))
})
