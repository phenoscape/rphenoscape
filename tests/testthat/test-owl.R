context("Manchester OWL functions")

test_that("as.owl handles one or a vector of owl expressions", {
  owlstr <- "<http://purl.obolibrary.org/obo/BFO_0000050> some <http://purl.obolibrary.org/obo/UBERON_0000033>"
  testthat::expect_false(is.owl(owlstr))
  testthat::expect_false(is.manchester_owl(owlstr))
  testthat::expect_true(is.character(owlstr))

  # create one owl object
  obj <- as.owl(owlstr)
  testthat::expect_is(obj, "owl")
  testthat::expect_true(is.owl(obj))
  testthat::expect_true(is.manchester_owl(obj))
  testthat::expect_true(is.character(obj))

  # simply returns objects that are already class owl
  obj2 <- as.owl(obj)
  testthat::expect_equal(obj2, obj)

  # can run vectorized
  objs <- as.owl(c(
    "<http://purl.obolibrary.org/obo/BFO_0000050> some <http://purl.obolibrary.org/obo/UBERON_0000033>",
    "<http://purl.obolibrary.org/obo/VTO_0034991> or <http://purl.obolibrary.org/obo/VTO_0037519>"
  ))
  testthat::expect_is(objs, "list")
  testthat::expect_equal(length(objs), 2)
  testthat::expect_true(all(sapply(objs, is.owl)))
})

test_that("as.owl processes label-based class expression when usesLabels is TRUE", {
  skip_on_cran()
  result = as.owl("'part of' some head", usesLabels = TRUE)
  testthat::expect_match(result, "<http.*> some <http.*>")
  testthat::expect_true(is.owl(result))
  testthat::expect_true(is.manchester_owl(result))
  testthat::expect_true(is.character(result))
})

test_that("as.owl raises error on unknown labels", {
  skip_on_cran()
  testthat::expect_error(as.owl("'part of' some INVALID", usesLabels = TRUE))
})