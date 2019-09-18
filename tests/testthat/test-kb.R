context("Phenoscape KB metadata etc")

test_that("KB annotation summary", {
  kbmeta <- get_KBinfo()

  testthat::expect_is(kbmeta, "list")
  testthat::expect_s3_class(kbmeta, "KBinfo")
  testthat::expect_gte(length(names(kbmeta)), 5)
  testthat::expect_true(any(sapply(kbmeta, is.integer)))
  testthat::expect_false(all(sapply(kbmeta, is.integer)))
  testthat::expect_true(any(sapply(kbmeta,
                                   function(x) any(grepl("POSIX[c,l]t", class(x))))))
  testthat::expect_output(print(kbmeta), "Build time")
  testthat::expect_output(print(kbmeta, tz = "UTC"), " UTC")
})