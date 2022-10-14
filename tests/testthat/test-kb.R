context("Phenoscape KB metadata etc")

test_that("custom user agent is set", {
  reflect_api <- "http://httpbin.org/user-agent"
  ua_resp <- get_json_data(reflect_api, {})$`user-agent`
  testthat::expect_match(ua_resp, "r-curl/[0-9.]+")
  testthat::expect_match(ua_resp, "httr/[0-9.]+")
  testthat::expect_match(ua_resp, paste0(utils::packageName(), "/[0-9.]+"))
  testthat::expect_message(ua_resp2 <- get_csv_data("http://httpbin.org/user-agent", {}))
  testthat::expect_equivalent(ua_resp, sub("\\s+user-agent:\\s+", "", ua_resp2[1,1]))
  # unfortunately, we can't test the get_nexml_data() method in the same way
})

test_that("clean_jsonld_names changes '@id' names to 'id'", {
  list_with_id <- list(`@id`=1, b=2)
  testthat::expect_equal(clean_jsonld_names(list_with_id), list(id=1, b=2))
  df_with_id <- data.frame(X=c(1,2))
  names(df_with_id) <- c('@id')
  testthat::expect_equal(clean_jsonld_names(df_with_id), data.frame(id=c(1,2)))
})

test_that("rclean_jsonld_names recursively changes '@id' names to 'id'", {
  nested_list <- list(`@id`=1, b=list(`@id`=2))
  testthat::expect_equal(rclean_jsonld_names(nested_list), list(id=1, b=list(id=2)))
  df_with_id <- data.frame(X=c(1,2))
  names(df_with_id) <- c('@id')
  list_of_df <- list(`@id`=1, b=df_with_id)
  testthat::expect_equal(rclean_jsonld_names(list_of_df), list(id=1, b=data.frame(id=c(1,2))))
})

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