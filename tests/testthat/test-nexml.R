context("changing nexml object content")

test_that("dropping OTUs works", {
  nex <- nexml_read(system.file("examples", "ontotrace-result.xml", package = "rphenoscape"))

  # giving a block that doesn't exist generates a warning
  testthat::expect_warning(nexml_drop_otu(nex, filter = FALSE, block = 2))

  # dropping none should mean no change
  nex2 <- nexml_drop_otu(nex, filter = FALSE)
  testthat::expect_length(nex2@otus[[1]]@otu, 9)
  testthat::expect_equal(sapply(nex2@otus[[1]]@otu, slot, name = "id"),
                         sapply(nex@otus[[1]]@otu, slot, name = "id"))

  # dropping using a filter function
  nmatch <- sum(grepl(" sp.", sapply(nex@otus[[1]]@otu, slot, name = "label")))
  testthat::expect_gt(nmatch, 0)
  testthat::expect_lt(nmatch, length(nex@otus[[1]]@otu))

  nex_sub <- nexml_drop_otu(nex, filter = function(x) grepl(" sp.", x), at = "label")
  testthat::expect_length(nex_sub@otus[[1]]@otu, length(nex@otus[[1]]@otu) - nmatch)
  testthat::expect_length(nex_sub@characters[[1]]@matrix@row,
                          length(nex@characters[[1]]@matrix@row) - nmatch)
  mat_sub <- get_characters(nex_sub)
  testthat::expect_equal(dim(mat_sub), dim(get_characters(nex)) - c(1, 0))
  meta_sub <- pk_get_ontotrace_meta(nex_sub)
  meta_nex <- pk_get_ontotrace_meta(nex)
  testthat::expect_equal(dim(meta_sub$id_taxa), dim(meta_nex$id_taxa) - c(1, 0))
  testthat::expect_equal(dim(meta_sub$id_entities), dim(meta_nex$id_entities))

  # dropping using a logical vector
  toDrop <- grepl(" sp.", sapply(nex@otus[[1]]@otu, slot, name = "label"))
  nex_sub2 <- nexml_drop_otu(nex, filter = toDrop)
  testthat::expect_equal(sapply(nex_sub2@otus[[1]]@otu, slot, name = "id"),
                         sapply(nex_sub@otus[[1]]@otu, slot, name = "id"))
  testthat::expect_equal(get_characters(nex_sub2), mat_sub)
  testthat::expect_equal(pk_get_ontotrace_meta(nex_sub2), meta_sub)
})

test_that("dropping characters works", {
  nex <- nexml_read(system.file("examples", "ontotrace-result.xml", package = "rphenoscape"))
  
  # giving a block that doesn't exist generates a warning
  testthat::expect_warning(nexml_drop_char(nex, filter = FALSE, block = 2))
  
  # dropping none should mean no change
  nex2 <- nexml_drop_char(nex, filter = FALSE)
  testthat::expect_length(nex2@characters[[1]]@format@char, 3)
  testthat::expect_equal(sapply(nex2@characters[[1]]@format@char, slot, name = "id"),
                         sapply(nex@characters[[1]]@format@char, slot, name = "id"))
  
  # dropping using a filter function
  nmatch <- sum(grepl("pelvic",
                      sapply(nex@characters[[1]]@format@char, slot, name = "label")))
  testthat::expect_gt(nmatch, 0)
  testthat::expect_lt(nmatch, length(nex@characters[[1]]@format@char))
  
  nex_sub <- nexml_drop_char(nex,
                             filter = function(x) grepl("pelvic", x), at = "label")
  testthat::expect_length(nex_sub@characters[[1]]@format@char,
                          length(nex@characters[[1]]@format@char) - nmatch)
  testthat::expect_length(nex_sub@characters[[1]]@format@states,
                          length(nex@characters[[1]]@format@states) - nmatch)
  testthat::expect_length(nex_sub@characters[[1]]@matrix@row,
                          length(nex@characters[[1]]@matrix@row) - nmatch)
  mat_sub <- get_characters(nex_sub)
  testthat::expect_equal(dim(mat_sub), dim(get_characters(nex)) - c(1, 1))
  # states still match, and thus symbols aren't all NA
  testthat::expect_false(any(apply(mat_sub, 2, function(x) all(is.na(x)))))
  meta_sub <- pk_get_ontotrace_meta(nex_sub)
  meta_nex <- pk_get_ontotrace_meta(nex)
  testthat::expect_equal(dim(meta_sub$id_taxa), dim(meta_nex$id_taxa))
  testthat::expect_equal(dim(meta_sub$id_entities), dim(meta_nex$id_entities) - c(1,0))

  # dropping using a logical vector
  toDrop <- grepl("pelvic", sapply(nex@characters[[1]]@format@char, slot, name = "label"))
  nex_sub2 <- nexml_drop_char(nex, filter = toDrop)
  testthat::expect_equal(sapply(nex_sub2@characters[[1]]@format@char, slot, name = "id"),
                         sapply(nex_sub@characters[[1]]@format@char, slot, name = "id"))
  testthat::expect_equal(get_characters(nex_sub2), mat_sub)
  testthat::expect_equal(pk_get_ontotrace_meta(nex_sub2), meta_sub)
})

test_that("dropping unused otus works", {
  nex <- nexml_read(system.file("examples", "ontotrace-result.xml", package = "rphenoscape"))

  nex_sub <- nexml_drop_char(nex,
                             filter = function(x) grepl("pelvic", x), at = "label")

  # filter should also work standalone
  isUnused <- is_unused_otu(nex_sub@otus[[1]]@otu, nexml = nex_sub)
  testthat::expect_equal(sum(isUnused), 1)

  # drop unused otus:
  nex_sub2 <- nexml_drop_otu(nex_sub, filter = is_unused_otu)
  testthat::expect_length(nex_sub2@otus[[1]]@otu,
                          length(nex@otus[[1]]@otu) - sum(isUnused))
  testthat::expect_length(nex_sub2@otus[[1]]@otu,
                          length(nex_sub@otus[[1]]@otu) - sum(isUnused))
  testthat::expect_length(nex_sub2@otus[[1]]@otu,
                          length(nex_sub2@characters[[1]]@matrix@row))
  mat_sub <- get_characters(nex_sub2)
  testthat::expect_equal(dim(mat_sub), dim(get_characters(nex_sub)))
  meta_sub <- pk_get_ontotrace_meta(nex_sub2)
  testthat::expect_equal(nrow(meta_sub$id_taxa), nrow(mat_sub))
  testthat::expect_equal(nrow(meta_sub$id_entities), ncol(mat_sub))
})

test_that("dropping unused characters works", {
  nex <- nexml_read(system.file("examples", "ontotrace-result.xml", package = "rphenoscape"))
  
  nex_sub <- nexml_drop_otu(nex,
                            filter = function(x) !grepl(" sp.", x), at = "label")
  
  # filter should also work standalone
  isUnused <- is_unused_char(nex_sub@characters[[1]]@format@char, nexml = nex_sub)
  testthat::expect_equal(sum(isUnused), 2)
  
  # drop unused chars:
  nex_sub2 <- nexml_drop_char(nex_sub, filter = is_unused_char)
  testthat::expect_length(nex_sub2@characters[[1]]@format@char,
                          length(nex@characters[[1]]@format@char) - sum(isUnused))
  testthat::expect_length(nex_sub2@characters[[1]]@format@char,
                          length(nex_sub@characters[[1]]@format@char) - sum(isUnused))
  testthat::expect_length(nex_sub2@characters[[1]]@matrix@row,
                          length(nex_sub@characters[[1]]@matrix@row))
  mat_sub <- get_characters(nex_sub2)
  testthat::expect_equal(dim(mat_sub), dim(get_characters(nex_sub)))
  meta_sub <- pk_get_ontotrace_meta(nex_sub2)
  testthat::expect_equal(nrow(meta_sub$id_taxa), nrow(mat_sub))
  testthat::expect_equal(nrow(meta_sub$id_entities), ncol(mat_sub))
})

test_that("adding provenance records works", {
  nex <- nexml_read(system.file("examples", "ontotrace-result.xml", package = "rphenoscape"))

  # no provenance record if nothing was changed
  nex2 <- nexml_drop_otu(nex, filter = FALSE)
  testthat::expect_equal(get_metadata_values(nex2, props = "dc:description"),
                         get_metadata_values(nex, props = "dc:description"))
  testthat::expect_length(RNeXML::get_meta(nex2, props = "dcterms:provenance"), 0)
  testthat::expect_length(RNeXML::get_meta(nex2, props = "prov:wasGeneratedBy"), 0)

  nex2 <- nexml_drop_char(nex, filter = FALSE)
  testthat::expect_equal(get_metadata_values(nex2, props = "dc:description"),
                         get_metadata_values(nex, props = "dc:description"))
  testthat::expect_length(RNeXML::get_meta(nex2, props = "dcterms:provenance"), 0)
  testthat::expect_length(RNeXML::get_meta(nex2, props = "prov:wasGeneratedBy"), 0)

  # dropping otus should generate a provenance record
  nex2 <- nexml_drop_otu(nex, filter = function(x) grepl(" sp.", x), at = "label")
  testthat::expect_length(RNeXML::get_meta(nex2, props = "dcterms:provenance"), 2)
  testthat::expect_length(RNeXML::get_meta(nex2, props = "prov:wasGeneratedBy"), 1)
  testthat::expect_match(get_metadata_values(nex2, props = "dc:description"),
                         "^Original", all = FALSE)
  testthat::expect_match(get_metadata_values(nex2, props = "dc:description"),
                         "^Modified", all = FALSE)
  testthat::expect_match(get_metadata_values(nex2, props = "dc:description"),
                         "function\\(x\\) grepl", all = FALSE)
  testthat::expect_match(get_metadata_values(nex2, props = "prov:value"),
                         "function\\(x\\) grepl")

  # dropping chars should also add a provenance record
  nex2 <- nexml_drop_char(nex, filter = function(x) grepl("pelvic", x), at = "label")
  testthat::expect_length(RNeXML::get_meta(nex2, props = "dcterms:provenance"), 2)
  testthat::expect_length(RNeXML::get_meta(nex2, props = "prov:wasGeneratedBy"), 1)
  testthat::expect_match(get_metadata_values(nex2, props = "dc:description"),
                         "^Original", all = FALSE)
  testthat::expect_match(get_metadata_values(nex2, props = "dc:description"),
                         "^Modified", all = FALSE)
  testthat::expect_match(get_metadata_values(nex2, props = "dc:description"),
                         "function\\(x\\) grepl", all = FALSE)
  testthat::expect_match(get_metadata_values(nex2, props = "prov:value"),
                         "function\\(x\\) grepl")

  # dropping char then otu should add two provenance records
  nex2 <- 
    nexml_drop_char(nex, filter = function(x) grepl("pelvic", x), at = "label") %>%
    nexml_drop_otu(filter = is_unused_otu)
  testthat::expect_length(RNeXML::get_meta(nex2, props = "dcterms:provenance"), 3)
  testthat::expect_length(RNeXML::get_meta(nex2, props = "prov:wasGeneratedBy"), 2)
  testthat::expect_match(get_metadata_values(nex2, props = "prov:value"),
                         "filter = ")
})