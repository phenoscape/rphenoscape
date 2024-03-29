context("term finding and info APIs")

test_that("Test term details", {
  skip_on_cran()
  b <- phenotypic_quality_term_info("shape")
  c <- anatomy_term_info("basihyal bone")

  g <- gene_info("socs5")
  gg <- gene_info("socs5", "Danio rerio")

  expect_is(b, 'data.frame')
  expect_equal(nrow(b), 1)
  expect_false(any(is.na(b)))
  expect_is(c, 'data.frame')
  expect_equal(nrow(c), 1)
  expect_false(any(is.na(c)))
  

  expect_warning(bb <- phenotypic_quality_term_info("shape tt"))
  expect_warning(cc <- anatomy_term_info("fin tt"))
  expect_true(is.na(bb))
  expect_true(is.na(cc))

  expect_is(g, "data.frame")
  expect_gte(length(unique(g$taxon.label)), 2)
  expect_setequal(unique(g$matchType), c("exact", "partial"))
  expect_is(gg, "data.frame")
  expect_length(unique(gg$taxon.label), 1)
  expect_length(unique(gg$label), 2)
})

test_that("Test deprecated term details", {
  skip_on_cran()
  expect_warning(b <- pk_phenotype_detail("shape"))
  expect_warning(c <- pk_anatomical_detail("basihyal bone"))

  expect_warning(g <- pk_gene_detail("socs5"))
  expect_warning(gg <- pk_gene_detail("socs5", "Danio rerio"))

  expect_is(b, 'data.frame')
  expect_equal(nrow(b), 1)
  expect_false(any(is.na(b)))
  expect_is(c, 'data.frame')
  expect_equal(nrow(c), 1)
  expect_false(any(is.na(c)))
})

test_that("taxon details works", {
  skip_on_cran()
  a <- taxon_info("Coralliozetus")

  expect_is(a, 'data.frame')
  expect_gt(nrow(a), 0)
  expect_setequal(colnames(a),
                  c("id", "label", "extinct", "rank.id", "rank.label", "common_name"))

  expect_warning(aa <- taxon_info("coral tt"))
  expect_warning(bb <- taxon_info("http://foobar.com/taxon"))
  expect_warning(cc <- taxon_info(c("coral tt", "Danio rerio")))
  expect_true(is.na(aa))
  expect_true(is.na(bb))
  expect_true(any(is.na(cc)))
  expect_false(all(is.na(cc)))
  expect_equal(nrow(cc), 2)
  expect_true(all(is.na(cc[1,])))

  dd <- taxon_info(c("Danio", "Danio rerio"))
  expect_equal(nrow(dd), 2)
  expect_true(any(is.na(dd$common_name)))
  expect_false(all(is.na(dd$common_name)))
})

test_that("taxon info can use includeRelatedSynonyms", {
  expect_warning(d <- taxon_info("Chrosomus eos"))
  expect_true(is.na(d))
  expect_warning(d2 <- taxon_info("Chrosomus eos", includeRelatedSynonyms = FALSE))
  expect_true(is.na(d2))
  expect_warning(d3 <- taxon_info("Chrosomus eos", includeRelatedSynonyms = TRUE))
  expect_equal(nrow(d3), 1)
  expect_equal(d3$id, "http://purl.obolibrary.org/obo/VTO_0040309")
})

test_that("Test deprecated taxon details", {
  skip_on_cran()
  expect_warning(a <- pk_taxon_detail("Coralliozetus"))

  expect_is(a, 'data.frame')
  expect_gt(nrow(a), 0)
  expect_setequal(colnames(a),
                  c("id", "label", "extinct", "rank.id", "rank.label", "common_name"))
})

test_that("is_extinct works", {
  skip_on_cran()

  ext <- is_extinct("Fisherichthys")
  expect_is(ext, "logical")
  expect_equal(names(ext), c("Fisherichthys"))
  # make one an unresolvable typo
  expect_warning(ext <- is_extinct(c("Fisheririchthys", "Tiktaalik")))
  expect_is(ext, "logical")
  expect_length(ext, 2)
  expect_true(any(is.na(ext)))
  expect_false(all(is.na(ext)))
  expect_equal(names(ext), c("Fisheririchthys", "Tiktaalik"))
})

test_that("Test deprecated is extinct", {
  skip_on_cran()

  expect_warning(ext <- pk_is_extinct("Fisherichthys"))
  expect_is(ext, "logical")
  expect_equal(names(ext), c("Fisherichthys"))
})

test_that("Test retrieving IRI", {
  skip_on_cran()

  i <- get_term_iri("Coralliozetus", "vto")
  expect_equal(i, "http://purl.obolibrary.org/obo/VTO_0042955")
  expect_warning(ii <- get_term_iri("Coralliozetus TT", "vto"))
  expect_warning(iii <- get_term_iri("Coralliozetus", "pato"))
  expect_true(is.na(ii))
  expect_true(is.na(iii))

  tiris <- find_term("pelvic fin", definedBy = NA, matchTypes = c("exact"))
  expect_gte(nrow(tiris), 1)
  expect_silent(tiri <- get_term_iri("pelvic fin", as = NA, exactOnly = TRUE))
  expect_equal(tiri, "http://purl.obolibrary.org/obo/UBERON_0000152")

  tiris <- find_term("part_of", definedBy = NA)
  expect_true("isDefinedBy" %in% colnames(tiris))
  expect_true(all(is.na(tiris$isDefinedBy)))

  expect_warning(tiri <- get_term_iri("anatomical structure", as = NA, exactOnly = TRUE))
  expect_is(tiri, "character")
  expect_true(startsWith(tiri, "http://purl.obolibrary.org/obo/"))
})

test_that("Test retrieving IRI using includeRelatedSynonyms", {
  expect_warning(IRI <- get_term_iri("Chrosomus eos", as="taxon"))
  expect_equal(IRI, NA)

  expect_warning(IRI <- get_term_iri("Chrosomus eos", as = "taxon", includeRelatedSynonyms = TRUE))
  expect_equal(IRI, "http://purl.obolibrary.org/obo/VTO_0040309")

  expect_warning(IRI <- get_term_iri("Chrosomus eos", as = "taxon", includeRelatedSynonyms = FALSE))
  expect_equal(IRI, NA)
})

test_that("Test finding terms without a limit", {
  # Ensure that we receive more than the default KB API limit (100)
  expect_gt(nrow(find_term("fin", limit=NA)), 100)
})

test_that("Deprecated function forr retrieving IRI", {
  skip_on_cran()

  expect_warning(tt <- pk_get_iri("Coralliozetus", "vto"))
  expect_equal(tt, "http://purl.obolibrary.org/obo/VTO_0042955")
  expect_equal(tt, get_term_iri("Coralliozetus", "vto"))

  expect_warning(tt <- pk_get_iri("pelvic fin", as = NA, exactOnly = TRUE))
  expect_equal(tt, "http://purl.obolibrary.org/obo/UBERON_0000152")
  expect_equal(tt, get_term_iri("pelvic fin", as = NA, exactOnly = TRUE))
})

test_that("Test getting labels", {
  tt <- c("http://purl.obolibrary.org/obo/UBERON_0000981",
          "http://purl.obolibrary.org/obo/UBERON_0002103",
          "http://purl.obolibrary.org/obo/UBERON_0000976",
          "http://purl.obolibrary.org/obo/UBERON_0002102")
  lbls <- get_term_label(tt)
  testthat::expect_equal(length(tt), nrow(lbls))
  testthat::expect_false(any(is.na(lbls$label)))
  testthat::expect_setequal(tt, lbls$id)

  lbls <- get_term_label(tt, preserveOrder = TRUE)
  testthat::expect_equal(tt, lbls$id)

  testthat::expect_silent(lbls <- get_term_label(c(tt, "http://foo")))
  testthat::expect_equal(sum(is.na(lbls$label)), 1)
  testthat::expect_equal(lbls$id[is.na(lbls$label)], "http://foo")

  lbls <- get_term_label(tt[1])
  testthat::expect_equal(nrow(lbls), 1)
  testthat::expect_false(is.na(lbls$label))

  lbls <- get_term_label("http://foobar")
  testthat::expect_equal(nrow(lbls), 1)
  testthat::expect_equal(lbls$id, "http://foobar")
  testthat::expect_true(is.na(lbls$label))
})

test_that("labels for pre-generated post-comps", {
  phen <- sample(get_phenotypes("basihyal bone")$id, size = 1)
  subs <- sample(rownames(subsumer_matrix(phen)), size = 30)
  subs.l <- get_term_label(subs, preserveOrder = TRUE)
  testthat::expect_lte(sum(is.na(subs.l$label)), 1)

  subs <- sample(rownames(subsumer_matrix(c("femur"))), 30)
  subs.l <- get_term_label(subs, preserveOrder = TRUE)
  # Unfortunately, there are some regular ontologies for which the database
  # does not consistently have labels. Filter those out.
  ontFilter <- Reduce(
    function(v1, v2) v1 | startsWith(subs.l$id, v2),
    paste0("http://purl.obolibrary.org/obo/", c("CARO"), "_"),
    init = rep(FALSE, times = length(subs.l$id))
  )
  subs.l <- subs.l[! (is.na(subs.l$label) & ontFilter),]
  testthat::expect_lte(sum(is.na(subs.l$label)), 1)
})

test_that("creating terminfo objects and extracting properties", {
  terms = find_term("basihyal bone")

  # not a terminfo object
  testthat::expect_false(is.terminfo(terms))
  testthat::expect_false(is.terminfo(terms[1,]))
  testthat::expect_false(is.terminfo(terms$id[1]))

  # create one terminfo object
  obj <- as.terminfo(terms[1, "id"])
  testthat::expect_is(obj, "terminfo")
  testthat::expect_true(is.terminfo(obj))
  testthat::expect_true(is_valid_terminfo(obj))
  # the current api automatically supplies classification data
  testthat::expect_true("classification" %in% names(obj))
  testthat::expect_equal(length(obj$classification), 3)

  # robust to unresolving IRIs
  testthat::expect_warning(obj <- as.terminfo("foo"))
  testthat::expect_is(obj, "terminfo")
  testthat::expect_true(is.terminfo(obj))
  testthat::expect_false(is_valid_terminfo(obj))

  # robust to unresolving IRIs including classification
  testthat::expect_warning(obj <- as.terminfo("foo", withClassification = TRUE))
  testthat::expect_is(obj, "terminfo")
  testthat::expect_true(is.terminfo(obj))
  testthat::expect_false(is_valid_terminfo(obj))

  # also works with data.frame as input
  obj <- as.terminfo(terms[1,])
  testthat::expect_is(obj, "terminfo")
  testthat::expect_true(is.terminfo(obj))
  testthat::expect_true(is_valid_terminfo(obj))

  # can run vectorized
  testthat::expect_gt(length(terms$id), 1)
  objs <- as.terminfo(terms$id)
  testthat::expect_length(objs, nrow(terms))
  testthat::expect_is(objs, "list")
  testthat::expect_true(all(sapply(objs, is.terminfo)))
  testthat::expect_length(is_valid_terminfo(objs), length(objs))
  testthat::expect_true(all(is_valid_terminfo(objs)))
  l <- sapply(objs, function(p) p$label)
  testthat::expect_true(all(l == terms$label))
  # filter out ZP terms because they do not have synonyms
  non_zp_terms <- terms[terms$isDefinedBy != ontology_iri("ZP"),]
  objs <- as.terminfo(non_zp_terms$id)
  testthat::expect_true(all(sapply(objs, function(p) nrow(p$synonyms)) > 0))
})

test_that("pretty-printing terminfo objects", {
  # find term iri
  term_iri <- find_term("basihyal bone", limit = 1)
  # create terminfo object
  ti <- as.terminfo(term_iri)
  # check print output
  expect_output(print(ti), "terminfo 'basihyal bone' http.*Definition:.*Synonyms:.*Relationships:.*")
  # create terminfo object with classification
  ti <- as.terminfo(term_iri, withClassification = TRUE)
  # check print output including classification sections
  expect_output(print(ti), "terminfo 'basihyal bone' http.*Definition:.*Synonyms:.*Relationships:.*Subclass of:.*Superclass of::*")

  # find taxon term iri
  term_iri <- find_term('Coralliozetus angelicus', limit = 1)
  # create terminfo object
  ti <- as.terminfo(term_iri)
  # check print output including taxon specific sections
  expect_output(print(ti), "terminfo 'Coralliozetus angelicus' http.*Synonyms:.*Extinct:.*Rank:.*Common Name:.*")
})

test_that("as.terminfo withClassification can be controlled via an option", {
  # find term iri
  term_iri <- find_term("basihyal bone", limit = 1)
  # create terminfo object
  ti <- as.terminfo(term_iri)
  # classification is filled in by default
  testthat::expect_true("classification" %in% names(ti))

  # turn on the option to default withClassification to TRUE
  options(rphenoscape.fetch.classification = TRUE)
  # create terminfo object
  ti <- as.terminfo(term_iri)
  # classification should be filled in
  testthat::expect_true("classification" %in% names(ti))
  testthat::expect_equal(length(ti$classification), 3)

  # setting the option to FALSE should return to the default behavior
  options(rphenoscape.fetch.classification = FALSE)
  # create terminfo object
  ti <- as.terminfo(term_iri)
  # classification is filled in by default
  testthat::expect_true("classification" %in% names(ti))
})

test_that("as.terminfo can add classification to terminfo objects", {
  # find term iri
  term_iri <- find_term("basihyal bone", limit = 1)
  # create terminfo object
  ti <- as.terminfo(term_iri)
  # classification is filled in by default
  testthat::expect_true("classification" %in% names(ti))
  # run as.terminfo on a terminfo object requesting classification data
  ti <- as.terminfo(ti, withClassification = TRUE)
  # classification should be filled in
  testthat::expect_true("classification" %in% names(ti))
})
