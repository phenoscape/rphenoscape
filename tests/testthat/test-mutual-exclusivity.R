context("mutually exclusive")

test_that("determine mutual exclusive evidence for two phenotype IRIs", {
  phenotype1 <- get_phenotypes("basihyal bone", quality = "bifurcated")
  phenotype2 <- get_phenotypes("basihyal bone", quality = "cylindrical")
  result_list <- mutually_exclusive(c(phenotype1$id, phenotype2$id), progress_bar = FALSE)

  # The result is a list with names matrix and dataframe
  expect_is(result_list, "list")
  expect_equivalent(names(result_list), c("matrix", "dataframe"))

  # The dataframe should be data.frame with 5 columns and 1 row
  df <- result_list$dataframe
  expect_is(df, "data.frame")
  expect_equivalent(colnames(df), c("id.1", "label.1", "id.2", "label.2", "mutual_exclusivity"))
  expect_equal(nrow(df), 1)
  # The values of the first 4 columns come from the input phenotypes
  expect_equal(df$id.1, phenotype1$id)
  expect_equal(df$label.1, phenotype1$label)
  expect_equal(df$id.2, phenotype2$id)
  expect_equal(df$label.2, phenotype2$label)
  # The mutual_exclusivity column should be a factor with specific levels
  expect_is(df$mutual_exclusivity, "factor")
  expected_levels <- c("strong_compatibility", "weak_compatibility", "inconclusive_evidence", "weak_exclusivity", "strong_exclusivity")
  expect_equivalent(levels(df$mutual_exclusivity), expected_levels)

  # The matrix should be a NxN matrix where N is the number of phenotypes (2 in this case)
  mat <- result_list$matrix
  expect_is(mat, "matrix")
  expect_equivalent(dim(mat), c(2,2))
  # Check the values of the matrix
  # Identical phenotypes and should have strong compatibility.
  # The combination of phenotype1 and phenotype2 should be the derived mutual_exclusivity value from the dataframe.
  strong_compatibility_factor <- factor("strong_compatibility", levels = expected_levels, ordered = TRUE)  
  expected_matrix_data <- as.integer(c(
    strong_compatibility_factor, df$mutual_exclusivity, 
    df$mutual_exclusivity, strong_compatibility_factor))
  expect_equivalent(mat, matrix(expected_matrix_data, nrow = 2, ncol = 2))
})

test_that("determining mutual exclusive evidence errors with less than two phenotypes", {
  phenotype1 <- get_phenotypes("basihyal bone", quality = "bifurcated")
  expect_error(mutually_exclusive(c(phenotype1$id), progress_bar = FALSE))
  expect_error(mutually_exclusive(c(), progress_bar = FALSE))
})

test_that("determine mutual exclusive evidence for phenotype objects", {
  phenotypes <- as.phenotype(head(get_phenotypes("basihyal bone"), 4))
  result_list <- mutually_exclusive(phenotypes, progress_bar = FALSE)
  # matrix should be 4x4 matching the three input phenotypes
  expect_equivalent(dim(result_list$matrix), c(4, 4))
  # dataframe should have a row for each combination of 2 items from then 4 phenotypes (6 rows)
  expect_equal(nrow(result_list$dataframe), 6)
})

test_that("test progress bar when determining mutual exclusive evidence", {
  phenotype_ids <- head(get_phenotypes("basihyal bone")$id, 4)
  # test that no output is printed by default or if progress_bar is FALSE
  expect_silent(result_list <- mutually_exclusive(phenotype_ids))
  expect_silent(result_list <- mutually_exclusive(phenotype_ids, progress_bar = FALSE))
  # test that there is no output printed when progress_bar is TRUE
  expect_output(result_list <- mutually_exclusive(phenotype_ids, progress_bar = TRUE))
})

test_that("test determining mutual exclusive evidence with opposites", {
  weak_exclusivity_factor <- factor("weak_exclusivity", levels = exclusivity_types, ordered = TRUE)
  strong_exclusivity_factor <- factor("strong_exclusivity", levels = exclusivity_types, ordered = TRUE)
  phens <- get_phenotypes(entity="femur", quality="elongated")
  femur_elongated_iri <- phens$id[phens$label == "femur elongated"]
  phens <- get_phenotypes(entity="femur", quality="decreased length")
  femur_decreased_length_iri <- phens$id[phens$label == "femur decreased length"]
  phenotypes_to_compare <- c(femur_elongated_iri, femur_decreased_length_iri)

  me <- mutually_exclusive(phenotypes_to_compare, progress_bar = FALSE)$dataframe$mutual_exclusivity
  expect_equal(me, weak_exclusivity_factor)

  elongated_iri <- "http://purl.obolibrary.org/obo/PATO_0001154"
  decreased_length_iri <- "http://purl.obolibrary.org/obo/PATO_0000574"

  quality_opposites <- data.frame(
    quality.a = elongated_iri,
    quality.b = decreased_length_iri,
    other.data = "stuff"
  )
  me <- mutually_exclusive(phenotypes_to_compare, progress_bar = FALSE,
                           quality_opposites = quality_opposites)$dataframe$mutual_exclusivity
  expect_equal(me, strong_exclusivity_factor)

  # order of opposites does not matter and extra columns are allowed
  quality_opposites <- data.frame(
    quality.a = decreased_length_iri,
    quality.a_label = "decreased length",
    quality.b = elongated_iri,
    quality.b_label = "elongated_iri"
  )
  me <- mutually_exclusive(phenotypes_to_compare, progress_bar = FALSE,
                           quality_opposites = quality_opposites)$dataframe$mutual_exclusivity
  expect_equal(me, strong_exclusivity_factor)
})

test_that("test determining mutual exclusive evidence with opposites trims IRIs", {
  strong_exclusivity_factor <- factor("strong_exclusivity", levels = exclusivity_types, ordered = TRUE)
  phens <- get_phenotypes(entity="femur", quality="elongated")
  femur_elongated_iri <- phens$id[phens$label == "femur elongated"]
  phens <- get_phenotypes(entity="femur", quality="decreased length")
  femur_decreased_length_iri <- phens$id[phens$label == "femur decreased length"]
  phenotypes_to_compare <- c(femur_elongated_iri, femur_decreased_length_iri)

  elongated_iri <- " http://purl.obolibrary.org/obo/PATO_0001154 "
  decreased_length_iri <- "\thttp://purl.obolibrary.org/obo/PATO_0000574   "

  quality_opposites <- data.frame(
    quality.a = elongated_iri,
    quality.b = decreased_length_iri
  )
  me <- mutually_exclusive(phenotypes_to_compare, progress_bar = FALSE,
                           quality_opposites = quality_opposites)$dataframe$mutual_exclusivity
  expect_equal(me, strong_exclusivity_factor)
})

test_that("test determining mutual exclusive evidence with opposites checks dataframe columns", {
  elongated_iri <- "http://purl.obolibrary.org/obo/PATO_0001154"
  decreased_length_iri <- "http://purl.obolibrary.org/obo/PATO_0000574"
  phenotype1 <- get_phenotypes("basihyal bone", quality = "bifurcated")
  phenotype2 <- get_phenotypes("basihyal bone", quality = "cylindrical")
  phenotypes <- c(phenotype1$id, phenotype2$id)
  quality_opposites <- data.frame(
    quality.a = elongated_iri,
    quality.two = decreased_length_iri
  )
  expect_error(mutually_exclusive(phenotypes, quality_opposites = quality_opposites))
})
