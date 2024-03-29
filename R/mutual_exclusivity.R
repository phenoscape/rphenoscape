# define mutual exclusivity types
exclusivity_types <- list(strong_compatibility='strong_compatibility',
                          weak_compatibility='weak_compatibility',
                          inconclusive_evidence='inconclusive_evidence',
                          weak_exclusivity='weak_exclusivity',
                          strong_exclusivity='strong_exclusivity')

#' Determine whether two phenotypes are mutually exclusive
#'
#' Determines whether the data in the KB includes evidence for mutual exclusivity
#' between two given phenotypes.
#' 
#' See [mutually_exclusive()] for details on which links in the data are considered
#' as strong or weak evidence for mutual exclusivity and compatibility.
#' 
#' @param phenotype.a character or phenotype object, phenotype ID (IRI) if character, or
#' a phenotype object obtained by passing phenotype ID to as.phenotype() function.
#' @param phenotype.b character or phenotype object, phenotype ID (IRI) if character, or
#' a phenotype object obtained by passing phenotype ID to as.phenotype() function.
#' @param studies character, a vector of study IDs. This is an optional
#' parameter that acts as a filter in case the determination of
#' mutual exclusivity is to be based only on the evidence found in a particular set of studies.
#' The default is NULL, which means
#' that all studies present in the KB database will be considered for evidence.
#' @param charstates dataframe, a dataframe obtained from [charstates()] by
#' passing a list of the two phenotypes as argument. This optional parameter exists to speed up
#' the computation for the [mutually_exclusive()] function since `mutually_exclusive()`
#' repeatedly calls mutual_exclusivity_pairwise(). The default is NULL. _Note that passing this argument
#' but doing so incorrectly can result in wrong output._
#' @param quality_opposites dataframe, an optional dataframe containing columns
#' "quality.a" and "quality.b" to denote pairs of phenotypic quality terms, in
#' the form of their term IRIs, to be considered opposites of each others.
#' See documentation under [mutually_exclusive()] for more details.
#'
#' @return A character (string), the mutual exclusivity type among the two phenotypes.
#' See [mutually_exclusive()] for documentation on the possible values, although note
#' that this function returns these as a character vector, not levels of an ordered factor.
mutual_exclusivity_pairwise <- function(phenotype.a, phenotype.b, studies=NULL, charstates=NULL,
                                        quality_opposites=NULL){

    # convert phenotypes to phenotype objects for faster computation
    if (!is.phenotype(phenotype.a)) {
        phenotype.a <- as.phenotype(phenotype.a, withTaxa=TRUE)
    }

    if (!is.phenotype(phenotype.b)) {
        phenotype.b <- as.phenotype(phenotype.b, withTaxa=TRUE)
    }

    if (!is.null(quality_opposites)) {
      # check validity of the quality_opposites dataframe
      if (!all(c("quality.a","quality.b") %in% colnames(quality_opposites))) {
        stop("Missing a required column for the quality_opposites parameter. The quality_opposites dataframe parameter requires 'quality.a' and 'quality.b' columns to be present.")
      }

      # trim white space from quality opposite IRIs
      quality_opposites$quality.a <- trimws(quality_opposites$quality.a)
      quality_opposites$quality.b <- trimws(quality_opposites$quality.b)
    }

    is_pair_mutually_exclusive <- exclusivity_types$inconclusive_evidence

    # load charstates dataframe that contains both the phenotypes
    if (is.null(charstates)) {
        character_states <- charstates(list(phenotype.a, phenotype.b))
    } else {
        character_states <- charstates
    }

    # filter the character_states dataframe by studies
    if (!is.null(studies)) {
        character_states <- character_states[character_states$study.id %in% studies, ]
    }

    # store the charstate dataframe associated with each phenotype separately for easier computation
    charstates.a <- character_states[(character_states$phenotype.id == phenotype.a$id), ]
    charstates.b <- character_states[(character_states$phenotype.id == phenotype.b$id), ]

    # determine mutual exclusivity only if both phenotypes have non-empty states
    num_states.a <- length(charstates.a$state.id)
    num_states.b <- length(charstates.b$state.id)
    if (num_states.a > 0 && num_states.b > 0) {

        # compute the intersection of the two characters vectors
        chars.a <- unique(charstates.a$character.id)
        chars.b <- unique(charstates.b$character.id)
        mutual_characters_present <- length(intersect(chars.a, chars.b)) > 0

        if (mutual_characters_present) {
            # If the two phenotypes have mutual characters, then
            # there is enough evidence to decide mutual exclusivity among the two phenotypes.
            # Therefore, the mutual exclusion in this case is called 'strong'.
            # Mutual exclusivity is decided by looking at whether the two phenotypes have mutual states.

            states.a <- charstates.a$state.id
            states.b <- charstates.b$state.id
            mutual_states_present <- length(intersect(states.a, states.b)) > 0

            if (mutual_states_present) {
                # strong compatibility if phenotypes share states
                is_pair_mutually_exclusive <- exclusivity_types$strong_compatibility
            } else {
                # strong exclusivity if phenotypes do not share states
                is_pair_mutually_exclusive <- exclusivity_types$strong_exclusivity
            }

        } else {
            # If there are no mutal characters among the two phenotypes, then
            # there is lack (but not absence) of evidence to determine mutual exclusivity.
            # Therefore, the mutual exclusion in this case is called 'weak'.
            # Mutual exclusivity is decided by looking at whether the two phenotypes have mutual taxa.

            mutual_taxa_present <- length(intersect(phenotype.a$taxa$id, phenotype.b$taxa$id)) > 0
            if(mutual_taxa_present){
                # weak compatibility
                is_pair_mutually_exclusive <- exclusivity_types$weak_compatibility
            } else {
                # weak exclusivity
                is_pair_mutually_exclusive <- exclusivity_types$weak_exclusivity
            }
        }

    } else {
        # If either (or both) phenotype(s) have empty state(s), then we say that
        # there is no evidence to compute mutual exclusivity.
        # This block raises a warning and is_pair_mutually_exclusive is returned
        # with the default value: 'inconclusive_evidence'.

        if (num_states.a == 0) {
            warning("Phenotype A does not have any states in the charstates dataframe.")

        } else if (num_states.b == 0){
            warning("Phenotype B does not have any states in the charstates dataframe.")

        } else {
            warning("Both phenotypes do not have any states in the charstates dataframe.")
        }
    }

    # if we have quality opposite data and phenotype entities match
    if (!is.null(quality_opposites) && phenotype.a$eqs$entities == phenotype.b$eqs$entities) {
      # only check phenotypes that both have a single quality
      if (length(phenotype.a$eqs$qualities) == 1 && length(phenotype.b$eqs$qualities) == 1) {
        # find the list of opposites for phenotype.a
        phenotype.a.opposites <- find_quality_opposites(phenotype.a$eqs$qualities, quality_opposites)
        if (any(phenotype.a.opposites == phenotype.b$eqs$qualities)) {
          # strong exclusivity if phenotypes are opposite
          is_pair_mutually_exclusive <- exclusivity_types$strong_exclusivity
        }
      }
    }

    # return mutual exclusivity
    is_pair_mutually_exclusive
}

find_quality_opposites <- function(phenotype_quality, quality_opposites) {
  # returns a vector of quality IRIs that are opposite of phenotype_qualities
  union(
    # for phenotype_quality == quality.a return matching quality.b opposites
    quality_opposites[phenotype_quality == quality_opposites$quality.a]$quality.b,
    # for phenotype_quality == quality.b return matching quality.a opposites
    quality_opposites[phenotype_quality == quality_opposites$quality.b]$quality.a
  )
}

#' Determine mutual exclusivity between two or more phenotypes
#'
#' Determines whether the data in the KB includes evidence for mutual exclusivity
#' between all distinct pairs of phenotypes in the provided list.
#' 
#' Evidence for mutual exclusivity or compatibility between a pair of phenotypes
#' is taken from how they are or aren't linked to the same taxon, character, and
#' character state. If two phenotypes are found to be linked to the same character
#' state at least once, it is considered _strong evidence for mutual compatibility_.
#' If they are found to be linked to the same character at least once, but not
#' to the same character state (implying they must be linked to different states of
#' the same character at least once), it is considered _strong evidence for mutual exclusivity_.
#' Otherwise, if there is no character to which both are linked, the presence and absence
#' of data showing co-occurrence in taxa is used as _weak evidence_. That is, if they are
#' linked to character states of different characters, but the states are both exhibited
#' by the same taxon at least once, it is considered as _weak evidence for mutual compatibility_
#' (weak because the co-occurrence may be due to taxon-level polymorphism),
#' and if there is no such taxon, this is considered as _weak evidence for mutual exclusivity_
#' (because lack of evidence could still be due to gaps in the data, and does therefore not
#' imply evidence of absence).
#' 
#' It is possible that for any pair of phenotypes the KB contains conflicting
#' evidence on mutual exclusivity. For example, two phenotypes could be linked to
#' the same character state for one character, and different character states for
#' another character (possibly from another study by another author), the first of
#' which would present strong evidence for mutual compatibility and the latter
#' strong evidence of mutual exclusivity. The current implementation does not detect
#' and thus ignores this possibility.
#'
#' @param phenotypes character or phenotype, a vector of phenotype IDs, or
#' phenotype objects for which one wants to determine the mutual exclusivity.
#' @param studies character, a vector of study IDs (optional) if one is
#' interested in determining mutual exclusivity only based on the evidence from a
#' particular set of studies. By default _all_ studies in the KB are considered.
#' @param progress_bar logical, whether to print progress of the function.
#'
#' WARNING: setting progress_bar to TRUE clears the R console by executing the
#' cat('\014') command before printing the progress.
#'
#' @param quality_opposites dataframe, an optional dataframe containing columns
#' "quality.a" and "quality.b" to denote pairs of phenotypic quality terms, in
#' the form of their term IRIs, to be considered opposites of each others. If
#' provided, two phenotypes will be determined to have _strong exclusivity_ if
#' their qualities match a pair of opposites. The test will only be applied to
#' pairs of phenotypes in which the EQ expressions of both comprise of the same
#' number of entities and only a single quality term. Columns included in the
#' dataframe other than "quality.a" and "quality.b" will be ignored.
#'
#' @return A list consisting a matrix and a dataframe that contain mutual exclusivity
#' results for the phenotypes.
#' 
#' The mutual exclusivity type between two phenotypes is encoded as an ordered factor with the following
#' levels (in this order):
#' - 'strong_compatibility'  : the data show strong evidence of mutual compatibility;
#' - 'weak_compatibility'    : the data suggest mutual compatibility, but the evidence is weak;
#' - 'inconclusive_evidence' : the default for phenotypes that lack links to character states;
#' - 'weak_exclusivity'      : the data suggest mutual exclusivity, but the evidence is weak;
#' - 'strong_exclusivity'    : the data show strong evidence of mutual exclusivity.
#'
#' The first item in the list is a square matrix with each dimension equal to the
#' number of phenotypes. The cells (_i_,_j_) of the matrix are the integer representation of
#' the factor level for the mutual exclusivity type between the _i_ th and _j_ th phenotype
#' in the list of phenotypes provided as input. The matrix is symmetric.
#'
#' The second item in the list is a dataframe with five columns, the labels and ids
#' of each phenotype of a pair, and the mutual exclusivity type of the pair is in the
#' fifth column.
#'
#' @examples
#' # Example 1: pass phenotypes as characters with phenotype IDs
#' # get a specific study of interest based on which mutual exclusivity is to be determined
#' studies <- get_studies()
#' study <- studies$id[studies$label == 'Dillman et al. (2016)']
#'
#' # get phenotypes ids present in the study
#' phenotypes <- get_phenotypes(study = study)
#' # select 10 phenotype ids randomly from those in the study
#' phenotypes_ids <- sample(phenotypes$id, 10)
#'
#' # determine mutual exclusivity
#' exclusivity <- mutually_exclusive(phenotypes_ids)
#'
#' # resultant matrix
#' exclusivity$matrix
#'
#' # resultant dataframe
#' str(exclusivity$dataframe)
#'
#' # filter dataframe to contain only mutually exclusive phenotypes
#' exclusivity_df <- exclusivity$dataframe
#' exclusive_phenotype_pairs <- exclusivity_df[
#'     exclusivity_df$mutual_exclusivity == "weak_exclusivity" |
#'     exclusivity_df$mutual_exclusivity == "strong_exclusivity", ]
#'
#' # Example 2: pass phenotypes as phenotype objects
#' # sample some phenotypes objects present in the study
#' phenotypes_objs <- as.phenotype(sample(phenotypes$id, 4), withTaxa=TRUE)
#'
#' # determine mutual exclusivity
#' exclusivity <- mutually_exclusive(phenotypes_objs)
#'
#' # resultant matrix
#' exclusivity$matrix
#'
#' # resultant dataframe
#' str(exclusivity$dataframe)
#' 
#' # Example 3: determine mutual exclusivity for two phenotypes
#' # fetch phenotypes for an entity/taxon combination
#' phenotypes <- get_phenotypes(entity="hyomandibular bone", taxon="Hypancistrus")
#'
#' # select two phenotype ids based on the labels
#' phenotypes_ids <- c(
#'   phenotypes$id[phenotypes$label == 'hyomandibular bone in contact with prootic bone'],
#'   phenotypes$id[phenotypes$label == 'hyomandibular bone in contact with quadrate bone']
#' )
#'
#' # determine mutual exclusivity
#' exclusivity <- mutually_exclusive(phenotypes_ids)
#'
#' # exclusivity value
#' exclusivity$dataframe$mutual_exclusivity
#'
#' # Example 4: determine mutual exclusivity for two phenotypes including opposite quality data
#'
#' # create a list of phenotypes to compare (femur elongated vs femur decreased length)
#' phens <- get_phenotypes(entity="femur", quality="elongated")
#' femur_elongated_iri <- phens$id[phens$label == "femur elongated"]
#' phens <- get_phenotypes(entity="femur", quality="decreased length")
#' femur_decreased_length_iri <- phens$id[phens$label == "femur decreased length"]
#' phenotypes_to_compare <- c(femur_elongated_iri, femur_decreased_length_iri)
#'
#' # compare the phenotypes without using opposite quality data
#' exclusivity <- mutually_exclusive(phenotypes_to_compare)
#' exclusivity$dataframe$mutual_exclusivity
#'
#' # create a dataframe containing the quality opposites
#' elongated_iri <- find_term("elongated", matchTypes = "exact")$id
#' decreased_length_iri <- find_term("decreased length", matchTypes = "exact")$id
#' quality_opposites <- data.frame(
#'   quality.a = elongated_iri,
#'   quality.b = decreased_length_iri
#' )
#'
#' # compare the phenotypes using opposite quality data
#' exclusivity <- mutually_exclusive(phenotypes_to_compare, quality_opposites=quality_opposites)
#' exclusivity$dataframe$mutual_exclusivity
#' @export
mutually_exclusive <- function(phenotypes, studies=NULL, progress_bar=FALSE, quality_opposites=NULL){

    # make sure that at least two phenotypes are passed
    if (is.null(phenotypes) || length(phenotypes) == 1) {
        stop("Less than two phenotypes passed. The function expects at least two phenotypes to compute mutual exclusivity.")
    }

    # initialize a square matrix to store mutual exclusivity result among each pair of phenotypes
    num_phenotypes <- length(phenotypes)
    mutual_exclusivity_matrix <- matrix(as.integer(factor(exclusivity_types$inconclusive_evidence,
                                                          levels = exclusivity_types,
                                                          ordered = TRUE)),
                                        nrow=num_phenotypes,
                                        ncol=num_phenotypes)
    rownames(mutual_exclusivity_matrix) <- 1:num_phenotypes
    colnames(mutual_exclusivity_matrix) <- 1:num_phenotypes

    # determine the number of combinations of phenotypes taken 2 at a time
    num_combinations = choose(length(phenotypes), 2)

    # create a pre-allocated set of vectors to store mutual exclusive pair data
    id.1  <- character(num_combinations)
    label.1 <- character(num_combinations)
    id.2 <- character(num_combinations)
    label.2 = character(num_combinations)
    mutual_exclusivity_vector = character(num_combinations)

    # initialize variables to keep track of progress of the function
    if (progress_bar) {
        iteration <- 1
        total_iterations <- choose(num_phenotypes, 2) + num_phenotypes
        num_mutual_exclusive_pairs <- 0
        num_mutual_compatible_pairs <- 0
        num_inconclusive_evidence <- 0
    }

    # convert phenotypes to phenotype objects for faster computation
    if (!all(sapply(phenotypes, is.phenotype))) {
        phenotypes <- as.phenotype(phenotypes, withTaxa=TRUE)
    }

    # load charstates dataframe that contains all the phenotypes
    character_states <- charstates(phenotypes)

    vec_idx <- 1
    # find mutual exclusivity for each pair in the phenotype vector
    for (row in 1:num_phenotypes) {
        for (column in row:num_phenotypes) {

            # print progress of the loop
            if (progress_bar) {
                # prevent flickering by only displaying every 100 iterations
                if (iteration %% 100 == 1) {
                    cat('\014')
                    cat(paste0(round(iteration / (total_iterations) * 100), '% completed\n',
                               'Mutually exclusive pairs  = ', num_mutual_exclusive_pairs, '\n',
                               'Mutually compatible pairs = ', num_mutual_compatible_pairs, '\n',
                               'Inconclusive evidence     = ', num_inconclusive_evidence, '\n'))
                }
                iteration <- iteration + 1
            }

            # compute mutual exclusivity among distinct phenotypes only
            if (row != column) {

                # compute mutual exclusivity
                mutual_exclusivity <- mutual_exclusivity_pairwise(phenotypes[[row]],
                                                              phenotypes[[column]],
                                                              studies=studies,
                                                              charstates=character_states,
                                                              quality_opposites=quality_opposites)

                # store exclusivity result in matrix
                mutual_exclusivity_integer <- match(mutual_exclusivity, exclusivity_types)
                mutual_exclusivity_matrix[row, column] <- mutual_exclusivity_integer
                mutual_exclusivity_matrix[column, row] <- mutual_exclusivity_integer

                # store exclusivity result in vectors
                id.1[vec_idx] <- phenotypes[[row]]$id
                label.1[vec_idx] <- phenotypes[[row]]$label
                id.2[vec_idx] <- phenotypes[[column]]$id
                label.2[vec_idx] <- phenotypes[[column]]$label
                mutual_exclusivity_vector[vec_idx] <- mutual_exclusivity
                # increment vectors index
                vec_idx <- vec_idx + 1

                # update progress bar statistics
                if (progress_bar) {
                    if (mutual_exclusivity == exclusivity_types$strong_exclusivity || mutual_exclusivity == exclusivity_types$weak_exclusivity) {
                        # keep track of the number of exclusive phenotype pairs for printing in the progress bar
                        num_mutual_exclusive_pairs <- num_mutual_exclusive_pairs + 1

                    } else if (mutual_exclusivity == exclusivity_types$inconclusive_evidence) {
                        # keep track of the number of phenotype pairs with inconclusive evidence for printing in the progress bar
                        num_inconclusive_evidence <- num_inconclusive_evidence + 1

                    } else if (mutual_exclusivity == exclusivity_types$strong_compatibility || mutual_exclusivity == exclusivity_types$weak_compatibility) {
                        # keep track of the number of compatible phenotype pairs for printing in the progress bar
                        num_mutual_compatible_pairs <- num_mutual_compatible_pairs + 1
                    }
                }

            } else if (row == column) {
                # a phenotype is strongly compatible with itself
                mutual_exclusivity_matrix[row, column] <- factor(exclusivity_types$strong_compatibility,
                                                                 levels = exclusivity_types,
                                                                 ordered = TRUE)
            }
        }
    }

    mutual_exclusivity_factors <- factor(mutual_exclusivity_vector, levels = exclusivity_types, ordered = TRUE)
    mutual_exclusivity_df <- data.frame(id.1    = id.1,
                                        label.1 = label.1,
                                        id.2    = id.2,
                                        label.2 = label.2,
                                        mutual_exclusivity = mutual_exclusivity_factors,
                                        stringsAsFactors = FALSE)

    # return a list containing the resultant matrix and the dataframe
    list(matrix=mutual_exclusivity_matrix,
         dataframe=mutual_exclusivity_df)
}
