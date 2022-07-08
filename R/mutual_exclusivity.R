#' Determine whether two phenotypes are mutually exclusive.
#'
#' The function expects the two phenotypes to be passed as two separate arguments, not as a vector.
#' The order in which the phenotypes are passed does not matter.
#'
#' @param phenotype.a character or phenotype object, phenotype ID (IRI) if character, or
#' a phenotype object obtained by passing phenotype ID to as.phenotype() function.
#' @param phenotype.b character or phenotype object, phenotype ID (IRI) if character, or
#' a phenotype object obtained by passing phenotype ID to as.phenotype() function.
#' @param studies character, a vector of study IDs. This is an optional
#' parameter that acts as a filter in case the user wants to determine
#' mutual exclusivity based on the evidence found in a particular set of studies.
#' The default is NULL which means that no filter is applied and the function
#' looks at all the studies present in the KB database.
#' @param charstates dataframe, a dataframe obtained by running
#' charstates(list(phenotype.a, phenotype.b)) where phenotypes are passed as
#' phenotype objects. This optional parameter exists to speed up the computation
#' for the mutual_exclusivity_test() function since mutual_exclusivity_test()
#' repeatedly calls mutual_exclusivity_pair(). The default is NULL, and it is
#' not recommended to use this argument while using the mutual_exclusivity_pair()
#' function as passing this argument makes negligible runtime speedup but increases
#' the risk of getting a wrong output if the correct charstates dataframe
#' corresponding to the two input phenotypes is not passed.
#'
#' @return is_pair_mutually_exclusive factor, the mutual exclusivity type among the two phenotypes.
#' is_pair_mutually_exclusive has the following levels:
#'
#' 'strong_compatibility'  : if the two phenotypes share one or more character-state value(s),
#' they are mutually compatible (strong evidence).
#'
#' 'weak_compatibility'    : if the two phenotypes do not share any characters,
#' but there are taxa exhibiting both of their respective character states,
#' then they are mutually compatible (weak evidence).
#'
#' 'inconclusive_evidence' : default if no evidence supporting or contradicting
#' mutual exclusivity can be found.
#'
#' 'weak_exclusivity'      : if the two phenotypes do not share any characters,
#' and there are no taxa exhibiting both of their respective character states,
#' then they are mutually exclusive (weak evidence).
#'
#' 'strong_exclusivity'    : if the two phenotypes share one or more characters,
#' but do not share any states, then they are mutually exclusive (strong evidence).
#'
#' The levels presented above also have the following order:
#' strong_compatibility < weak_compatibility < inconclusive_evidence < weak_exclusivity < strong_exclusivity
#'
#' The default exclusivity type "inconclusive_evidence" is present at the center in the order.
#' The further away a level is from the center level, the stronger the evidence is
#' that is found to determine compatibility or exclusivity. Exclusivity has higher order because
#' that is the primary question one is interested to answer.
#'
#' @examples
#' \dontrun{
#' # Example 1: looking at entire database while determining exclusivity
#' # get all studies in the database
#' studies <- pk_get_study_list()
#'
#' # get all phenotypes in the databse
#' phenotypes <- get_phenotypes()
#' phenotypes <- phenotypes$id
#'
#' # determine mutual exclusivity
#' exclusivity <- mutual_exclusivity_pair(phenotypes[1], phenotypes[7])
#'
#'
#' # Example 2: passing phenotype objects and looking at a particular study while determining exclusivity
#' # get a specific study of interest based on which mutual exclusivity is to be determined
#' studies <- pk_get_study_list()
#' study <- studies$id[studies$label == 'Dillman et al. (2016)']
#'
#' # get phenotypes objects present in the study
#' phenotypes <- get_phenotypes(study = study)
#' phenotypes <- as.phenotype(phenotypes, withTaxa=TRUE)
#'
#' # determine mutual exclusivity
#' exclusivity <- mutual_exclusivity_pair(phenotype.a = phenotypes[5], phenotype.b = phenotypes[10], studies=study)
#'
#'
#' # Example 3 - passing charstates dataframe
#' # get all studies in the database
#' studies <- pk_get_study_list()
#'
#' # get all phenotypes in the database
#' phenotypes <- get_phenotypes()
#' phenotypes <- phenotypes$id
#'
#' # get charstates dataframe; phenotypes must be passed as phenotype objects
#' char_states <- charstates(list(phenotype.a <- as.phenotype(phenotypes[2]), phenotype.b <- as.phenotype(phenotypes[11])))
#'
#' # determine mutual exclusivity
#' exclusivity <- mutual_exclusivity_pair(phenotype.a = phenotype.a, phenotype.b = phenotype.b, charstates = char_states)
#' }
#'
#' @export
mutual_exclusivity_pair <- function(phenotype.a, phenotype.b, studies=NULL, charstates=NULL){

    # convert phenotypes to phenotype objects for faster computation
    if (!is.phenotype(phenotype.a)) {
        phenotype.a <- as.phenotype(phenotype.a, withTaxa=TRUE)
    }

    if (!is.phenotype(phenotype.b)) {
        phenotype.b <- as.phenotype(phenotype.b, withTaxa=TRUE)
    }

    # define mutual exclusivity types
    exclusivity_types <- c('strong_compatibility',
                           'weak_compatibility',
                           'inconclusive_evidence',
                           'weak_exclusivity',
                           'strong_exclusivity')
    is_pair_mutually_exclusive <- exclusivity_types[3]

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
                is_pair_mutually_exclusive <- exclusivity_types[1]
            } else {
                # strong exclusivity if phenotypes do not share states
                is_pair_mutually_exclusive <- exclusivity_types[5]
            }

        } else {
            # If there are no mutal characters among the two phenotypes, then
            # there is lack (but not absence) of evidence to determine mutual exclusivity.
            # Therefore, the mutual exclusion in this case is called 'weak'.
            # Mutual exclusivity is decided by looking at whether the two phenotypes have mutual taxa.

            mutual_taxa_present <- length(intersect(phenotype.a$taxa$id, phenotype.b$taxa$id)) > 0
            if(mutual_taxa_present){
                # weak compatibility
                is_pair_mutually_exclusive <- exclusivity_types[2]
            } else {
                # weak exclusivity
                is_pair_mutually_exclusive <- exclusivity_types[4]
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

    # convert mutual exclusivity state to a factor
    is_pair_mutually_exclusive <- factor(is_pair_mutually_exclusive,
                                         levels = exclusivity_types,
                                         ordered = TRUE)
    # return mutual exclusivity
    is_pair_mutually_exclusive
}

#' Determine mutual exclusivity among two or more phenotypes.
#'
#' The function computes mutual exclusion among all distinct pairs of phenotypes
#' by making calls to the mutual_exclusivity_pair() function.
#'
#' @param phenotypes character or phenotype, a vector of phenotype IDs, or
#' phenotype objects for which one wants to determine the mutual exclusivity.
#' @param studies character, a vector of study IDs (optional) if one is
#' interested in determining mutual exclusivity based on the evidence from a
#' particular set of studies.
#' @param progress_bar logical, whether to print progress of the function. Default
#' is TRUE since it is suggested to track the progress of the function especially
#' if several (hundred) phenotypes are passed.
#'
#' WARNING: setting progress_bar to TRUE clears the R console by executing the
#' cat('\014') command before printing the progress.
#'
#' @return A list consisting a matrix and a dataframe that contain mutual exclusivity
#' results for the phenotypes.
#' The first item in the list is a matrix which is square and whose size is
#' equal to the number of phenotypes.
#' Each cell of the matrix contains an integer value that informs about the mutual exclusivity
#' among the phenotypes corresponding to the row and the column of the cell.
#' The integers are numeric representation of the factor level returned by the
#' mutual_exclusivity_pair() function. Refer the documentation of mutual_exclusivity_pair()
#' to see its return value.
#'
#' The second item in the list is a dataframe that contains five columns:
#' the labels and ids of each phenotype of a pair, and the mutual exclusivity
#' type of the pair. The fifth column 'mutual_exclusivity' is an ordered factor
#' that has the same levels as returned by the mutual_exclusivity_pair() function.
#' Refer the documentation of mutual_exclusivity_pair() to see its return value.
#'
#' @examples
#' \dontrun{
#' # Example 1: pass phenotypes as characters with phenotype IDs
#' # get a specific study of interest based on which mutual exclusivity is to be determined
#' studies <- pk_get_study_list()
#' study <- studies$id[studies$label == 'Dillman et al. (2016)']
#'
#' # get phenotypes ids present in the study
#' phenotypes <- get_phenotypes(study = study)
#' phenotypes_ids <- phenotypes$id
#'
#' # determine mutual exclusivity
#' exclusivity <- mutual_exclusivity(phenotypes_ids)
#'
#' # resultant matrix
#' exclusivity$matrix
#'
#' # resultant dataframe
#' str(exclusivity$dataframe)
#'
#' # filter dataframe to contain only mutually exclusive phenotypes
#' exclusivity_df <- exclusivity$dataframe
#' exclusive_phenotype_pairs <- exclusivity_df[exclusivity_df$mutual_exclusivity == "weak_exclusivity" |
#'                                                                           exclusivity_df$mutual_exclusivity == "strong_exclusivity", ]
#'
#' # Example 2: pass phenotypes as phenotype objects
#' # get phenotypes objects present in the study
#' phenotypes <- get_phenotypes(study = study)
#' phenotypes_objs <- as.phenotype(phenotypes, withTaxa=TRUE)
#'
#' # determine mutual exclusivity
#' exclusivity <- mutual_exclusivity(phenotypes_objs)
#'
#' # resultant matrix
#' exclusivity$matrix
#'
#' # resultant dataframe
#' str(exclusivity$dataframe)
#' }
#' @export
mutual_exclusivity <- function(phenotypes, studies=NULL, progress_bar=TRUE){

    # make sure that at least two phenotypes are passed
    if (is.null(phenotypes) || length(phenotypes) == 1) {
        stop("Less than two phenotypes passed. The function expects at least two phenotypes to compute mutual exclusivity.")
    }

    # initialize a square matrix to store mutual exclusivity result among each pair of phenotypes
    num_phenotypes <- length(phenotypes)
    exclusivity_types <- c('strong_compatibility',
                           'weak_compatibility',
                           'inconclusive_evidence',
                           'weak_exclusivity',
                           'strong_exclusivity')
    mutual_exclusivity_matrix <- matrix(as.integer(factor("inconclusive_evidence",
                                                          levels = exclusivity_types,
                                                          ordered = TRUE)),
                                        nrow=num_phenotypes,
                                        ncol=num_phenotypes)
    rownames(mutual_exclusivity_matrix) <- 1:num_phenotypes
    colnames(mutual_exclusivity_matrix) <- 1:num_phenotypes

    # create empty dataframe to store mutual exclusive pairs
    mutual_exclusivity_df <- data.frame(id.1    = character(),
                                        label.1 = character(),
                                        id.2    = character(),
                                        label.2 = character(),
                                        mutual_exclusivity = factor(c(),
                                                                    levels = exclusivity_types,
                                                                    ordered = TRUE),
                                        stringsAsFactors = FALSE)

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

    # find mutual exclusivity for each pair in the phenotype vector
    for (row in 1:num_phenotypes) {
        for (column in row:num_phenotypes) {

            # print progress of the loop
            if (progress_bar == TRUE) {
                cat('\014')
                cat(paste0(round(iteration / (total_iterations) * 100, 4), '% completed\n',
                           'Mutually exclusive pairs  = ', num_mutual_exclusive_pairs, '\n',
                           'Mutually compatible pairs = ', num_mutual_compatible_pairs, '\n',
                           'Inconclusive evidence     = ', num_inconclusive_evidence, '\n'))
                iteration <- iteration + 1
            }

            # compute mutual exclusivity among distinct phenotypes only
            if (row != column) {

                # compute mutual exclusivity
                mutual_exclusivity <- mutual_exclusivity_pair(phenotypes[[row]],
                                                              phenotypes[[column]],
                                                              studies=studies,
                                                              charstates=character_states)

                # store exclusivity result in matrix
                mutual_exclusivity_matrix[row, column] <- mutual_exclusivity
                mutual_exclusivity_matrix[column, row] <- mutual_exclusivity

                # store exclusivity result in dataframe
                dataframe_row <- data.frame(id.1    = phenotypes[[row]]$id,
                                            label.1 = phenotypes[[row]]$label,
                                            id.2    = phenotypes[[column]]$id,
                                            label.2 = phenotypes[[column]]$label,
                                            mutual_exclusivity=mutual_exclusivity,
                                            stringsAsFactors = FALSE
                )
                mutual_exclusivity_df <- rbind(mutual_exclusivity_df, dataframe_row, stringsAsFactors=FALSE)

                # update progress bar statistics
                if (progress_bar) {
                    if (mutual_exclusivity == "strong_exclusivity" || mutual_exclusivity == "weak_exclusivity") {
                        # keep track of the number of exclusive phenotype pairs for printing in the progress bar
                        num_mutual_exclusive_pairs <- num_mutual_exclusive_pairs + 1

                    } else if (mutual_exclusivity == "inconclusive_evidence") {
                        # keep track of the number of phenotype pairs with inconclusive evidence for printing in the progress bar
                        num_inconclusive_evidence <- num_inconclusive_evidence + 1

                    } else if (mutual_exclusivity == "strong_compatibility" || mutual_exclusivity == "weak_compatibility") {
                        # keep track of the number of compatible phenotype pairs for printing in the progress bar
                        num_mutual_compatible_pairs <- num_mutual_compatible_pairs + 1
                    }
                }

            } else if (row == column) {
                # a phenotype is strongly compatible with itself
                mutual_exclusivity_matrix[row, column] <- factor("strong_compatibility",
                                                                 levels = exclusivity_types,
                                                                 ordered = TRUE)
            }
        }
    }

    # return a list containing the resultant matrix and the dataframe
    list(matrix=mutual_exclusivity_matrix,
         dataframe=mutual_exclusivity_df)
}
