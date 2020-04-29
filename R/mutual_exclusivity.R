#' Determine whether two phenotypes are mutually exclusive.
#'
#' The function expects two phenotypes, not a vector.
#' Studies can be provided in case one is interested in determining the mutual
#' exclusivity among the two phenotypes based on the evidence found in a
#' particular set of studies.
#' A charstates dataframe is generally not required. It exists to make
#' computation for the mutual_exclusivity_test() function faster since
#' it repeatedly calls mutual_exclusivity_pair_test().
#'
#' @param phenotype.a character, phenotype ID (IRI) or a phenotype object.
#' @param phenotype.b character, phenotype ID (IRI) or a phenotype object.
#' @param studies character, a vector of study IDs.
#' @param charstates dataframe, a dataframe obtained by running
#' charstates(list(phenotype.a, phenotype.b))
#'
#' @return is_pair_mutually_exclusive character, the mutual exclusivity result of two phenotypes.
#' The output can be one of the following:
#' 'inconclusive_evidence' : default if no evidence supporting or contradicting
#' mutual exclusivity can be found.
#'
#' 'strong_compatibility'  : if the two phenotypes share one or more character-state value(s),
#' they are mutually compatible (strong evidence).
#'
#' 'strong_exclusivity'    : if the two phenotypes share one or more characters,
#' but do not share any states, then they are mutually exclusive (strong evidence).
#'
#' 'weak_compatibility'    : if the two phenotypes do not share any characters,
#' but there are taxa exhibiting both of their respective character states,
#' then they are mutually compatible (weak evidence).
#'
#' 'weak_exclusivity'      : if the two phenotypes do not share any characters,
#' and there are no taxa exhibiting both of their respective character states,
#' then they are mutually exclusive (weak evidence).
#'
#' @examples
#' \dontrun{
#' # get all studies in the database
#' studies <- pk_get_study_list()
#' study <- studies$id[studies$label == 'Dillman et al. (2016)']
#'
#' # get phenotypes
#' phenotypes <- get_phenotypes(study=study)
#' #phenotypes <- phenotypes$id  # slower
#' phenotypes <- as.phenotype(phenotypes, withTaxa=TRUE)  # much faster, recommended
#'
#' # determine mutual exclusivity between the first and fith phenotypes in the study
#' print(mutual_exclusivity_pair_test(phenotypes[1], phenotypes[5]))
#' }
#'
#' @export
mutual_exclusivity_pair_test <- function(phenotype.a, phenotype.b, studies=NULL, charstates=NULL){

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
            warning("Phenotype 1 does not have any states in the charstates dataframe.")

        } else if (num_states.b == 0){
            warning("Phenotype 2 does not have any states in the charstates dataframe.")

        } else {
            warning("Both phenotypes do not have any states in the charstates dataframe.")
        }
    }

    # return mutual exclusivity
    is_pair_mutually_exclusive
}

#' Determine mutual exclusivity among multiple phenotypes.
#'
#' This function computes mutual exclusion among all pairs of phenotypes
#' in the phenotypes vector. It does so by calling the mutual_exclusivity_pair_test()
#' function for each phenotype pair.
#'
#' @param phenotypes phenotype, a vector of phenotype objects or a vector of IRIs of
#' phenotypes for which one wants to determine the mutual exclusivity.
#' Passing phenotypes as phenotype objects results in much faster computation
#' than passing phenotype IRIs.
#' @param studies character, a vector of study IDs if one is interested to
#' determine mutual exclusivity based on the evidence from a particular set
#' of studies.
#' @param progress_bar logical, whether to print progress of the function.
#' If more than several hundred phenotypes are passed, it is suggested to track
#' the progress of the function by setting this parameter to TRUE.
#'
#' WARNING: setting progress_bar to TRUE clears the R console and then prints the progress.
#'
#' @return A list of a matrix and a dataframe containing mutual exclusivity
#' results for the phenotypes.
#' The resulting matrix is square where its size is equal to the number of phenotypes.
#' Each cell contains a value that informs about the mutual exclusivity
#' among the phenotypes corresponding to the row and the column of the cell.
#' The resulting dataframe contains five columns: the labels and ids of each phenotype
#' in a pair, and the mutual exclusivity value of the pair.
#'
#' @examples
#' \dontrun{
#' # get all studies in the database
#' studies <- pk_get_study_list()
#' study <- studies$id[studies$label == 'Dillman et al. (2016)']
#'
#' # get phenotypes
#' phenotypes <- get_phenotypes(study=study)
#' #phenotypes <- phenotypes$id  # slower
#' phenotypes <- as.phenotype(phenotypes, withTaxa=TRUE)  # much faster, recommended
#'
#'# compute mutual exclusivity
#' me <- mutual_exclusivity_test(phenotypes, study, progress_bar=TRUE)
#'
#' # look at the resultant matrix
#' me$matrix
#'
#' # look at resultant dataframe
#' head(me$dataframe[, c('label.1', 'label.2', 'mutual_exclusivity')])
#' }
#' @export
mutual_exclusivity_test <- function(phenotypes, studies=NULL, progress_bar=TRUE){

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
    mutual_exclusivity_matrix <- matrix("inconclusive evidence",
                                        nrow=num_phenotypes,
                                        ncol=num_phenotypes)
    rownames(mutual_exclusivity_matrix) <- 1:num_phenotypes
    colnames(mutual_exclusivity_matrix) <- 1:num_phenotypes

    # boolean to determine whether resultant dataframe is created or not
    dataframe_created <- FALSE

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
                mutual_exclusivity <- mutual_exclusivity_pair_test(phenotypes[[row]],
                                                                   phenotypes[[column]],
                                                                   studies=studies,
                                                                   charstates=character_states)
                # store exclusivity as factor
                mutual_exclusivity <- factor(mutual_exclusivity,
                                             levels = exclusivity_types,
                                             ordered = TRUE)

                # store exclusivity result in matrix
                mutual_exclusivity_matrix[row, column] <- mutual_exclusivity
                mutual_exclusivity_matrix[column, row] <- mutual_exclusivity

                # store exclusivity result in a dataframe
                dataframe_row <- c(phenotypes[[row]]$id,
                                   phenotypes[[row]]$label,
                                   phenotypes[[column]]$id,
                                   phenotypes[[column]]$label,
                                   as.character(mutual_exclusivity)
                )

                # create dataframe
                if (dataframe_created == FALSE) {
                    mutual_exclusivity_df <- data.frame(id.1    = dataframe_row[1],
                                                        label.1 = dataframe_row[2],
                                                        id.2    = dataframe_row[3],
                                                        label.2 = dataframe_row[4],
                                                        mutual_exclusivity = mutual_exclusivity,
                                                        stringsAsFactors = FALSE)
                    dataframe_created <- TRUE

                } else {
                    # append all subsequent results to existing dataframe
                    mutual_exclusivity_df <- rbind(mutual_exclusivity_df, dataframe_row)
                }

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
