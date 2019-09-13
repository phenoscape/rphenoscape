#' Returns the date of the current release of the Phenoscape Knowledgebase (KB) and
#' counts of annotated matrices, taxa, phenotypes, characters, and states.
#' 
#' @param
#' @return Summary counts of annotations in the Phenoscape KB
#' @examples
#'
get_KBinfo <- function() {
  request <- GET("https://kb.phenoscape.org/api/kb/annotation_summary")
  response <- content(request, as = "text", encoding = "UTF-8")
  summary <- fromJSON(response)
  do.call("rbind", summary)
}

# I prefer to see the output formatted using the following line -- ok to add ? Or is there a different print format that's preferred?
# cat("\nAnnotated taxa:", (summary$annotated_taxa[1]), "\nAnnotated characters:", (summary$annotated_characters[1]), "\nAnnotated states:", (summary$annotated_states[1]), "\nBuild date and time:", (summary$build_time[1]), "\n", "\n")

