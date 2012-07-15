#' List of the biggest Mediawiki sites
#'
#' This function is like to disappear in future versions.
#'
#' @return data.frame 
#' @docType data
#' @references 
#' \url{http://s23.org/wikistats/}
#' @export
wikisites <- function() {
    uri <- "http://s23.org/wikistats/largest_csv.php?sort=good_desc,total_desc&th=1000&lines=46698"
    res <- read.csv(uri, stringsAsFactors=FALSE)
    res$ts <- as.POSIXct(res$ts)
    return(res)
}

