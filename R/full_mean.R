#' @title Compare two statements Mean ± SEM
#' @description Create a sentence comparing two simple means (e.g. from simple_means)
#' @param means character vector of means.
#' @param group grouping column (e.g. "treatment")
#' @return A string with the format "Mean ± SEM unit vs Mean ± SEM"
#' @examples
#' \dontrun{
#' if(interactive()){
#'  ## Basic example
#'  x <- rnorm(50 + rnorm(50))
#'  simple_mean(x, digits = 3)
#'
#'  ## With units
#'  simple_mean(iris$Petal.Length, units = "cm")
#'  }
#' }
#' @rdname full_mean
#' @export
full_mean <- function(means, group){
  
  if(!is.character(means))
    stop("Please provide means as character vector!")
  
  if((is.null(group) | length(group) != 2) |
     (is.null(means) | length(means) != 2))
    stop("means and group must have exactly length 2.")
  
  paste(means[1], "for", group[1], "vs",
        means[2], "for", group[2])
}
