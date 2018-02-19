#' @title Combine two means in one sentence.
#' @description This will create a character vector with two means and a string
#' inbetween.
#' @param means A numeric or character vector with means or means +- sem.
#' @param group A two-level factor identifying the means being compared.
#' @param compared_str String to be placed between the means. Default = "vs.".
#' @return A character vector.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname print_meansFull
#' @export
compare_means <- function(means, group, compared_str = "vs."){

  stopifnot(is.factor(group), length(levels(group)) == 2)

  paste(means[group == levels(group)[1]],
        compared_str,
        means[group == levels(group)[2]])

}
