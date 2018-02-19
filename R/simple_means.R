#' @title Mean ± SEM
#' @description Calculate mean an standard error from a numeric vector and return
#' a character string.
#' @param x numeric vector.
#' @param digits Minimum number of signiicant digits past the decimal point to keep. Default: 2.
#' @param units (optional) string vector with units, Default: NULL
#' @param na.rm a logical value indicating whether NA values should be stripped
#' before the computation proceeds.
#' @return A string with the format "Mean ± SEM"
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
#' @rdname simple_mean
#' @export
simple_mean <- function(x, digits = 2, units = NULL, na.rm = FALSE){

  if(length(digits) > 1){
    digits <- digits[1]
    warning("Using only digits[1]")
  }

  if(isTRUE(na.rm)){
    smf <- function(x, digits){
      paste(signif(mean(x, na.rm = TRUE), digits),
            "\u00b1",
            signif(sqrt(stats::var(x, na.rm = TRUE)/length(stats::na.omit(x))),
                   digits))
    }
  } else{
    smf <- function(x, digits){
      paste(signif(mean(x), digits),
            "\u00b1",
            signif(sqrt(stats::var(x)/length(x)), digits))
    }
  }

  if(is.null(units)){
    smf(x, digits)
  } else{
    paste(smf(x, digits), units[1])
  }
}
