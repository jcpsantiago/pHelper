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
simple_mean <- function(x, digits = 2, units = "", na.rm = FALSE) {
  if (!is.numeric(x)) {
    stop("Please provide a numeric vector!")
  }

  if (length(digits) > 1) {
    digits <- digits[1]
    warning("Using only digits[1]")
  }

  if (!is.character(units)) {
    stop("Please provide units as a character vector.")
  }

  if (isTRUE(na.rm)) {
    sim_mean <- paste(
      signif(mean(x, na.rm = TRUE), digits),
      "\u00b1",
      signif(
        sqrt(stats::var(x, na.rm = TRUE) / length(stats::na.omit(x))),
        digits
      ), units
    )
  } else {
    sim_mean <- paste(
      signif(mean(x), digits),
      "\u00b1",
      signif(sqrt(stats::var(x) / length(x)), digits), units
    )
  }
  
  ## trim white space, in case no units are given
  stringr::str_trim(sim_mean, "right")
}
