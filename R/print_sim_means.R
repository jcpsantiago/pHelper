#' @title Mean ± SEM
#' @description Calculate mean an standard error from a numeric vector and return
#' a character string.
#' @param x numeric vector.
#' @param units (optional) string vector with units, Default: NULL
#' @param ... further arguments to be passed to mean() and var().
#' @return A string with the format "Mean ± SEM"
#' @examples
#' \dontrun{
#' if(interactive()){
#'  ## Basic example
#'  x <- rnorm(50 + rnorm(50))
#'  simple_mean(x, digits = 3)
#'
#'  ## With units
#'  iris %>%
#'    dplyr::mutate(units = "cm") %>%
#'    dplyr::group_by(Species) %>%
#'    dplyr::summarise(mean_petal_length = simple_mean(Petal.Length, units))
#'  }
#' }
#' @seealso
#'  \code{\link[broman]{myround}}
#' @rdname print_meanSimple
#' @export
#' @importFrom broman myround
simple_mean <- function(x, units = NULL, ..., digits = 2){

  m <- mean(x, ...)
  u <- units[1]
  se <- sqrt(var(x, ...)/length(x))

  if(is.null(units)){
    paste(broman::myround(m, digits),
          "\u00b1",
          broman::myround(se, digits))
  } else{
    paste(broman::myround(m, digits),
          "\u00b1",
          broman::myround(se, digits),
          u)
  }
}
