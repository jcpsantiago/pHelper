#' @title Extract p-values from a string.
#' @description Given a character string, this function will extract the p-value
#' @param x a character string containing a p-value
#' @param full include "p=" or just the number, Default: TRUE
#' @return a character string
#' @examples
#' \dontrun{
#' if(interactive()){
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_extract}}
#' @rdname extract_pvalue
#' @export
#' @importFrom stringr str_extract
extract_pvalue <- function(x, full = TRUE){

  if(!is.character(x))
    stop("You can only extract p-values from character strings!")

  if(isTRUE(full)){
    stringr::str_extract(x,
                         "(p|P)(=|\\s=\\s)0\\.[0-9]+")
  } else{
    stringr::str_extract(extract_pvalue(x), "0\\.[0-9]+")
  }
}
