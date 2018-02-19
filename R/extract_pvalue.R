#' @title Extract p-values from a string.
#' @description Given a character string, this function will extract
#' @param x PARAM_DESCRIPTION
#' @param full PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
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

  stopifnot(is.character())

  if(isTRUE(full)){
    stringr::str_extract(x,
                         "p=0.[0-9]+ | p = 0.[0-9]+ | P = 0.[0-9]+ | p=0.[0-9]+")
  } else{
    stringr::str_extract(extract_pvalue(x), "0.[0-9]+")
  }
}
