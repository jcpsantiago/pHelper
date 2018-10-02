#' @title Format t-test results
#' @description Taking tidy results from a t-test, this function will create a
#' string ready for you to use in your paper.
#' @param tidy_ttest Tidy data-frame with t-test results from broom::tidy(t.test()).
#' @param digits Number of significant digits.
#' @param ci Should confidence intervals be printed? Default: TRUE.
#' @param ptype Should p-values be presented in full or with a cutoff?
#' @return A character vector.
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  }
#' }
#' @rdname print_ttest
#' @importFrom dplyr case_when
#' @export
print_ttest <- function(tidy_ttest, digits = 2, ci = TRUE, ptype = "full"){

  stopifnot(is.data.frame(tidy_ttest))

  ## set correct type of p-value to present
  if(ptype == "cutoff"){
    pvalue <- dplyr::case_when(
      tidy_ttest$p.value < 0.01 & tidy_ttest$p.value > 0.001 ~ "<0.01",
      tidy_ttest$p.value < 0.001 ~ "<0.001",
      TRUE ~ paste0("=", signif(tidy_ttest$p.value, digits))
    )
  } else if(ptype == "full"){
    pvalue <- paste0("=", signif(tidy_ttest$p.value, digits))
  }

  ## Save t-value, df and p-value
  t <-paste0("t(", signif(tidy_ttest$parameter, digits), ")=",
             signif(tidy_ttest$estimate, digits))

  p <- paste0("p", pvalue)

  ## Save the final string with, or without confidence interval
  if(isTRUE(ci)){
    paste0(t, ", ",
           "95% CI [", signif(tidy_ttest$conf.low, digits), ", ",
           signif(tidy_ttest$conf.high, digits), "]", ", ",
           p)
  } else{
    paste0(t, ", ", p)
  }
}
