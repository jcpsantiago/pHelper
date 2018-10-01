context("test-print_ttest")

df <- iris %>% dplyr::filter(Species != "virginica")

test_that("fail if x is not a data.frame", {
  expect_error(print_ttest("not a data.frame"))
  expect_error(print_ttest(list(a = "a", b = "b")))
})
