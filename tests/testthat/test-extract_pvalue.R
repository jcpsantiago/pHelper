context("test-extract_pvalue.R")

test_that("stop if vector is not character", {
  expect_error(extract_pvalue(20))
})
