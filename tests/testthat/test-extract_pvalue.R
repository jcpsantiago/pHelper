context("test-extract_pvalue.R")

test_that("correctly extracts p-value", {
  expect_equal(extract_pvalue("P = 0.0034"), "P = 0.0034")
  )
})
