context("test-extract_pvalue.R")

test_that("fails if something else besides character is given", {
  expect_error(extract_pvalue(20))
})

test_that("extracts p-value, no matter the original formatting", {
  
  expect_equal(extract_pvalue("significant with p=0.01"), "p=0.01")
  expect_equal(extract_pvalue("significant with P=0.01"), "P=0.01")
  expect_equal(extract_pvalue("significant with p = 0.01"), "p = 0.01")
  expect_equal(extract_pvalue("significant with P = 0.01"), "P = 0.01")
  
  expect_equal(extract_pvalue("significant with p=0.01", full = FALSE), "0.01")
  expect_equal(extract_pvalue("significant with P=0.01", full = FALSE), "0.01")
  expect_equal(extract_pvalue("significant with p = 0.01", full = FALSE), "0.01")
  expect_equal(extract_pvalue("significant with P = 0.01", full = FALSE), "0.01")
})
