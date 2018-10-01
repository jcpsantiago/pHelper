context("test-full_mean.R")

test_that("fail when the provided vector is not character", {
  expect_error(full_mean(c(1, 1, 2)))
})

test_that("fail when no mean/group or a mean/group with length != 2", {
  expect_error(full_mean(c("mean1", "mean2"), 
                         group = c("tx1")))
  
  expect_error(full_mean(c("mean1", "mean2")))
  
  expect_error(full_mean(c("mean1"),
                         group = c("tx1", "tx2")))
  
  expect_error(full_mean(group = c("tx1", "tx2")))
})

test_that("fail if the character string is not a character vector", {
  expect_error(full_mean(c("mean1", "mean2"),
                         group = c("tx1", "tx2"),
                         comparison_string = 999))
})

test_that("correctly prints out two means with the selected string", {
  expect_equal(full_mean(c("mean1", "mean2"),
                         group = c("tx1", "tx2")),
               "mean1 for tx1 vs mean2 for tx2")
  expect_equal(full_mean(c("mean1", "mean2"),
                         group = c("tx1", "tx2"),
                         comparison_string = "compared with"),
               "mean1 for tx1 compared with mean2 for tx2")
})