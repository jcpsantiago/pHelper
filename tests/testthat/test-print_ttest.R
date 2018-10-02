context("test-print_ttest")

tidy_ttest_lowp <- readRDS(test_path("test_data", "tidy_ttest_lowp.rds"))
tidy_ttest_highp <- readRDS(test_path("test_data", "tidy_ttest_highp.rds"))


test_that("fail if x is not a data.frame", {
  expect_error(print_ttest("not a data.frame"))
  expect_error(print_ttest(list(a = "a", b = "b")))
})

test_that("prints correctly with default settings", {
  expect_equal(print_ttest(tidy_ttest_lowp, ci = TRUE, ptype = "full"),
               "t(95)=0.66, 95% CI [0.52, 0.8], p=2.5e-15")
  expect_equal(print_ttest(tidy_ttest_highp),
               "t(75)=-1.1, 95% CI [-1.1, -1], p=0.85")
})

test_that("prints correctly with cutoff", {
  expect_equal(print_ttest(tidy_ttest_lowp, ptype = "cutoff"),
               "t(95)=0.66, 95% CI [0.52, 0.8], p<0.001")
  expect_equal(print_ttest(tidy_ttest_highp, ptype = "cutoff"),
                           "t(75)=-1.1, 95% CI [-1.1, -1], p=0.85")
})

test_that("confidence intervals can be disabled correctly", {
  expect_equal(print_ttest(tidy_ttest_lowp, ci = FALSE),
               "t(95)=0.66, p=2.5e-15")
  expect_equal(print_ttest(tidy_ttest_lowp, ci = FALSE, ptype = "cutoff"),
               "t(95)=0.66, p<0.001")
})
