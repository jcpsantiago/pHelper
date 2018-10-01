context("test-print_sim_means.R")
require(dplyr)

x <- rnorm(50 + rnorm(50))
df <- iris %>%
  dplyr::mutate(unit = "cm",
                digits = 2,
                moredigits = 0)

test_that("fail when the provided vector is not numeric", {
  expect_error(simple_mean(c("a", "b", "c")))
})

test_that("output is a character vector", {

  sm <- simple_mean(x)

  expect_is(object = sm, class = "character")
})

test_that("warns when vector length > 1", {

  expect_warning(
    simple_mean(df$Sepal.Length, digits = df$digits)
  )

  expect_warning(
    simple_mean(df$Sepal.Length, digits = df$moredigits)
  )

})

