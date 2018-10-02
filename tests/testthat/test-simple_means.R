context("test-print_sim_means.R")

set.seed(1234)
x <- rnorm(50 + rnorm(50))
x_mis <- x
x_mis[1] <- NA

df <- dplyr::mutate(iris, unit = "cm",
                          digits = 2,
                          moredigits = 0)


test_that("fail when the provided vector is not numeric", {
  expect_error(simple_mean(c("a", "b", "c")))
})

test_that("units are given as a character vector", {
  expect_error(simple_mean(x, units = c(2, 4)))
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


test_that("check the function is printing correctly", {
  expect_equal(simple_mean(x), paste("0.14", "\u00b1", "0.15"))
  expect_equal(simple_mean(x_mis, na.rm = TRUE), paste("0.18", "\u00b1", "0.14"))
})

