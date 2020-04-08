context("Check utilities functions")

test_that("co2_convertion_median function returns single numeric value", {
  output <- co2_convertion_median(1:200)
  testthat::expect_is(output, "numeric")
  testthat::expect_equal(length(output), 1)
})

test_that("co2_convertion function returns numeric vector", {
  input <- 1:200
  output <- co2_convertion(input)
  testthat::expect_is(output, "numeric")
  testthat::expect_equal(length(output), length(input))

})