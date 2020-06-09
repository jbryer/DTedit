context("test-dtedit")
# this file is for testing the applications in the inst/ directory

library(shinytest)

testthat::test_that("sample simple app", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # use compareImages = FALSE
  appdir <- system.file(package = "DTedit", "examples/simple")
  shinytest::expect_pass(shinytest::testApp(appdir, compareImages = FALSE))
})
