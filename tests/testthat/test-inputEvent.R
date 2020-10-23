# shinyTest with covr code by Ruben Faelens
#  https://github.com/rfaelens/exampleShinyTest/

ensurePackagePresent <- function(pkgName = "DTedit", quiet=TRUE) {
  tmp_lib <- tempfile("R_LIBS")
  dir.create(tmp_lib)

  ## Is the library available on the current search path?
  ## If so, it probably was installed by covr or shinytest or ...
  ## If it is in R_LIBS_USER, then we refuse: this was installed
  ## through Build and Install, and is an old version!
  ## If no good version is available, then install the library
  ## in a temporary path and return this path
  pkgPat <- path.package(pkgName, quiet = T)
  if (is.null(pkgPat) || pkgName %in% devtools::dev_packages() ||
      startsWith(pkgPat, Sys.getenv("R_LIBS_USER")) ) {
    if (!quiet) message("Package ", pkgName, appendLF = F)
    if (!quiet && is.null(pkgPat)) message(" is not available. ", appendLF = F)
    if (!quiet && pkgName %in% devtools::dev_packages())
      message(" is loaded as dev package. ", appendLF = F)
    if (!quiet && pkgName %in% devtools::dev_packages())
      message(" is available in R_LIBS_USER. ", appendLF = F)
    if (!quiet) message("Installing new version...")
    #Either the package is not available
    #or it is currently in the dev packages
    #or we are about to load the (outdated) installed version!
    message("Installing temporary version of ", pkgName)
    pkg <- devtools::as.package(".")
    stopifnot(pkg$package == pkgName)
    utils::install.packages(repos = NULL,
                            lib = tmp_lib,
                            pkg$path,
                            type = "source",
                            INSTALL_opts = c(#"--example",
                              #"--install-tests",
                              "--with-keep.source",
                              "--with-keep.parse.data",
                              "--no-docs",
                              "--no-html",
                              "--no-lock",
                              "--no-help", "--no-demo", "--no-exec",
                              "--data-compress=none",
                              "--no-byte-compile", "--no-staged-install",
                              "--no-test-load",
                              "--no-multiarch"),
                            quiet = quiet)
  } else {
    if (!quiet)
      message(
        "Package ",
        pkgName,
        " is available in temporary library, not installing new version..."
      )
  }
  return(tmp_lib)
}

tmp_lib <- ensurePackagePresent()
context("inputEvent") # note that context is not encouraged for testthat >= 2.1
testthat::test_that("inputEvent", {
  testthat::skip_on_cran()
  # skip_on_cran relies on NOT_CRAN environment variable to be TRUE
  # https://github.com/r-lib/covr/issues/314
  testthat::skip_on_travis()

  results <- withr::with_libpaths(tmp_lib, {
    shinytest::testApp(appDir = testthat::test_path("../inputEvent"),
                       compareImages = FALSE)
  }, action = "prefix")
  shinytest::expect_pass(results)
})
