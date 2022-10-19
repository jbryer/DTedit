library(devtools)

devtools::document()
devtools::build()
devtools::install()
devtools::check(cran = TRUE)

library(DTedit)

# Test with shiny app
DTedit::dtedit_demo()
