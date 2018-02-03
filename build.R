library(devtools)

document()
build()
install()
check(cran = TRUE)

library(DTedit)

# Test with shiny app
DTedit::dtedit_demo()
