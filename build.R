library(devtools)

document()
build()
install()
check()

library(DTedit)

# Test with shiny app
DTedit::dtedit_demo()
