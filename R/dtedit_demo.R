#' Run a shiny app showing how the DTedit function works.
#'
#' @export
dtedit_demo <- function() {
	dir <- find.package('DTedit')
	shiny::runApp(paste0(dir, '/inst/shiny'))
}
