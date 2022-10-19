#' Shiny UI Module version of DTedit
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'        server function.
#' @seealso DTedit::dtedit_server
#' @export
dtedit_ui <- function(id) {
	ns <- NS(id)
	uiOutput(ns(id))
}

#' Shiny Server Module version of DTedit
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'        UI function.
#' @param ... other parameters passed to [DTedit::dtedit].
#' @seealso DTedit::dtedit_ui
#' @export
dtedit_server <- function(id, ...) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			return(dtedit(input = input,
						  output = output,
						  name = id,
						  id = id,
						  ...))
		}
	)
}
