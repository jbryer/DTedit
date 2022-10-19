library(shiny)
library(DTedit)

##### Create the Shiny server
server <- function(input, output) {
	mydata <- data.frame(name = character(),
						 email = character(),
						 useR = factor(levels = c('Yes', 'No')),
						 notes = character(),
						 stringsAsFactors = FALSE)

	##### Callback functions.
	my.insert.callback <- function(data, row) {
		mydata <- rbind(data, mydata)
		return(mydata)
	}

	my.update.callback <- function(data, olddata, row) {
		mydata[row,] <- data[1,]
		return(mydata)
	}

	my.delete.callback <- function(data, row) {
		mydata <- mydata[-row,]
		return(mydata)
	}

	##### Create the DTedit object
	DTedit::dtedit(input, output,
		   name = 'mycontacts',
		   thedata = mydata,
		   edit.cols = c('name', 'email', 'useR', 'notes'),
		   edit.label.cols = c('Name', 'Email Address', 'Are they an R user?', 'Additional notes'),
		   input.types = c(notes='textAreaInput'),
		   view.cols = c('name', 'email', 'useR'),
		   callback.update = my.update.callback,
		   callback.insert = my.insert.callback,
		   callback.delete = my.delete.callback)
}

##### Create the shiny UI
ui <- fluidPage(
	h3('DTedit Template'),
	uiOutput('mycontacts')
)

##### Start the shiny app
shinyApp(ui = ui, server = server)
