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
	  # 'data' contains the dataframe *after* the row has been inserted/added
	  # 'row' is the row number where data has been inserted
		mydata <<- rbind(mydata, data[row,])
		# in this case, 'mydata' should just be the same as 'data'
		return(mydata)
	}

	my.update.callback <- function(data, olddata, row) {
	  # 'data' contains the dataframe *after* the row has been updated
	  # 'row' is the row number where data has been updated
	  # 'olddata' is the previous version of the data
		mydata[row,] <<- data[row,]
		# in this case, 'mydata' should just be the same as 'data'
		return(mydata)
	}

	my.delete.callback <- function(data, row) {
	  # 'data' contains the dataframe *before* the row has been deleted
	  # 'row' is the row number where data is to be deleted
		mydata <<- mydata[-row,]
		# in this case, 'mydata' should just be the same as data[-c(row),]
		return(mydata)
	}

	##### Create the DTedit object
	DTedit::dtedit(
	  input, output,
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
