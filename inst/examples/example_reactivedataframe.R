library(shiny)
library(DTedit)

##### Callback functions.


data.update.callback <- function(data, olddata, row) {
	return(data)
}

data.insert.callback <- function(data, row) {
	return(data)
}

data.delete.callback <- function(data, row) {
	data <- data[-row,, drop = FALSE]
	return(data)
}

##### Create the Shiny server
server <- function(input, output) {

	data <- reactiveVal()
	data(data.frame(a = c("Apple", "Cherry"), b = c("Pie", "Tart"), stringsAsFactors = FALSE))
	data_DT_gui <- callModule(dtedit, 'data',
				  thedataframe = data,
				  edit.cols = c("a", "b"),
				  callback.delete = data.delete.callback,
				  callback.insert = data.insert.callback,
				  callback.update = data.update.callback
	)

	observe({
		data(as.data.frame(data_DT_gui$thedata(), stringsasfactors = FALSE))
		print(isolate(data()))
		print(paste("Edit count:", data_DT_gui$edit.count()))
	})

	observeEvent(input$data_scramble, {
#		extra_email <- data.frame( # create random user
#			Name = paste(first[sample(1:length(first), 1)], second[sample(1:length(second), 1)]),
#			Email = paste0(do.call(paste0, replicate(sample(5:8, 1), sample(tolower(LETTERS), 1, TRUE), FALSE)),
#				       '@',sample(email, 1)),
#			Date = as.Date(Sys.Date()-sample(1:1000, 1), origin = "1970-01=01"),
#			Type = factor(sample(names.Types(), 1), levels = names.Types()),
#			Like = I(list(factor(sample(names.Likes(), sample(1:length(names.Likes()), 1)),
#					     levels = names.Likes()))),
#			stringsAsFactors = FALSE
#		)
#		data(data.frame(rbind(names(), extra_email), stringsAsFactors = FALSE))
		print("Scrambling...")
	})
}

##### Create the shiny UI
ui <- fluidPage(
	dteditUI('data'),
	actionButton("data_scramble", "Scramble an entry")
)

shinyApp(ui = ui, server = server)
