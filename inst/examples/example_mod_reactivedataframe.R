##### Minimal DTedit example using reactive dataframe #####
\donttest{library(shiny)
library(DTedit)

##### Create the Shiny server #####
server <- function(input, output) {
	
	data <- reactiveVal() # # 'data' will be a 'reactive' dataframe
	data(data.frame(Column1 = c("Apple", "Cherry", "Frozen"),
			Column2 = c("Pie", "Tart", "Yoghurt"),
			stringsAsFactors = FALSE))
	data_DT_gui <- callModule(
	  dteditmod,
	  'dataspace',
	  thedata = data, 
	  edit.cols = c("Column1", "Column2")
	)
	
	observe({
		data(isolate(as.data.frame(data_DT_gui$thedata(), stringsasfactors = FALSE)))
		print(isolate(data()))
		print(paste("Edit count:", data_DT_gui$edit.count())) 
		# only reacts to change in $edit.count()
	})
	
	observeEvent(input$data_scramble, {
		print("Scrambling...")
		temp <- data()
		if (nrow(temp)>0) {
			row <- sample(1:nrow(temp), 1)  # row
			col <- sample(1:2, 1)           # column
			temp[row, col] <- paste(sample(unlist(strsplit(temp[row, col], "")),
						       nchar(temp[row, col])),
						sep = '', collapse = '')
			data(temp) # adjusted dataframe 'automatically' read by DTedit
		}
	})
}

##### Create the shiny UI ######
ui <- fluidPage(
	h3("DTedit using reactive dataframe"),
	wellPanel(p("Try the 'Scramble' button!")),
	dteditmodUI("dataspace"),
	actionButton("data_scramble", "Scramble an entry")
)

shinyApp(ui = ui, server = server)
}