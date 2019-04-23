library(shiny)
library(RSQLite)
source("../../R/dtedit.R")

##### Load books data.frame as a SQLite database
conn <- dbConnect(RSQLite::SQLite(), "books.sqlite")

if(!'books' %in% dbListTables(conn)) {
	books <- read.csv('books.csv', stringsAsFactors = FALSE)
	books$Authors <- strsplit(books$Authors, ';')
	books$Authors <- lapply(books$Authors, trimws) # Strip white space
	books$Authors <- unlist(lapply(books$Authors, paste0, collapse = ';'))
	books$id <- 1:nrow(books)
	books$Date <- paste0(books$Date, '-01-01')
	dbWriteTable(conn, "books", books, overwrite = TRUE)
}

getBooks <- function() {
	res <- dbSendQuery(conn, "SELECT * FROM books")
	books <- dbFetch(res)
	dbClearResult(res)
	books$Authors <- strsplit(books$Authors, ';')
	books$Date <- as.Date(books$Date)
	books$Publisher <- as.factor(books$Publisher)
	return(books)
}

##### Callback functions.
books.insert.callback <- function(data, row) {
	query <- paste0("INSERT INTO books (id, Authors, Date, Title, Publisher) VALUES (",
					"", max(getBooks()$id) + 1, ", ",
					"'", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
					"'", as.character(data[row,]$Date), "', ",
					"'", data[row,]$Title, "', ",
					"'", as.character(data[row,]$Publisher), "' ",
					")")
	print(query) # For debugging
	dbSendQuery(conn, query)
	return(getBooks())
}

books.update.callback <- function(data, olddata, row) {
	query <- paste0("UPDATE books SET ",
					"Authors = '", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
					"Date = '", as.character(data[row,]$Date), "', ",
					"Title = '", data[row,]$Title, "', ",
					"Publisher = '", as.character(data[row,]$Publisher), "' ",
					"WHERE id = ", data[row,]$id)
	print(query) # For debugging
	dbSendQuery(conn, query)
	return(getBooks())
}

books.delete.callback <- function(data, row) {
	query <- paste0('DELETE FROM books WHERE id = ', data[row,]$id)
	dbSendQuery(conn, query)
	return(getBooks())
}

##### Create the Shiny server
server <- function(input, output) {
	books <- getBooks()
	callModule(dtedit, 'books',
		   thedataframe = books,
		   edit.cols = c('Title', 'Authors', 'Date', 'Publisher'),
		   edit.label.cols = c('Book Title', 'Authors', 'Publication Date', 'Publisher'),
		   input.types = c(Title='textAreaInput'),
		   input.choices = list(Authors = unique(unlist(books$Authors))),
		   view.cols = names(books)[c(5,1,3)],
		   callback.update = books.update.callback,
		   callback.insert = books.insert.callback,
		   callback.delete = books.delete.callback)

	
	names.Type <- reactiveVal()
	names.Type(data.frame(Types = c("Admin", "User"), stringsAsFactors = FALSE))
	names.Typedt <- callModule(dtedit, 'names.Type',
	                           thedataframe = names.Type,
	                           edit.cols = c("Types"),
	                           input.types = c(Types = "textAreaInput"),
	                           view.cols = c("Types"))

	names.Types <- reactiveVal(isolate(names.Type()$Types))

	names <- reactiveVal()
	names(data.frame(Name=character(), Email=character(), Date=as.Date(integer(), origin='1970-01-01'),
	                 Type = isolate(factor(character(), levels = names.Types())),
	                 stringsAsFactors=FALSE))
	
	observe({
	        print(names.Typedt$thedata())
	        names.Types(names.Typedt$thedata()$Types)
	        isolate({
	                print(names.Types())
	                temp <- droplevels(names()) # names() factor levels cannot be altered directly
	                levels(temp$Type) <- names.Types()
	                names(temp)
	        })
	})
	
	namesdt <- callModule(dtedit, 'names', 
	                      thedataframe = names,
	                      input.types = c(Type = "selectInputReactive"),
	                      input.choices = c(Type = "names.Types"),
	                      input.choices.reactive = list(names.Types = names.Types)
	                      )
	
	observe({
	  print(namesdt$thedata())
	  names(as.data.frame(namesdt$thedata(), stringsasfactors = FALSE))
	  print(paste("Edit count:", namesdt$edit.count()))
	})
	
	observeEvent(input$email_clean,{
	  names(names()[0,]) # empty the dataframe
	})
	
	observeEvent(input$email_add, {
	  first <- c("April", "Bob", "Charles", "Deborah", "Elle", "Francis", "Grace", "Horace", "Indigo", "Jan", "Kel")
	  second <- c("Zartus", "Yelland", "Xeron", "Wells", "Vorostek", "Ursida", "Tellus", "Smith", "Rose", "Quentin")
	  email <- c("hotmail.com", "yahoo.com", "gmail.com", "outlook.com", "github.com", "bigpond.com", "medscape.com")
	  extra_email <- data.frame( # create random user
	    Name = paste(first[sample(1:length(first), 1)], second[sample(1:length(second), 1)]),
	    Email = paste0(do.call(paste0, replicate(sample(5:8, 1), sample(tolower(LETTERS), 1, TRUE), FALSE)),
	                   '@',sample(email, 1)),
	    Date = as.Date(Sys.Date()-sample(1:1000, 1), origin = "1970-01=01"),
	    Type = factor(sample(names.Types(), 1), levels = names.Types()),
	    stringsAsFactors = FALSE
	  )
	  names(data.frame(rbind(names(), extra_email), stringsAsFactors = FALSE))
	})
}

##### Create the shiny UI
ui <- fluidPage(
  tabsetPanel(
    type = "tabs",
    tabPanel("Books",
             h3('Books'),
             dteditUI('books')
    ),
    tabPanel("Emails",
             tabsetPanel(
                     type = "tabs",
                     tabPanel("Users",
                              h3('Email Addresses'),
                              dteditUI('names')
                     ),
                     tabPanel("Add/Delete",
                              wellPanel(
                                      actionButton("email_add", "Add an email entry"),
                                      actionButton("email_clean", "Delete entire email list")
                              )
                     ),
                     tabPanel("User Types",
                              wellPanel(
                                      dteditUI("names.Type")
                              )
                
                     )
             )
    )
  )
)

shinyApp(ui = ui, server = server)
