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

	names <- reactiveVal()
	names(data.frame(Name=character(), Email=character(), Date=as.Date(integer(), origin='1970-01-01'),
	                 Type = factor(levels=c('Admin', 'User')),
	                 stringsAsFactors=FALSE))
	namesdt <- callModule(dtedit, 'names', names)
	
	observe({
	  print(namesdt$thedata())
	  print(namesdt$edit.count())
	})
}

##### Create the shiny UI
ui <- fluidPage(
	h3('Books'),
	dteditUI('books'),
	hr(), h3('Email Addresses'),
	dteditUI('names')
)

shinyApp(ui = ui, server = server)
