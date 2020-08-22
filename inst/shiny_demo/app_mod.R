library(shiny)
library(RSQLite)
library(DTedit)

##### Book module ########################################################

books.ModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dteditmodUI(ns('books'))
  )
}

books.Module <- function(input, output, session) {

  ##### Load books data.frame as a SQLite database
  conn <- dbConnect(RSQLite::SQLite(), "books.sqlite")

  if (!'books' %in% dbListTables(conn) || isTRUE(getOption("shiny.testmode"))) {
    # the sqlite file doesn't have the right data
    # OR we are running in test mode (test mode -> reset the data)
    books <- read.csv('books.csv', stringsAsFactors = FALSE)
    books$Authors <- strsplit(books$Authors, ';')
    books$Authors <- lapply(books$Authors, trimws) # Strip white space
    books$Authors <- unlist(lapply(books$Authors, paste0, collapse = ';'))
    books$id <- 1:nrow(books) # can also use 'seq_length(nrow(books))'
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

  books <- getBooks()

  ##### Callback functions.
  books.insert.callback <- function(data, row) {
    query <- paste0(
      "INSERT INTO books (id, Authors, Date, Title, Publisher) VALUES (",
      "", max(getBooks()$id) + 1, ", ",
      "'", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
      "'", as.character(data[row,]$Date), "', ",
      "'", data[row,]$Title, "', ",
      "'", as.character(data[row,]$Publisher), "' ",
      ")"
    )
    print(query) # For debugging
    res <- dbSendQuery(conn, query)
    dbClearResult(res)
    return(getBooks())
  }

  books.update.callback <- function(data, olddata, row) {
    query <- paste0(
      "UPDATE books SET ",
      "Authors = '", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
      "Date = '", as.character(data[row,]$Date), "', ",
      "Title = '", data[row,]$Title, "', ",
      "Publisher = '", as.character(data[row,]$Publisher), "' ",
      "WHERE id = ", data[row,]$id
    )
    print(query) # For debugging
    res <- dbSendQuery(conn, query)
    dbClearResult(res)
    return(getBooks())
  }

  books.delete.callback <- function(data, row) {
    query <- paste0('DELETE FROM books WHERE id = ', data[row,]$id)
    res <- dbSendQuery(conn, query)
    dbClearResult(res)
    return(getBooks())
  }

  booksdt <- callModule(
    dteditmod,
    'books',
    thedata = books,
    edit.cols = c('Title', 'Authors', 'Date', 'Publisher'),
    edit.label.cols = c(
      'Book Title', 'Authors', 'Publication Date', 'Publisher'
    ),
    input.types = c(Title = 'textAreaInput'),
    input.choices = list(Authors = unique(unlist(books$Authors))),
    view.cols = names(books)[c(5,1,3)],
    callback.update = books.update.callback,
    callback.insert = books.insert.callback,
    callback.delete = books.delete.callback
  )

  return(
    list(
      thedata = reactive({booksdt$thedata}),
      conn = conn # the database connection
    )
  )
}

##### Names module #################################################################

names.ModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dteditmodUI(ns('names'))
  )
}

names.Module <- function(
  input, output, session,
  names.Types, names.Likes,
  clean, email_add
) {

  names <- reactiveVal()
  names(
    data.frame(
      Name = character(), Email = character(),
      Date = as.Date(integer(), origin = '1970-01-01'),
      Type = isolate(factor(character(), levels = names.Types())),
      Like = character(),
      # Like = I(list(isolate(factor(character(), levels = names.Likes())))),
      stringsAsFactors = FALSE
    )
  )

  observeEvent(clean(),{
    names(names()[0,]) # empty the dataframe
  })

  observeEvent(email_add(), {
    first <- c("April", "Bob", "Charles", "Deborah", "Elle",
               "Francis", "Grace", "Horace", "Indigo", "Jan", "Kel")
    second <- c("Zartus", "Yelland", "Xeron", "Wells", "Vorostek",
                "Ursida", "Tellus", "Smith", "Rose", "Quentin")
    email <- c("hotmail.com", "yahoo.com", "gmail.com", "outlook.com",
               "github.com", "bigpond.com", "medscape.com")
    extra_email <- data.frame( # create random user
      Name = paste(
        first[sample(seq_len(length(first)), 1)],
        second[sample(seq_len(length(second)), 1)]
      ),
      Email = paste0(
        do.call(
          paste0,
          replicate(
            sample(5:8, 1),
            sample(tolower(LETTERS), 1, TRUE),
            FALSE
          )
        ),
        '@',sample(email, 1)
      ),
      Date = as.Date(Sys.Date() - sample(1:1000, 1), origin = "1970-01=01"),
      Type = factor(sample(names.Types(), 1), levels = names.Types()),
      Like = I(
        list(
          factor(sample(names.Likes(),
                        sample(seq_len(length(names.Likes())), 1)),
                 levels = names.Likes()
          )
        )
      ),
      stringsAsFactors = FALSE
    )
    names(data.frame(rbind(names(), extra_email), stringsAsFactors = FALSE))
  })

  namesdt <- callModule(
    dteditmod,
    'names',
    thedata = names,
    input.types = c(
      Type = "selectInputReactive",
      Like = "selectInputMultipleReactive"
    ),
    input.choices = c(Type = "names.Types", Like = "names.Likes"),
    input.choices.reactive = list(
      names.Types = names.Types,
      names.Likes = names.Likes
    )
  )

  return(
    list(
      thedata = reactive({namesdt$thedata}),
      edit.count = reactive({namesdt$edit.count})
    )
  )
}

##### Name Types module ############################################################

names.Type.ModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dteditmodUI(ns('names.Type'))
  )
}

names.Type.Module <- function(input, output, session, names) {

  names.Type.update.callback <- function(data, olddata, row) {
    # update a user-type
    # do not allow an updated user-type which is the same as another
    ## if this is attempted, show a warning
    if (data[row,] %in% data[-row,]) {
      stop(paste0(
        "Cannot change user-type to '", data[row,],
        "', that user-type already exists!"
      ))
    }
    return(data)
  }

  names.Type.insert.callback <- function(data, row) {
    # insert a user-type
    # do not allow a new user-type which is the same as an old one
    ## if this is attempted, show a warning
    if (data[row,] %in% data[-row,]) {
      stop(paste0(
        "Cannot add '", data[row,],"', that user-type already exists!"
      ))
    }
    return(data)
  }

  names.Type.delete.callback <- function(data, row) {
    # remove a user-type
    # it is possible for this user-type to be currently used
    # by an entry in names()  (names has been explicitly passed by reference
    # to the dtedit function), in which case this function will show a warning
    if (data[row,] %in%
        get("input.choices.reactive", parent.frame())[["names"]]()$Type) {
      stop(paste0(
        "Cannot delete '", data[row,],
        "', this user-type currently assigned to a user."))
    } else {
      data <- data[-row,, drop = FALSE]
    }
    return(data)
  }

  names.Type <- reactiveVal()
  names.Type(data.frame(Types = c("Admin", "User"), stringsAsFactors = FALSE))
  names.Typedt <- callModule(
    dteditmod,
    'names.Type',
    thedata = names.Type,
    edit.cols = c("Types"),
    input.types = c(Types = "textAreaInput"),
    view.cols = c("Types"),
    input.choices.reactive = list(names = names),
    # names is never used as an input, but will be checked
    # during the callback.delete
    callback.delete = names.Type.delete.callback,
    callback.insert = names.Type.insert.callback,
    callback.update = names.Type.update.callback
  )

  return(list(thedata = reactive({names.Typedt$thedata})))
}

##### Names Likes module #######################################################

names.Like.ModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dteditmodUI(ns('names.Like'))
  )
}

names.Like.Module <- function(input, output, session, names) {
  names.Like.update.callback <- function(data, olddata, row) {
    # update a like
    # do not allow an updated like which is the same as another
    ## if this is attempted, show a warning
    if (data[row,] %in% data[-row,]) {
      stop(paste0(
        "Cannot change like to '", data[row,],
        "', that like already exists!"
      ))
    }
    return(data)
  }

  names.Like.insert.callback <- function(data, row) {
    # insert a like
    # do not allow a like which is the same as an old one
    ## if this is attempted, show a warning
    if (data[row,] %in% data[-row,]) {
      stop(paste0("Cannot add '", data[row,],"', that like already exists!"))
    }
    return(data)
  }

  names.Like.delete.callback <- function(data, row) {
    # remove a like
    # it is possible for this like to be currently used
    # by an entry in names()  (names has been explicitly passed by reference
    # to the dtedit function), in which case this function will show a warning
    if (data[row,] %in%
        unlist(get("input.choices.reactive", parent.frame())[["names"]]()$Like
        )) {
      stop(paste0("Cannot delete '", data[row,],
                  "', this like currently assigned to a user."))
    } else {
      data <- data[-row,, drop = FALSE]
    }
    return(data)
  }

  names.Like <- reactiveVal()
  names.Like(data.frame(Likes = c("Apple", "Pear"), stringsAsFactors = FALSE))
  names.Likedt <- callModule(
    dteditmod,
    'names.Like',
    thedata = names.Like,
    edit.cols = c("Likes"),
    input.types = c(Likes = "textAreaInput"),
    view.cols = c("Likes"),
    input.choices.reactive = list(names = names),
    # names is never used as an input, but will be checked
    # during the callback.delete
    callback.delete = names.Like.delete.callback,
    callback.insert = names.Like.insert.callback,
    callback.update = names.Like.update.callback
  )

  return(list(thedata = reactive({thedata = names.Likedt$thedata})))
}

##### Create the Shiny server
server <- function(input, output, session) {

  clean <- reactive({input$email_clean})
  email_add <- reactive({input$email_add})

  books.values <- shiny::callModule(books.Module, 'myBooks')

  names <- reactiveVal()
  names.Type.values <- shiny::callModule(names.Type.Module, 'my.names.Type', names)
  names.Like.values <- shiny::callModule(names.Like.Module, 'my.names.Like', names)
  names.values <- shiny::callModule(
    names.Module, 'myNames',
    reactive({names.Type.values$thedata()$Types}),
    reactive({names.Like.values$thedata()$Likes}),
    clean, email_add
  )

  observe({
    print(names.values$thedata())
    names(as.data.frame(names.values$thedata(), stringsasfactors = FALSE))
    print(paste("Edit count:", names.values$edit.count()))
  })

  data_list <- list() # exported list for shinytest
  shiny::observeEvent(books.values$thedata(), {
    data_list[[length(data_list) + 1]] <<- books.values$thedata()
  })
  shiny::exportTestValues(data_list = {data_list})

  session$onSessionEnded(function() {
    dbDisconnect(books.values$conn)
  })
}

##### Create the shiny UI
ui <- fluidPage(
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Books",
      h3('Books'),
      books.ModuleUI('myBooks')
    ),
    tabPanel(
      "Emails",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Users",
          h3('Email Addresses'),
          names.ModuleUI('myNames')
        ),
        tabPanel(
          "Add/Delete",
          wellPanel(
            actionButton("email_add", "Add an email entry"),
            actionButton("email_clean", "Delete entire email list")
          )
        ),
        tabPanel(
          "User Types",
          wellPanel(
            names.Type.ModuleUI('my.names.Type')
          )

        ),
        tabPanel(
          "Likes",
          wellPanel(
            names.Like.ModuleUI('my.names.Like')
          )

        )
      )
    )
  )
)

shinyApp(ui = ui, server = server)
