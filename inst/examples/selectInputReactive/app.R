library(shiny)
library(DTedit)

##### Create the Shiny server
server <- function(input, output, session) {

  names.Type.update.callback <- function(data, olddata, row) {
    # update a user-type
    # do not allow an updated user-type which is the same as another
    ## if this is attempted, show a warning
    if (data[row,] %in% data[-row,]) {
      stop(paste0("Cannot change user-type to '", data[row,],"', that user-type already exists!"))
    }
    return(data)
  }

  names.Type.insert.callback <- function(data, row) {
    # insert a user-type
    # do not allow a new user-type which is the same as an old one
    ## if this is attempted, show a warning
    if (data[row,] %in% data[-row,]) {
      stop(paste0("Cannot add '", data[row,],"', that user-type already exists!"))
    }
    return(data)
  }

  names.Type.delete.callback <- function(data, row) {
    # remove a user-type
    # it is possible for this user-type to be currently used
    # by an entry in names()  (names has been explicitly passed by reference
    # to the dtedit function), in which case this function will show a warning
    if (data[row,] %in% get("input.choices.reactive", parent.frame())[["names"]]()$Type) {
      stop(paste0("Cannot delete '", data[row,],
                  "', this user-type currently assigned to a user."))
    } else {
      data <- data[-row,, drop = FALSE]
    }
    return(data)
  }

  names.Like.update.callback <- function(data, olddata, row) {
    # update a like
    # do not allow an updated like which is the same as another
    ## if this is attempted, show a warning
    if (data[row,] %in% data[-row,]) {
      stop(paste0("Cannot change like to '", data[row,],"', that like already exists!"))
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
    if (data[row,] %in% unlist(get("input.choices.reactive", parent.frame())[["names"]]()$Like)) {
      stop(paste0("Cannot delete '", data[row,],
                  "', this like currently assigned to a user."))
    } else {
      data <- data[-row,, drop = FALSE]
    }
    return(data)
  }

  names.Like <- reactiveVal()
  names.Like(data.frame(Likes = c("Apple", "Pear"), stringsAsFactors = FALSE))
  names.Likedt <- dtedit(
    input, output,
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
  names.Likes <- reactiveVal(isolate(names.Like()$Likees))

  names.Type <- reactiveVal()
  names.Type(data.frame(Types = c("Admin", "User"), stringsAsFactors = FALSE))
  names.Typedt <- dtedit(
    input, output,
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

  names.Types <- reactiveVal(isolate(names.Type()$Types))

  names <- reactiveVal()
  names(
    data.frame(
      Name=character(), Email=character(),
      Date=as.Date(integer(), origin='1970-01-01'),
      Type = character(),
      Like = character(),
      # Like = I(list(isolate(factor(character(), levels = names.Likes())))),
      stringsAsFactors=FALSE
    )
  )

  observe({
    names.Types(names.Typedt$thedata$Types)
    names.Likes(names.Likedt$thedata$Likes)
  })

  namesdt <- dtedit(
    input, output,
    'names',
    thedata = names,
    input.types = c(Type = "selectInputReactive", Like = "selectInputMultipleReactive"),
    input.choices = c(Type = "names.Types", Like = "names.Likes"),
    input.choices.reactive = list(names.Types = names.Types, names.Likes = names.Likes)
  )

  observe({
    print(namesdt$thedata)
    names(as.data.frame(namesdt$thedata, stringsasfactors = FALSE))
    print(paste("Edit count:", namesdt$edit.count))
  })

  observeEvent(input$email_clean,{
    names(names()[0,]) # empty the dataframe
  })

  observeEvent(input$email_add, {
    email <- c("hotmail.com", "yahoo.com",
               "gmail.com", "outlook.com",
               "github.com", "bigpond.com",
               "medscape.com")
    extra_email <- data.frame( # create random user
      Name = randomNames::randomNames(name.order = "first.last", name.sep = " "),
      Email = paste0(
        do.call(paste0, replicate(sample(5:8, 1), sample(tolower(LETTERS), 1, TRUE), FALSE)),
        '@',sample(email, 1)
      ),
      Date = as.Date(Sys.Date()-sample(1:1000, 1), origin = "1970-01=01"),
      Type = factor(sample(names.Types(), 1), levels = names.Types()),
      Like = I(list(factor(sample(names.Likes(), sample(1:length(names.Likes()), 1)),
                           levels = names.Likes()))),
      stringsAsFactors = FALSE
    )
    names(data.frame(rbind(names(), extra_email), stringsAsFactors = FALSE))
  })

  data_list <- list() # exported list for shinytest
  shiny::observeEvent(namesdt$thedata, {
    data_list[[length(data_list) + 1]] <<- namesdt$thedata
  })
  shiny::exportTestValues(data_list = {data_list})

}

##### Create the shiny UI
ui <- fluidPage(
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Users",
      h3('Email Addresses'),
      uiOutput('names')
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
        uiOutput("names.Type")
      )

    ),
    tabPanel(
      "Likes",
      wellPanel(
        uiOutput("names.Like")
      )

    )
  )
)

if (interactive() || isTRUE(getOption("shiny.testmode")))
  shinyApp(ui = ui, server = server)
