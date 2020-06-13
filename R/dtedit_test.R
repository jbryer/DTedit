#' @include dtedit.R
#' needs dtedit/dteditmod/dteditmodUI
NULL

#' test application
#' 
#' for testthat/codecov
#' 
#' @param appname choose test
#' 
#' @return a shiny app
#' @export
testDTedit <- function(appname = "simple") {
  
  if (appname == "simple") {
    server <- function(input, output) {
      
      Grocery_List <- dtedit(
        input, output,
        name = 'Grocery_List',
        thedata = data.frame(
          Buy = c('Tea', 'Biscuits', 'Apples'),
          Quantity = c(7, 2, 5),
          stringsAsFactors = FALSE
        )
      )
      
      data_list <- list() # exported list for shinytest
      shiny::observeEvent(Grocery_List$thedata(), {
        data_list[[length(data_list) + 1]] <<- Grocery_List$thedata()
      })
      shiny::exportTestValues(data_list = {data_list})
    }
    
    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      shiny::uiOutput('Grocery_List')
    )
    
    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return (shiny::shinyApp(ui = ui, server = server))
  }
  
  if (appname == "simple_modular") {
    server <- function(input, output, session) {
      
      Grocery_List <- callModule(
        dteditmod,
        id = 'Grocery_List',
        thedata = data.frame(
          Buy = c('Tea', 'Biscuits', 'Apples'),
          Quantity = c(7, 2, 5),
          stringsAsFactors = FALSE
        )
      )
      data_list <- list() # exported list for shinytest
      edit_count <- list()
      shiny::observeEvent(Grocery_List$thedata(), {
        data_list[[length(data_list) + 1]] <<- Grocery_List$thedata()
        edit_count[[length(edit_count) + 1]] <<- Grocery_List$edit.count()
      })
      shiny::exportTestValues(data_list = {data_list}, edit_count = {edit_count})
    }
    
    ui <- fluidPage(
      h3('Grocery List'),
      dteditmodUI('Grocery_List')
    )
    
    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return(shinyApp(ui = ui, server = server))
  }

  if (appname == "reactive") {
    server <- function(input, output) {
      
      mydata <- reactiveVal({
        data.frame(
          Buy = c('Tea', 'Biscuits', 'Apples'),
          Quantity = c(7, 2, 5),
          stringsAsFactors = FALSE
        )
      })
      
      Grocery_List_Results <- dtedit(
        input, output,
        name = 'Grocery_List',
        thedata = mydata
      )
      
      observeEvent(input$more, {
        # if the 'Buy More!' button is pressed
        newdata <- data.frame(
          Buy = mydata()$Buy,
          Quantity = mydata()$Quantity * 2,
          # doubles the quantity
          stringsAsFactors = FALSE
        )
        mydata(newdata)
      })
      
      observeEvent(input$less, {
        # if the 'Too Much!' button is pressed
        newdata <- data.frame(
          Buy = mydata()$Buy,
          Quantity = mydata()$Quantity * 0.5,
          # halves the quantity
          stringsAsFactors = FALSE
        )
        mydata(newdata)
      })
      
      data_list <- list() # exported list for shinytest
      edit_count <- list()
      observeEvent(Grocery_List_Results$thedata(), {
        # the data has been added
        # copy the changes to our own copy
        mydata(Grocery_List_Results$thedata())
        data_list[[length(data_list) + 1]] <<- Grocery_List_Results$thedata()
        edit_count[[length(edit_count) + 1]] <<- Grocery_List_Results$edit.count()
      })
      shiny::exportTestValues(data_list = {data_list}, edit_count = {edit_count})
      
    }
    
    ui <- fluidPage(
      h3('Grocery List'),
      uiOutput('Grocery_List'),
      actionButton(inputId = "more", label = "Buy More!"),
      actionButton(inputId = "less", label = "Too Much!")
    )
    
    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return(shinyApp(ui = ui, server = server))
  }
  
  if (appname == "callback") {
    server <- function(input, output) {
      
      grocery.update.callback <- function(data, olddata, row) {
        # 'data' contains the dataframe *after* the row has been updated
        # 'row' is the row number where data has been updated
        # 'olddata' is the previous version of the data
        
        if (data[row, "Quantity"] < 0) {
          stop("Can't buy less than zero (0)!")
        }
        
        return(data)
      }
      
      grocery.insert.callback <- function(data, row) {
        # 'data' contains the dataframe *after* the row has been inserted
        
        if (data[row, "Quantity"] > 10) {
          stop("Can't buy more than ten (10)!")
        }
        
        return(data)
      }
      
      grocery.delete.callback <- function(data, row) {
        # 'data' contains the dataframe *after* the row has been inserted
        
        if (data[row, "Quantity"] != 0) {
          stop("Can only delete if quantity equal to zero!")
        }
        
        data <- data[-row, ]
        
        return(data)
      }
      
      grocery.callback.actionButton <- function(data, row, buttonID) {
        # data - the current copy of 'thedata'
        # row - the row number of the clicked button
        # buttonID - the buttonID of the clicked button
        print(paste("You chose", buttonID, ", row: ", row))
        
        if (substr(buttonID, 1, nchar("addOne")) == "addOne") {
          # in this demonstration, all the buttons are 'random'
          # but it is possible to define more than one column of buttons
          data[row, "Quantity"] <- data[row, "Quantity"][[1]] + 1
        }
        if (substr(buttonID, 1, nchar("subtractOne")) == "subtractOne") {
          # in this demonstration, all the buttons are 'random'
          # but it is possible to define more than one column of buttons
          data[row, "Quantity"] <- data[row, "Quantity"][[1]] - 1
        }
        return(data)
      }
      
      Grocery_List_Results <- dtedit(
        input, output,
        name = 'Grocery_List',
        thedata = data.frame(
          Buy = c('Tea', 'Biscuits', 'Apples'),
          Quantity = c(7, 2, 5),
          stringsAsFactors = FALSE
        ),
        action.buttons = list(
          myaction = list( # the 'myaction' name is arbitrary
            columnLabel = "Add One",
            buttonLabel = "+1",
            buttonPrefix = "addOne"
          ),
          myaction2 = list( # the 'myaction' name is arbitrary
            columnLabel = "Remove One",
            buttonLabel = "-1",
            buttonPrefix = "subtractOne"
          )
        ),
        callback.update = grocery.update.callback,
        callback.delete = grocery.delete.callback,
        callback.insert = grocery.insert.callback,
        callback.actionButton = grocery.callback.actionButton
      )
      
      data_list <- list() # exported list for shinytest
      edit_count <- list()
      observeEvent(Grocery_List_Results$thedata(), {
        data_list[[length(data_list) + 1]] <<- Grocery_List_Results$thedata()
        edit_count[[length(edit_count) + 1]] <<- Grocery_List_Results$edit.count()
      })
      shiny::exportTestValues(data_list = {data_list}, edit_count = {edit_count})
    }
    
    ui <- fluidPage(
      h3('Grocery List'),
      shiny::uiOutput('Grocery_List')
    )
    
    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return(shinyApp(ui = ui, server = server))
  }
  
  if (appname == "error_test") {
    server <- function(input, output) {
      
      Grocery_List <- dtedit(
        input, output,
        name = 'Grocery_List',
        thedata = data.frame(
          Buy = c('Tea', 'Biscuits', 'Apples'),
          Quantity = as.integer(c(7, 2, 5)),
          stringsAsFactors = FALSE
        )
      )
      
      error_list <- list()
      error_message <- function(msg) {
        error_list[[length(error_list) + 1]] <<- as.character(msg)
      }
      
      tryCatch(
        dtedit(
          input, output,
          name = "No_columns",
          thedata = data.frame()
        ),
        error = function(e) error_message(e)
      )
      
      tryCatch(
        dtedit(
          input, output,
          name = "Edit_label_disparity",
          thedata = data.frame(
            Buy = c('Tea', 'Biscuits', 'Apples'),
            Quantity = c(7, 2, 5),
            stringsAsFactors = FALSE
          ),
          edit.cols = c("Buy"),
          edit.label.cols = c("Item", "Number")
        ),
        error = function(e) error_message(e)
      )
      
      tryCatch(
        dtedit(
          input, output,
          name = "View_col_notThere",
          thedata = data.frame(
            Buy = c('Tea', 'Biscuits', 'Apples'),
            Quantity = c(7, 2, 5),
            stringsAsFactors = FALSE
          ),
          view.cols = c("Buy", "Sell")
        ),
        error = function(e) error_message(e)
      )
      
      tryCatch(
        dtedit(
          input, output,
          name = "Edit_col_notThere",
          thedata = data.frame(
            Buy = c('Tea', 'Biscuits', 'Apples'),
            Quantity = c(7, 2, 5),
            stringsAsFactors = FALSE
          ),
          edit.cols = c("Buy", "Shop")
        ),
        error = function(e) error_message(e)
      )
      
      tryCatch(
        dtedit(
          input, output,
          name = "Input_notEdit",
          thedata = data.frame(
            Buy = c('Tea', 'Biscuits', 'Apples'),
            Quantity = as.integer(c(7, 2, 5)),
            stringsAsFactors = FALSE
          ),
          edit.cols = c("Buy"),
          input.types = list(Buy = "textInput", Quantity = "numericInput")
        ),
        error = function(e) error_message(e)
      )
      
      tryCatch(
        dtedit(
          input, output,
          name = "Input_notValidType",
          thedata = data.frame(
            Buy = c('Tea', 'Biscuits', 'Apples'),
            Quantity = as.integer(c(7, 2, 5)),
            stringsAsFactors = FALSE
          ),
          input.types = list(Buy = "textInput", Quantity = "mySpecialNumeric")
        ),
        error = function(e) error_message(e)
      )
      
      # following will generate warning when trying to add a new row
      tryCatch(
        w1 <- dtedit(
          input, output,
          name = "NoChoice_selectInput",
          thedata = data.frame(
            Buy = character(),
            Quantity = integer(),
            stringsAsFactors = FALSE
          ),
          input.types = list(Buy = "selectInput")
        )
      )
      
      tryCatch(
        w2 <- dtedit(
          input, output,
          name = "NoChoice_selectInputReactive",
          thedata = data.frame(
            Buy = character(),
            Quantity = integer(),
            stringsAsFactors = FALSE
          ),
          input.types = list(Buy = "selectInputReactive")
        )
      )
      
      tryCatch(
        w3 <- dtedit(
          input, output,
          name = "NoChoice_selectInputMultiple",
          thedata = data.frame(
            Buy = character(),
            Quantity = integer(),
            stringsAsFactors = FALSE
          ),
          input.types = list(Buy = "selectInputMultiple")
        )
      )
      
      tryCatch(
        w4 <- dtedit(
          input, output,
          name = "NoChoice_selectInputMultipleReactive",
          thedata = data.frame(
            Buy = character(),
            Quantity = integer(),
            stringsAsFactors = FALSE
          ),
          input.types = list(Buy = "selectInputMultipleReactive")
        )
      )
      
      data_list <- list() # exported list for shinytest
      shiny::observeEvent(Grocery_List$thedata(), {
        data_list[[length(data_list) + 1]] <<- Grocery_List$thedata()
      })
      shiny::exportTestValues(data_list = {data_list}, error_list = {error_list})
    }
    
    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      shiny::uiOutput('Grocery_List'),
      shiny::uiOutput('NoChoice_selectInput'),
      shiny::uiOutput('NoChoice_selectInputReactive'),
      shiny::uiOutput('NoChoice_selectInputMultiple'),
      shiny::uiOutput('NoChoice_selectInputMultipleReactive')
    )
    
    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return(shiny::shinyApp(ui = ui, server = server))
  }
}  