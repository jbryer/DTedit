#' @include dtedit.R
#' needs dtedit/dteditmod/dteditmodUI
NULL

#' test application
#'
#' for testthat/codecov
#'
#' @param appname choose test
#'   simple
#'   simple_modular
#'   reactive
#'   callback
#'   error_test
#'   selectInputReactive
#'   password
#'   datetimeInput
#'   inputEvent
#' @param ... extra options passed to shiny::shinyApp
#'
#' @return a shiny app
#' @export
dtedit_test <- function(appname = "simple", ...) {

  if (appname == "simple") {
    server <- function(input, output, session) {

      Grocery_List <- dtedit(
        input, output,
        name = 'Grocery_List',
        thedata = data.frame(
          Buy = c('Tea', 'Biscuits', 'Apples'),
          Quantity = c(7, 2, 5),
          stringsAsFactors = FALSE
        )
      )

      #### shinytest code for testing purposes only ########
      data_list <- list() # exported list for shinytest
      shiny::observeEvent(Grocery_List$thedata, {
        data_list[[length(data_list) + 1]] <<- Grocery_List$thedata
      })
      shiny::observeEvent(Grocery_List$rows_selected, ignoreNULL = FALSE, {
        data_list[[length(data_list) + 1]] <<-
          paste("Row selected: ", Grocery_List$rows_selected)
      }) # record the selected row
      shiny::exportTestValues(data_list = {data_list})
      ######################################################
    }

    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      shiny::uiOutput('Grocery_List')
    )

    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return(shiny::shinyApp(ui = ui, server = server, ...))
  }

  if (appname == "simple_modular") {
    myModuleUI <- function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        dteditmodUI(ns('Grocery_List'))
      )
    }

    myModule <- function(input, output, session) {
      Grocery_List_Results <- shiny::callModule(
        dteditmod,
        id = 'Grocery_List',
        thedata = data.frame(
          Buy = c('Tea', 'Biscuits', 'Apples'),
          Quantity = c(7, 2, 5),
          stringsAsFactors = FALSE
        ),
        delete.info.cols = c("Buy"),
        delete.info.label.cols = c("Product"),
        icon.delete = shiny::icon("trash"),
        icon.edit = shiny::icon("edit"),
        icon.add = shiny::icon("plus"),
        icon.copy = shiny::icon("copy")
      )
      return(
        list(
          thedata = reactive({Grocery_List_Results$thedata}),
          edit.count = reactive({Grocery_List_Results$edit.count})
        )
      )
    }

    server <- function(input, output, session) {

      Grocery_List <- shiny::callModule(myModule, 'myModule1')

      #### shinytest code for testing purposes only ########
      data_list <- list() # exported list for shinytest
      edit_count <- list()
      shiny::observeEvent(Grocery_List$thedata(), {
        data_list[[length(data_list) + 1]] <<- Grocery_List$thedata()
        edit_count[[length(edit_count) + 1]] <<- Grocery_List$edit.count()
      })
      shiny::exportTestValues(
        data_list = {data_list}, edit_count = {edit_count}
      )
      ######################################################
    }

    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      myModuleUI('myModule1')
    )

    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return(shiny::shinyApp(ui = ui, server = server, ...))
  }

  if (appname == "reactive") {
    server <- function(input, output, session) {

      mydata <- shiny::reactiveVal({
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

      #### shinytest code for testing purposes only ########
      data_list <- list() # exported list for shinytest
      edit_count <- list()
      observeEvent(Grocery_List_Results$thedata, {
        # the data has been added
        # copy the changes to our own copy
        mydata(Grocery_List_Results$thedata)
        data_list[[length(data_list) + 1]] <<- Grocery_List_Results$thedata
        edit_count[[length(edit_count) + 1]] <<-
          Grocery_List_Results$edit.count
      })
      shiny::exportTestValues(
        data_list = {data_list}, edit_count = {edit_count}
      )
      #### shinytest code for testing purposes only ########
    }

    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      uiOutput('Grocery_List'),
      actionButton(inputId = "more", label = "Buy More!"),
      actionButton(inputId = "less", label = "Too Much!")
    )

    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return(shiny::shinyApp(ui = ui, server = server, ...))
  }

  if (appname == "callback") {
    server <- function(input, output, session) {

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
        click.time.threshold = 0.1, # very short, for auto-testing
        callback.update = grocery.update.callback,
        callback.delete = grocery.delete.callback,
        callback.insert = grocery.insert.callback,
        callback.actionButton = grocery.callback.actionButton
      )

      #### shinytest code for testing purposes only ########
      data_list <- list() # exported list for shinytest
      edit_count <- list()
      observeEvent(Grocery_List_Results$thedata, {
        data_list[[length(data_list) + 1]] <<- Grocery_List_Results$thedata
        edit_count[[length(edit_count) + 1]] <<-
          Grocery_List_Results$edit.count
      })
      shiny::exportTestValues(
        data_list = {data_list}, edit_count = {edit_count}
      )
      #### shinytest code for testing purposes only ########
    }

    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      shiny::uiOutput('Grocery_List')
    )

    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return(shiny::shinyApp(ui = ui, server = server, ...))
  }

  if (appname == "error_test") {
    server <- function(input, output, session) {

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

      tryCatch(
        dtedit(
          input, output,
          name = "DeleteInfoCols_notEqual_DeleteLabelCols",
          thedata = data.frame(
            Buy = c('Tea', 'Biscuits', 'Apples'),
            Quantity = as.integer(c(7, 2, 5)),
            stringsAsFactors = FALSE
          ),
          delete.info.cols = c("Buy", "Quantity"),
          delete.info.label.cols = c("Product")
        ),
        error = function(e) error_message(e)
      )

      tryCatch(
        dtedit(
          input, output,
          name = "DeleteInfoCols_notDefined",
          thedata = data.frame(
            Buy = c('Tea', 'Biscuits', 'Apples'),
            Quantity = as.integer(c(7, 2, 5)),
            stringsAsFactors = FALSE
          ),
          delete.info.cols = c("Buy", "Comment")
        ),
        error = function(e) error_message(e)
      )

      tryCatch(
        dtedit(
          input, output,
          name = "datetimeInput_notAllowed",
          thedata = data.frame(
            Buy = c('Tea', 'Biscuits', 'Apples'),
            Quantity = as.integer(c(7,2,3)),
            DueDate = as.POSIXct(c("2020-09-10", "2020-11-14", "2021-05-23")),
            stringsAsFactors = FALSE
          )
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

      #### shinytest code for testing purposes only ########
      data_list <- list() # exported list for shinytest
      shiny::observeEvent(Grocery_List$thedata, {
        data_list[[length(data_list) + 1]] <<- Grocery_List$thedata
      })
      shiny::exportTestValues(
        data_list = {data_list}, error_list = {error_list}
      )
      #### shinytest code for testing purposes only ########
    }

    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      shiny::uiOutput('Grocery_List'),
      shiny::h4("No choices selectInput"),
      shiny::uiOutput('NoChoice_selectInput'),
      shiny::h4("No choices selectInput Reactive"),
      shiny::uiOutput('NoChoice_selectInputReactive'),
      shiny::h5("No choices selectInputMultiple"),
      shiny::uiOutput('NoChoice_selectInputMultiple'),
      shiny::h5("No choices selectInputMultiple Reactive"),
      shiny::uiOutput('NoChoice_selectInputMultipleReactive')
    )

    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return(shiny::shinyApp(ui = ui, server = server, ...))
  }

  if (appname == "selectInputReactive") {
    server <- function(input, output, session) {

      less_choices <- c('Tea', 'Biscuits', 'Apples', 'Cheese')
      more_choices <- c(less_choices, 'Coffee', 'Pears', 'Fish')

      buy.Types <- shiny::reactiveVal(less_choices)

      Grocery_List_Results <- dtedit(
        input, output,
        name = 'Grocery_List',
        thedata = data.frame(
          Buy = c('Tea', 'Biscuits', 'Apples'),
          Timeframe = c('Today', 'Soon', 'Flexible'),
          Type = c('Plant', 'Processed', 'Fruit'),
          Quantity = c(7, 2, 5),
          BuyFor = I(list(list('Anne', 'Bob'),
                          list('Carly', 'Anne'),
                          list('Anne', 'Bob', 'Carly'))),
          stringsAsFactors = FALSE
        ),
        input.types = list(
          Buy = 'selectInputReactive',
          Timeframe = 'selectInput', # not explicitly defined choices
          Type = 'selectInput',
          BuyFor = 'selectInputMultiple' # not explicitly defined choices
        ),
        input.choices = list(
          Buy = 'buy.Types.list',
          Type = c('Plant', 'Processed', 'Fruit', 'Animal')
        ),
        input.choices.reactive =
          list(buy.Types.list = buy.Types)
      )

      observeEvent(input$choice, {
        if (input$choice == 1) {
          buy.Types(less_choices)
        } else {
          buy.Types(more_choices)
        }
      })

      #### shinytest code for testing purposes only ########
      data_list <- list() # exported list for shinytest
      shiny::observeEvent(Grocery_List_Results$thedata, {
        data_list[[length(data_list) + 1]] <<- Grocery_List_Results$thedata
      })
      shiny::exportTestValues(data_list = {data_list})
      ######################################################
    }

    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      uiOutput('Grocery_List'),
      shiny::radioButtons(
        'choice',
        label = 'Buy choices',
        choices = list('Less' = 1, 'More' = 2),
        selected = 1
      )
    )

    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return(shiny::shinyApp(ui = ui, server = server))
  }

  if (appname == "password") {
    server <- function(input, output, session) {
      Password_List <- dtedit(
        input, output,
        name = 'Password_List',
        thedata = data.frame(
          Name = c('Sylvia', 'Eric', 'Molly'),
          Password = c('', '', ''),
          stringsAsFactors = FALSE
        ),
        view.cols = c("Name"),
        input.types = c(Password = "passwordInput")
      )

      #### shinytest code for testing purposes only ########
      data_list <- list() # exported list for shinytest
      shiny::observeEvent(Password_List$thedata, {
        data_list[[length(data_list) + 1]] <<- Password_List$thedata
      })
      shiny::exportTestValues(data_list = {data_list})
      ######################################################
    }

    ui <- shiny::fluidPage(
      shiny::h3('Passwords'),
      shiny::uiOutput('Password_List')
    )

    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return (shiny::shinyApp(ui = ui, server = server, ...))
  }

  if (appname == "datetimeInput") {
    server <- function(input, output) {

      Grocery_List_Results <- dtedit(
        input, output,
        name = 'Grocery_List',
        thedata = data.frame(
          Buy = c('Tea', 'Biscuits', 'Apples'),
          Quantity = c(7, 2, 5),
          DueTime = as.POSIXct(c("2020-09-10", "2020-5-14", "2021-05-23"), tz = "GMT"),
          PurchaseDate = c(as.Date("2020-05-04"), as.Date(NA), as.Date("2018-07-04")),
          stringsAsFactors = FALSE
        ),
        useairDatepicker = TRUE
        ######################################################
        # test (superficially) airDatepicker input
        # unfortunately, there is no testing for datetimeInput yet
        # because
        # `app$setInputs(ToBuy_edit_DueTime = 1605548460000)`
        # does not set the widget's value, and I am unable
        # to find an alternative app$executeScript which will
        # set the vlaue
        # see https://github.com/rstudio/shinytest/issues/252
        ######################################################
      )

      #### shinytest code for testing purposes only ########
      data_list <- list() # exported list for shinytest
      shiny::observeEvent(Grocery_List_Results$thedata, {
        data_list[[length(data_list) + 1]] <<- Grocery_List_Results$thedata
      })
      shiny::exportTestValues(data_list = {data_list})
      ######################################################
    }

    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      shiny::uiOutput('Grocery_List')
    )

    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return (shiny::shinyApp(ui = ui, server = server, ...))
  }

  if (appname == "inputEvent") {

    server <- function(input, output) {
      Grocery_List_Results <- dtedit(
        input, output,
        name = 'Grocery_List',
        thedata = data.frame(
          Buy = c('Tea', 'Biscuits', 'Apples'),
          Quantity = c(7, 2, 5),
          stringsAsFactors = FALSE
        ),
        inputEvent = list(
          Quantity = function(x, value) {
            # value will be NA, if empty input box
            if (!is.na(value) && value > 100) {
              shiny::updateNumericInput(
                session = shiny::getDefaultReactiveDomain(),
                inputId = x,
                value = NA
              )
            }
          }
        )
      )

      #### shinytest code for testing purposes only ########
      data_list <- list() # exported list for shinytest
      shiny::observeEvent(Grocery_List_Results$thedata, {
        data_list[[length(data_list) + 1]] <<- Grocery_List_Results$thedata
      })
      shiny::exportTestValues(data_list = {data_list})
      ######################################################
    }

    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      shiny::uiOutput('Grocery_List')
    )

    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return (shiny::shinyApp(ui = ui, server = server, ...))
  }

  if (appname == "selectizeInput") {

    server <- function(input, output) {

      less_states <- list(
        Eastern = c(`New York` = 'NY', `New Jersey` = 'NJ'),
        Western = c(`California` = 'CA', `Washington` = 'WA')
      )
      more_states <- list(
        Eastern = c(`New York` = 'NY', `New Jersey` = 'NJ'),
        Midwest = c(`Illinois` = 'IL', `Indiana` = 'IN', `Minnestota` = 'MN'),
        Western = c(`California` = 'CA', `Washington` = 'WA')
      )
      less_product <- c('clothes', 'food', 'toys')
      more_product <- c('clothes', 'food', 'toys', 'tea', 'coffee')

      from.states <- shiny::reactiveVal(less_states)
      product.types <- shiny::reactiveVal(less_product)

      Grocery_List_Results <- dtedit(
        input, output,
        name = 'Grocery_List',
        thedata = data.frame(
          Store = c('stor1', 'stor1', 'stor2'),
          Product = c('food', 'clothes', 'clothes'),
          FromState = I(list(list('CA', 'NJ'), list('WA'), list('NY'))),
          ToState = I(list(list('CA'), list('WA'), list('NY'))),
          Quantity = c(7, 2, 5),
          stringsAsFactors = FALSE
        ),
        edit.label.cols = c('Store', 'Product', 'From State', 'To State', 'Quantity'),
        input.types = list(
          Store = 'selectizeInput',
          Product = 'selectizeInputReactive',
          FromState = 'selectizeInputMultipleReactive',
          ToState = 'selectizeInputMultiple'
        ),
        input.choices = list(
          Store = c("stor1","stor2"),
          Product = 'product.list',
          FromState = 'from.states.list',
          ToState = less_states
        ),
        input.choices.reactive =
          list(from.states.list = from.states,
               product.list = product.types),
        selectize.options = list(
          Store = list(
            placeholder = "Please select an option below",
            onInitialize = I('function() { this.setValue(""); }')
          ),
          FromState = list(create = TRUE, maxItems = 2),
          ToState = list(create = TRUE, maxItems = 3)
        )
      )

      shiny::observeEvent(input$choice_states, {
        if (input$choice_states == 1) {
          from.states(less_states)
        } else {
          from.states(more_states)
        }
      })
      shiny::observeEvent(input$choice_product, {
        if (input$choice_product == 1) {
          product.types(less_product)
        } else {
          product.types(more_product)
        }
      })

      #### shinytest code for testing purposes only ########
      data_list <- list() # exported list for shinytest
      shiny::observeEvent(Grocery_List_Results$thedata, {
        data_list[[length(data_list) + 1]] <<- Grocery_List_Results$thedata
      })
      shiny::exportTestValues(data_list = {data_list})
      ######################################################

    }

    ui <- shiny::fluidPage(
      shiny::h3('Grocery List'),
      shiny::uiOutput('Grocery_List'),
      shiny::radioButtons(
        inputId = 'choice_states',
        label = "'From State' choices",
        choices = list('Less' = 1, 'More' = 2),
        selected = 1
      ),
      shiny::radioButtons(
        inputId = 'choice_product',
        label = "Product choices",
        choices = list('Less' = 1, 'More' = 2),
        selected = 1
      )
    )

    if (interactive() || isTRUE(getOption("shiny.testmode")))
      return (shiny::shinyApp(ui = ui, server = server, ...))
  }
}
