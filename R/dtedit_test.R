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
}  