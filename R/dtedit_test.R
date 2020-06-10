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
      shiny::observeEvent(Grocery_List$thedata(), {
        data_list[[length(data_list) + 1]] <<- Grocery_List$thedata()
      })
      shiny::exportTestValues(data_list = {data_list})
    }
    
    ui <- fluidPage(
      h3('Grocery List'),
      dteditmodUI('Grocery_List')
    )
    
    if (interactive() || isTRUE(getOption("shiny.testmode")))
      shinyApp(ui = ui, server = server)
  }
}  