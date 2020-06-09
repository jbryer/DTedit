# minimal DTedit example 'dtedit'
# you can try this example in interactive mode
# with 'example("dtedit")'

library(shiny)
library(DTedit)

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
  observeEvent(Grocery_List$thedata(), {
    data_list[[length(data_list) + 1]] <<- Grocery_List$thedata()
  })
  exportTestValues(data_list = {data_list})
}

ui <- fluidPage(
  h3('Grocery List'),
  uiOutput('Grocery_List')
)

if (interactive() || isTRUE(getOption("shiny.testmode")))
  shinyApp(ui = ui, server = server)

#### end of 'dtedit' example ####


