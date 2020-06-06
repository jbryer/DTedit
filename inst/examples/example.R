# minimal DTedit example 'dtedit'
# you can try this example in interactive mode
# with 'example("dtedit")'
\donttest{
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
  }
  
  ui <- fluidPage(
    h3('Grocery List'),
    uiOutput('Grocery_List')
  )
  
  shinyApp(ui = ui, server = server)
}