# minimal DTedit example
\donttest{
library(shiny)
library(DTedit)

server <- function(input, output) {
  
  Grocery_List <- callModule(
    dteditmod,
    'Grocery_List',
    thedata = data.frame(
      Buy = c('Tea', 'Biscuits', 'Apples'),
      Quantity = c(7, 2, 5),
      stringsAsFactors = FALSE
    )
  )
}

ui <- fluidPage(
  h3('Grocery List'),
  dteditmodUI('Grocery_List')
)

shinyApp(ui = ui, server = server)
}