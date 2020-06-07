# minimal DTedit example 'dteditmod'
# this is a separate application from the 'dtedit' example!
#
# unfortunately, this application cannot be
# tried with 'example("dteditmod")', but you can copy
# the lines below into an '.R' file, or copy and paste to
# execute in 'interactive' console mode.
\donttest{
  library(shiny)
  library(DTedit)
  
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
  }
  
  ui <- fluidPage(
    h3('Grocery List'),
    dteditmodUI('Grocery_List')
  )
  
  shinyApp(ui = ui, server = server)
}