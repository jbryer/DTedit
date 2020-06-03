# minimal DTedit file input example
\donttest{
  library(shiny)
  library(DTedit)
  library(blob)
  
  server <- function(input, output) {
    
    Grocery_List <- callModule(
      dtedit,
      'Grocery_List',
      thedataframe = data.frame(
        Buy = c('Tea', 'Biscuits', 'Apples'),
        Quantity = c(7, 2, 5),
        Picture = c(as.blob(raw(0)), as.blob(raw(0)), as.blob(raw(0))),
        stringsAsFactors = FALSE
      ),
      view.cols = c("Buy", "Quantity"),
      edit.cols = c("Buy", "Quantity", "Picture"),
      input.choices = list(Picture = "image/*"),
      action.button = list(
        MyActionButton = list(
          columnLabel = "Show Picture",
          buttonLabel = "Show",
          buttonPrefix = "button_",
          afterColumn = "Quantity")
      )
    )
  }
  
  ui <- fluidPage(
    h3('Grocery List'),
    dteditUI('Grocery_List')
  )
  
  shinyApp(ui = ui, server = server)
}

