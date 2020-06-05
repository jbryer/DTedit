# minimal DTedit file input example, using blobs
\donttest{
  library(shiny)
  library(DTedit)
  library(blob)
  
  server <- function(input, output) {
    
    picture <- reactiveVal(NULL)
    spreadsheet <- reactiveVal(NULL)
    
    my.actionButton.callback <- function(data, row, buttonID) {
      if (substr(buttonID, 1, nchar("picture")) == "picture") {
        if (length(unlist(data[row, "Picture"])) > 0) {
          outfile <- tempfile(fileext = ".png")
          # create temporary filename
          zz <- file(outfile, "wb") # create temporary file
          writeBin(object = unlist(data[row, "Picture"]), con = zz)
          close(zz)
          
          picture(base64enc::dataURI(file = outfile))
          
          # cleanup
          file.remove(outfile)
        } else {
          picture(NULL)
        }
      }
      if (substr(buttonID, 1, nchar("spreadsheet")) == "spreadsheet") {
        if (length(unlist(data[row, "Spreadsheet"])) > 0) {
          outfile <- tempfile(fileext = ".csv")
          # create temporary filename
          zz <- file(outfile, "wb") # create temporary file
          writeBin(object = unlist(data[row, "Spreadsheet"]), con = zz)
          close(zz)
          
          spreadsheet(read.csv(outfile))
          
          # cleanup
          file.remove(outfile)
        } else {
          spreadsheet(NULL)
        }
      }
      return(NULL)
    }
    
    Grocery_List <- callModule(
      dtedit,
      'Grocery_List',
      thedataframe = data.frame(
        Buy = c('Tea', 'Biscuits', 'Apples'),
        Quantity = c(7, 2, 5),
        Picture = c(as.blob(raw(0)), as.blob(raw(0)), as.blob(raw(0))),
        Spreadsheet = c(as.blob(raw(0)), as.blob(raw(0)), as.blob(raw(0))),
        stringsAsFactors = FALSE
      ),
      view.cols = c("Buy", "Quantity"),
      edit.cols = c("Buy", "Quantity", "Picture", "Spreadsheet"),
      edit.label.cols = c(
        "Item to buy", "Quantity",
        "Picture (.png)", "Spreadsheet (.csv)"
      ),
      input.choices = list(Picture = ".png", Spreadsheet = ".csv"),
      # unfortunately, RStudio's 'browser' doesn't actually respect
      #  file-name/type restrictions. A 'real' browser does respect
      #  the restrictions.
      action.button = list(
        MyActionButton = list(
          columnLabel = "Picture",
          buttonLabel = "Show Picture",
          buttonPrefix = "picture",
          afterColumn = "Quantity"),
        MyOtherActionButton = list(
          columnLabel = "Spreadsheet",
          buttonLabel = "Show Spreadsheet",
          buttonPrefix = "spreadsheet"
        )
      ),
      callback.actionButton = my.actionButton.callback
    )
    
    output$listPicture <- shiny::renderUI({
      shiny::tags$img(
        src = picture(),
        width = "100%"
      )
    })
    
    output$showSpreadsheet <- DT::renderDataTable({
      spreadsheet()
    })
  }
  
  ui <- fluidPage(
    shinyalert::useShinyalert(),
    h3("Grocery List"),
    dteditUI("Grocery_List"),
    uiOutput("listPicture"),
    DT::dataTableOutput("showSpreadsheet")
  )
  
  shinyApp(ui = ui, server = server)
}

