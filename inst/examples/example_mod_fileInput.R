# DTedit file input example, using blobs
library(shiny)
library(DTedit)
library(blob)

server <- function(input, output) {
  
  picture <- reactiveVal(NULL)
  spreadsheet <- reactiveVal(NULL)
  
  my.actionButton.callback <- function(data, row, buttonID) {
    if (substr(buttonID, 1, nchar("picture")) == "picture") {
      # the 'action' button identifier (ID) prefix shows this
      # this is from the 'Picture' column
      if (length(unlist(data[row, "Picture"])) > 0) {
        # not a empty entry!
        outfile <- tempfile(fileext = ".png")
        # create temporary filename
        # and write the binary blob to the temporary file
        zz <- file(outfile, "wb") # create temporary file
        writeBin(object = unlist(data[row, "Picture"]), con = zz)
        close(zz)
        
        # read the picture from the temporary file
        picture(base64enc::dataURI(file = outfile))
        
        # cleanup (remove the temporary file)
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
    dteditmod,
    'Grocery_List',
    thedata = data.frame(
      Buy = c('Tea', 'Biscuits', 'Apples'),
      Quantity = c(7, 2, 5),
      Picture = c(as.blob(raw(0)), as.blob(raw(0)), as.blob(raw(0))),
      Spreadsheet = c(as.blob(raw(0)), as.blob(raw(0)), as.blob(raw(0))),
      stringsAsFactors = FALSE
    ),
    view.cols = c("Buy", "Quantity"),
    # note that the 'Picture' and 'Spreadsheet' columns are hidden
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
        # the column label happens to be the same as a
        # data column label, but it doesn't need to be
        buttonLabel = "Show Picture",
        buttonPrefix = "picture",
        # buttonPrefix will be in the buttonID passed
        # to callback.actionButton
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
  h3("Grocery List"),
  "Pictures must be in PNG (.png) format, and spreadsheets must be",
  "in comma-separated-value (.csv) format!",
  br(), br(), br(),
  dteditmodUI("Grocery_List"),
  uiOutput("listPicture"),
  DT::dataTableOutput("showSpreadsheet")
)

if (interactive())
  shinyApp(ui = ui, server = server)