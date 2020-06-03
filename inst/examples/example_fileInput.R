# minimal DTedit file input example
\donttest{
  library(shiny)
  library(DTedit)
  library(blob)

  server <- function(input, output) {

    my.actionButton.callback <- function(data, row, buttonID) {
      ns <- parent.frame(1)$ns # the namespace of the calling environment
      outfile <- tempfile(fileext = ".png")
      zz <- file(outfile, "wb")
      writeBin(object = unlist(data[row, "Picture"]), con = zz)
      close(zz)
      shiny::showModal(
        shiny::modalDialog(
          shiny::fluidPage(
            shiny::tags$img(
              src = base64enc::dataURI(file = outfile),
              width = "100%"
            )
          ),
          size = "l",
          footer = list(
            shiny::actionButton(ns("closeMyModal"), "OK")
          )
        )
      )
      browser()

      # cleanup
      file.remove(outfile)
      return(NULL)
    }

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
      input.choices = list(Picture = ".png"),
      action.button = list(
        MyActionButton = list(
          columnLabel = "Show Picture",
          buttonLabel = "Show",
          buttonPrefix = "button_",
          afterColumn = "Quantity")
      ),
      callback.actionButton = my.actionButton.callback
    )
  }

  ui <- fluidPage(
    h3('Grocery List'),
    dteditUI('Grocery_List')
  )

  shinyApp(ui = ui, server = server)
}

