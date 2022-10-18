##### Minimal DTedit example using reactive dataframe #####
library(shiny)
library(DTedit)

##### module ######################

myModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dteditmodUI(ns("dataspace"))
  )
}

myModule <- function(input, output, session, data_scramble) {
  data <- reactiveVal() # # 'data' will be a 'reactive' dataframe
  data(data.frame(Column1 = c("Apple", "Cherry", "Frozen"),
                  Column2 = c("Pie", "Tart", "Yoghurt"),
                  stringsAsFactors = FALSE))
  data_DT_gui <- callModule(
    dteditmod,
    "dataspace",
    thedata = data,
    edit.cols = c("Column1", "Column2")
  )

  observe({
    data(
      isolate(
        as.data.frame(
          data_DT_gui$thedata,
          stringsasfactors = FALSE
        )
      )
    )
    print(isolate(data()))
    print(paste("Edit count:", data_DT_gui$edit.count))
    # only reacts to change in $edit.count
  })

  observeEvent(data_scramble(), {
    print("Scrambling...")
    temp <- data()
    if (nrow(temp) > 0) {
      row <- sample(seq_len(nrow(temp)), 1)  # row
      col <- sample(1:2, 1)           # column
      temp[row, col] <- paste(
        sample(unlist(strsplit(temp[row, col], "")),
               nchar(temp[row, col])),
        sep = '', collapse = '')
      data(temp) # adjusted dataframe 'automatically' read by DTedit
    }
  })
}

##### Create the Shiny server #####
server <- function(input, output) {

  data_scramble <- shiny::reactive({
    input$data_scramble
  })

  shiny::callModule(myModule, "myModule1", data_scramble)
}

##### Create the shiny UI ######
ui <- fluidPage(
  h3("DTedit using reactive dataframe"),
  wellPanel(p("Try the 'Scramble' button!")),
  myModuleUI("myModule1"),
  actionButton("data_scramble", "Scramble an entry")
)

if (interactive() || isTRUE(getOption("shiny.testmode")))
  shinyApp(ui = ui, server = server)
