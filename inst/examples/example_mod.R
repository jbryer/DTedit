# minimal DTedit example 'dteditmod'
# this is a separate application from the 'dtedit' example!
#
# unfortunately, this application cannot be
# tried with 'example("dteditmod")', but you can copy
# and paste to execute in 'interactive' console mode,
# or copy the lines into an '.R' file and choose
# 'Run App' from RStudio.
library(shiny)
library(DTedit)

myModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dteditmodUI(ns('Grocery_List'))
  )
}

myModule <- function(input, output, session) {
  Grocery_List_Results <- shiny::callModule(
    dteditmod,
    id = 'Grocery_List',
    thedata = data.frame(
      Buy = c('Tea', 'Biscuits', 'Apples'),
      Quantity = c(7, 2, 5),
      stringsAsFactors = FALSE
    )
  )
}

server <- function(input, output, session) {

  shiny::callModule(myModule, 'myModule1')

}

ui <- fluidPage(
  h3('Grocery List'),
  myModuleUI('myModule1')
)

if (interactive() || isTRUE(getOption("shiny.testmode")))
  shinyApp(ui = ui, server = server)
