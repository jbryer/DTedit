app <- ShinyDriver$new("..")

app$snapshotInit("test")

app$snapshot(items = list(export = TRUE))

app$setInputs(Grocery_List_add = "click")
app$setInputs(Grocery_List_add_Buy = "")
app$setInputs(
  Grocery_List_add_Buy = "Carrots",
  Grocery_List_add_Quantity = 110,
  Grocery_List_add_PurchaseDate = as.character(
    jsonlite::toJSON(
      1000 * as.numeric(as.POSIXct(as.Date("2018-05-05"), tz = "UTC")),
      auto_unbox = FALSE)
  )
)
Sys.sleep(2)
app$setInputs(Grocery_List_insert = "click")
Sys.sleep(2)

app$snapshot(items = list(export = TRUE))

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/ Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
