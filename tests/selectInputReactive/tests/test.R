app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$setInputs(Grocery_List_add = "click")
app$setInputs(Grocery_List_add_Buy = "Cheese",
              Grocery_List_add_Type = "Processed",
              Grocery_List_add_Quantity = 2)
app$setInputs(Grocery_List_insert = "click")
Sys.sleep(2)
app$setInputs(choice = "2")
app$setInputs(Grocery_List_add = "click")
app$setInputs(Grocery_List_add_Buy = "Coffee",
              Grocery_List_add_Quantity = 1)
app$setInputs(Grocery_List_insert = "click")

Sys.sleep(2)
app$snapshot(items = list(export = TRUE), screenshot = TRUE)

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/
#  Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()