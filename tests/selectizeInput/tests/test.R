app <- ShinyDriver$new("../")

app$snapshotInit("test")

app$snapshot(items = list(export = TRUE))

app$setInputs(choice_states = "2")
app$setInputs(choice_product = "2")

app$setInputs(Grocery_Listdt_rows_selected = 2, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_Listdt_rows_last_clicked = 2, allowInputNoBinding_ = TRUE)

app$setInputs(Grocery_List_edit = "click")
app$setInputs(Grocery_List_edit_Product = "clothes",
              Grocery_List_edit_Quantity = 2,
              Grocery_List_edit_Store = "stor1",
              Grocery_List_edit_FromState = c("WA", "IN"),
              Grocery_List_edit_ToState = c("WA", "CA"))
Sys.sleep(2)
app$setInputs(Grocery_List_update = "click")

app$setInputs(Grocery_List_add = "click")
app$setInputs(Grocery_List_add_Store = "stor2",
              Grocery_List_add_Product = "food",
              Grocery_List_add_FromState = c("NY", "MN"),
              Grocery_List_add_ToState = "WA",
              Grocery_List_add_Quantity = 2)
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
