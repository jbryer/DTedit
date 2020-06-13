app <- ShinyDriver$new("..")

app$snapshotInit("test")

app$snapshot(items = list(export = TRUE), screenshot = TRUE)

app$setInputs(Grocery_Listdt_rows_selected = 1, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_Listdt_rows_last_clicked = 1, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_List_edit = "click")
app$setInputs(Grocery_List_edit_Buy = "Teabags",
              Grocery_List_edit_Quantity = 2)
Sys.sleep(2)
app$setInputs(Grocery_List_update = "click")
app$setInputs(Grocery_Listdt_rows_selected = 2, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_Listdt_rows_last_clicked = 2, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_List_remove = "click")
Sys.sleep(2)
app$setInputs(Grocery_List_delete = "click")
app$setInputs(Grocery_Listdt_rows_selected = 3, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_Listdt_rows_last_clicked = 3, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_List_copy = "click")
app$setInputs(Grocery_List_add_Quantity = 5,
              Grocery_List_add_Buy = "Apple Cider")
Sys.sleep(2)
app$setInputs(Grocery_List_insert = "click")
app$setInputs(Grocery_List_add = "click")
app$setInputs(Grocery_List_add_Buy = "")
app$setInputs(Grocery_List_add_Buy = "Carrots",
              Grocery_List_add_Quantity = 7)
Sys.sleep(2)
app$setInputs(Grocery_List_insert = "click")
Sys.sleep(2)

app$snapshot(items = list(export = TRUE), screenshot = TRUE)

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/ Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()