app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$snapshot(items = list(export = TRUE), screenshot = TRUE)
app$setInputs(Grocery_List_add = "click")
app$setInputs(Grocery_List_add_Buy = "Bananas")
app$setInputs(Grocery_List_add_Quantity = 12) # should dis-allow
app$setInputs(Grocery_List_insert = "click")
app$executeScript("$('.modal').modal('hide');") # close modal
# for closing modal in shinytest,
#  see https://github.com/rstudio/shinytest/issues/227
#  by LukasK13
Sys.sleep(3)
app$setInputs(Grocery_List_add = "click")
app$setInputs(Grocery_List_add_Buy = "Mangos")
app$setInputs(Grocery_List_add_Quantity = 3)
app$setInputs(Grocery_List_insert = "click")
app$setInputs(Grocery_Listdt_rows_selected = 2, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_Listdt_rows_last_clicked = 2, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_List_remove = "click")
app$setInputs(Grocery_List_delete = "click")
# should dis-allow, as current logic only allows delete if quantity = 0
app$snapshot(items = list(export = TRUE), screenshot = TRUE)
app$executeScript("$('.modal').modal('hide');") # close modal
app$setInputs(Grocery_Listdt_rows_selected = 3, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_Listdt_rows_last_clicked = 3, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_List_edit = "click")
app$setInputs(Grocery_List_edit_Quantity = 0)
# current logic only allows delete if quantity = 0
app$setInputs(Grocery_List_update = "click")
app$setInputs(Grocery_List_update = "click")
app$setInputs(Grocery_Listdt_rows_selected = 3,
              allowInputNoBinding_ = TRUE, wait_ = FALSE, values_ = FALSE)
app$setInputs(Grocery_Listdt_rows_last_clicked = 3,
              allowInputNoBinding_ = TRUE, wait_ = FALSE, values_ = FALSE)
# the above two lines aren't actually expected to change the state
app$setInputs(Grocery_List_remove = "click")
app$setInputs(Grocery_List_delete = "click")
Sys.sleep(3)
app$snapshot(items = list(export = TRUE), screenshot = TRUE)
app$setInputs(Grocery_Listdt_rows_selected = 4, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_Listdt_rows_last_clicked = 4, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_List_edit = "click")
app$setInputs(Grocery_List_edit_Quantity = -2)
# should dis-allow negative quantity
app$setInputs(Grocery_List_update = "click")
app$executeScript("$('.modal').modal('hide');") # close modal
Sys.sleep(3)
app$executeScript('document.getElementById("addOne_1").click()')
app$executeScript('document.getElementById("subtractOne_2").click()')
# directly 'clicks' on the action buttons with Javascript
Sys.sleep(2)
app$snapshot(items = list(export = TRUE), screenshot = TRUE)

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/
#  Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
