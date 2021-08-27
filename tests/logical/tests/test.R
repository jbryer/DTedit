app <- ShinyDriver$new("..")

app$snapshotInit("test")

app$setInputs(Logical_Listdt_rows_selected = 1, allowInputNoBinding_ = TRUE)
app$setInputs(Logical_Listdt_rows_last_clicked = 1, allowInputNoBinding_ = TRUE)
app$setInputs(Logical_List_edit = "click")
app$setInputs(Logical_List_edit_checkvalue = FALSE)
app$setInputs(Logical_List_update = "click")
Sys.sleep(2)

app$snapshot(items = list(export = TRUE))
app$setInputs(Logical_Listdt_rows_selected = 2, allowInputNoBinding_ = TRUE)
app$setInputs(Logical_Listdt_rows_last_clicked = 2, allowInputNoBinding_ = TRUE)
app$setInputs(Logical_List_edit = "click")
app$setInputs(Logical_List_edit_checkvalue = TRUE)
app$setInputs(Logical_List_update = "click")
Sys.sleep(2)

app$setInputs(Logical_Listdt_rows_selected = 3, allowInputNoBinding_ = TRUE)
app$setInputs(Logical_Listdt_rows_last_clicked = 3, allowInputNoBinding_ = TRUE)
app$setInputs(Logical_List_remove = "click")
Sys.sleep(2)
app$setInputs(Logical_List_delete = "click")

app$setInputs(Logical_Listdt_rows_selected = 4, allowInputNoBinding_ = TRUE)
app$setInputs(Logical_Listdt_rows_last_clicked = 4, allowInputNoBinding_ = TRUE)
app$setInputs(Logical_List_copy = "click")
app$setInputs(Logical_List_add_checkvalue = FALSE)
Sys.sleep(2)
app$setInputs(Logical_List_insert = "click")

app$setInputs(Logical_List_add = "click")
app$setInputs(Logical_List_add_checkvalue = TRUE)
Sys.sleep(2)
app$setInputs(Logical_List_insert = "click")
Sys.sleep(2)

app$snapshot(items = list(export = TRUE))

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/ Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
