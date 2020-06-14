app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$setInputs(Password_List_add = "click")
app$setInputs(Password_List_add_Name = "Fred",
              Password_List_add_Password = "my name is fred")
app$setInputs(Password_List_insert = "click")

Sys.sleep(2)
app$setInputs(Password_Listdt_rows_selected = 2, allowInputNoBinding_ = TRUE)
app$setInputs(Password_Listdt_rows_last_clicked = 2, allowInputNoBinding_ = TRUE)
app$setInputs(Password_List_edit = "click")
app$setInputs(Password_List_edit_Name = "Eric",
              Password_List_edit_Password = "abracadabra")
app$setInputs(Password_List_update = "click", wait_ = FALSE, values_ = FALSE)
app$setInputs(Password_List_update = "click")

Sys.sleep(2)
app$snapshot(items = list(export = TRUE), screenshot = TRUE)

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/
#  Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()