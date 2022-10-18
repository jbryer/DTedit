app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$setInputs(names_add = "click")
app$setInputs(names_add_Type = "Admin",
              names_add_Name = "Jane Elias",
              names_add_Email = "je@alibaba.com",
              names_add_Date = "2020-06-17",
              names_add_Like = c("Apple", "Pear"))
app$setInputs(names_insert = "click")
Sys.sleep(2)

app$setInputs(`names.Type_add` = "click")
app$setInputs(`names.Type_add_Types` = "Visitor")
app$setInputs(`names.Type_insert` = "click")
Sys.sleep(2)

app$setInputs(`names.Like_add` = "click")
app$setInputs(`names.Like_add_Likes` = "Oranges")
app$setInputs(`names.Like_insert` = "click")
Sys.sleep(2)

app$setInputs(names_add = "click")
app$setInputs(names_add_Name = "John Doe",
              names_add_Email = "jd@jd.com",
              names_add_Date = "2020-06-01",
              names_add_Type = "Visitor",
              names_add_Like = c("Apple", "Pear", "Oranges"))
app$setInputs(names_insert = "click")
Sys.sleep(2)

app$setInputs(namesdt_rows_selected = 1, allowInputNoBinding_ = TRUE)
app$setInputs(namesdt_rows_last_clicked = 1, allowInputNoBinding_ = TRUE)
app$setInputs(names_copy = "click")
app$setInputs(names_add_Name = "Augusta Ada",
              names_add_Email = "ada@defence.gov",
              names_add_Date = "2020-07-11",
              names_add_Like = c("Apple", "Pear"))
app$setInputs(names_insert = "click")

Sys.sleep(2)
app$snapshot(items = list(export = TRUE))

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/
#  Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
