app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$snapshot(items = list(export = TRUE))
app$setInputs(booksdt_rows_selected = 3, allowInputNoBinding_ = TRUE)
app$setInputs(booksdt_rows_last_clicked = 3, allowInputNoBinding_ = TRUE)
app$setInputs(books_edit = "click")
app$setInputs(books_edit_Publisher = "Wadsworth & Brooks",
              books_edit_Title = "S: An Interactive Environment for Data Analysis and Graphics",
              books_edit_Authors = c("Chambers, J.M.", "Becker, R.A.", "Hastie, T.J."),
              books_edit_Date = "1984-01-11")
app$setInputs(books_update = "click")
Sys.sleep(2)
app$snapshot(items = list(export = TRUE))
app$setInputs(books_add = "click")
app$setInputs(books_add_Authors = character(0))
app$setInputs(books_add_Publisher = "Springer",
              books_add_Title = "Things a Computer Scientist Rarely Talks About",
              books_add_Authors = c("Becker, R.A.", "Wilks, A.R."),
              books_add_Date = "2001-06-10")
app$setInputs(books_insert = "click")
Sys.sleep(2)
app$snapshot(items = list(export = TRUE))
app$setInputs(names_add = "click")
app$setInputs(names_add_Name = "John Doe",
              names_add_Email = "jd@jd.com",
              names_add_Date = "2020-06-01",
              names_add_Type = "User")
app$setInputs(names_insert = "click")
Sys.sleep(2)
app$snapshot(items = list(export = TRUE))

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/ Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
