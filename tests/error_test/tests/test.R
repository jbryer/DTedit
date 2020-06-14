app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$setInputs(Grocery_List_add = "click")
app$setInputs(Grocery_List_insert = "click")
app$executeScript("$('.modal').modal('hide');") # close modal

# the following 'add/insert' will create warnings
# due to lack of definition of choices

app$setInputs(NoChoice_selectInput_add = "click")
app$setInputs(NoChoice_selectInput_insert = "click")
app$executeScript("$('.modal').modal('hide');") # close modal
app$setInputs(NoChoice_selectInputReactive_add = "click")
app$setInputs(NoChoice_selectInputReactive_insert = "click")
app$executeScript("$('.modal').modal('hide');") # close modal
app$setInputs(NoChoice_selectInputMultiple_add = "click")
app$setInputs(NoChoice_selectInputMultiple_insert = "click")
app$executeScript("$('.modal').modal('hide');") # close modal
app$setInputs(NoChoice_selectInputMultipleReactive_add = "click")
app$setInputs(NoChoice_selectInputMultipleReactive_insert = "click")
app$executeScript("$('.modal').modal('hide');") # close modal

Sys.sleep(1)
x <- app$getDebugLog()[[3]]
no_choice_warnings <- x[grep("No choices", x)]
# retrieves all the warning message 'No choices'

testthat::expect_identical(
  no_choice_warnings[[1]],
  "  No choices available for Buy. Specify them using the input.choices parameter"
)
testthat::expect_identical(
  no_choice_warnings[[2]],
  "  No choices available for Buy. Specify them using the input.choices and input.choices.reactive parameter"
)
testthat::expect_identical(
  no_choice_warnings[[3]],
  "  No choices available for Buy. Specify them using the input.choices parameter"
)
testthat::expect_identical(
  no_choice_warnings[[4]],
  "  No choices available for Buy. Specify them using the input.choices and input.choices.reactive parameter"
)

Sys.sleep(2)
app$snapshot(items = list(export = TRUE), screenshot = TRUE)

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/
#  Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()