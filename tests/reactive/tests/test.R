app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$snapshot(items = list(export = TRUE))
app$setInputs(less = "click")
app$snapshot(items = list(export = TRUE))
app$setInputs(less = "click")
app$snapshot(items = list(export = TRUE))
app$setInputs(more = "click")
app$setInputs(more = "click")
app$setInputs(more = "click")
app$snapshot(items = list(export = TRUE))

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/ Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
