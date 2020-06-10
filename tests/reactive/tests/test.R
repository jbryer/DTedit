app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$snapshot(items = list(export = TRUE), screenshot = TRUE)
app$setInputs(less = "click")
app$snapshot(items = list(export = TRUE), screenshot = TRUE)
app$setInputs(less = "click")
app$snapshot(items = list(export = TRUE), screenshot = TRUE)
app$setInputs(more = "click")
app$setInputs(more = "click")
app$setInputs(more = "click")
app$snapshot(items = list(export = TRUE), screenshot = TRUE)

# wait for the process to close gracefully
# this allows covr to write out the coverage results
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
