app <- ShinyDriver$new("../", seed = 123456)
# RNGkind(sample.kind = "Rounding") # the old random sampling method
# data_scramble results in a random event, so need to set seed
app$snapshotInit("test")

app$setInputs(data_scramble = "click")
app$setInputs(data_scramble = "click")
app$setInputs(data_scramble = "click")
app$setInputs(data_scramble = "click")
app$setInputs(data_scramble = "click")
app$setInputs(data_scramble = "click")
app$snapshot(items = list(export = TRUE))

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/ Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
