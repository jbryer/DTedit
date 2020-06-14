app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$setInputs(Grocery_List_add = "click")
app$setInputs(Grocery_List_add_Buy = "Cheese",
              Grocery_List_add_Type = "Processed",
              Grocery_List_add_BuyFor = c("Carly", "Bob"),
              Grocery_List_add_Timeframe = "Soon",
              Grocery_List_add_Quantity = 2)
app$setInputs(Grocery_List_insert = "click")
Sys.sleep(2)

app$setInputs(choice = "2") # expanded choice list

app$setInputs(Grocery_List_add = "click")
app$setInputs(Grocery_List_add_Buy = "Coffee",
              Grocery_List_add_BuyFor = "Carly",
              Grocery_List_add_Timeframe = "Today",
              Grocery_List_add_Quantity = 1)
app$setInputs(Grocery_List_insert = "click")

Sys.sleep(2)

app$setInputs(Grocery_Listdt_rows_selected = 3, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_Listdt_rows_last_clicked = 3, allowInputNoBinding_ = TRUE)
app$setInputs(Grocery_List_copy = "click")
app$setInputs(Grocery_List_add_Buy = "Pears",
              Grocery_List_add_Timeframe = "Flexible",
              Grocery_List_add_Quantity = 1)
app$setInputs(Grocery_List_insert = "click")

Sys.sleep(1.5)
app$snapshot(items = list(export = TRUE), screenshot = TRUE)

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/
#  Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()