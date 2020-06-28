app <- ShinyDriver$new("../")
app$snapshotInit("test")

app$snapshot(items = list(export = TRUE))
app$setInputs(
  `Grocery_List-editdtdt_rows_selected` = 1, allowInputNoBinding_ = TRUE
)
app$setInputs(
  `Grocery_List-editdtdt_rows_last_clicked` = 1, allowInputNoBinding_ = TRUE
)
app$setInputs(`Grocery_List-editdt_edit` = "click")
app$uploadFile(`Grocery_List-editdt_edit_Picture` = "bee_agapanthus.jpg")
# <-- This should be the path to the file,
# relative to the app's tests/ directory
app$uploadFile(
  `Grocery_List-editdt_edit_Spreadsheet` = "DiabetesSIP_Report.csv"
  )
app$setInputs(`Grocery_List-editdt_update` = "click")
Sys.sleep(1.5)

app$setInputs(`Grocery_List-editdt_add` = "click")
app$setInputs(`Grocery_List-editdt_add_Buy` = "Locquats",
              `Grocery_List-editdt_add_Quantity` = 3)
app$uploadFile(`Grocery_List-editdt_add_Picture` = "epiphyllum.jpg")
app$uploadFile(`Grocery_List-editdt_add_Spreadsheet` = "Zostavax_Report.csv")
app$setInputs(`Grocery_List-editdt_insert` = "click")
Sys.sleep(1.5)

app$executeScript('document.getElementById("picture_1").click()')
app$executeScript('document.getElementById("spreadsheet_1").click()')
Sys.sleep(1.5)
app$snapshot(items = list(export = "spreadsheet_list"))

app$executeScript('document.getElementById("picture_4").click()')
app$executeScript('document.getElementById("spreadsheet_4").click()')
Sys.sleep(1.5)
app$snapshot(items = list(export = "spreadsheet_list"))
# on the third snapshot, just check spreadsheet_list
# because thedata is a large JSON (with two JPGs, total 1.2 MB)

# wait for the process to close gracefully
# this allows covr to write out the coverage results
#  https://github.com/rfaelens/exampleShinyTest/ Ruben Faelens
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
