<!-- badges: start -->
[![Travis build status](https://travis-ci.org/DavidPatShuiFong/DTedit.svg?branch=develop)](https://travis-ci.org/DavidPatShuiFong/DTedit?branch=develop)
[![Codecov test coverage](https://codecov.io/gh/DavidPatShuiFong/DTedit/branch/develop/graph/badge.svg)](https://codecov.io/gh/DavidPatShuiFong/DTedit?branch=develop)
<!-- badges: end -->

## Editable DataTables for shiny apps.

**Author:** Jason Bryer, Ph.D.
**Email:** jason@bryer.org

**Author:** David Fong, FRACGP
**Email:** vkelim@bigpond.com

Use the `devtools` package to install the development version of `DTedit`:

```r
devtools::install_github('jbryer/DTedit')
```

The `dtedit_demo` will run a sample `shiny` app with to editable data tables.

```r
DTedit::dtedit_demo()
```

![DTedit Screen Shot](inst/screens/dtedit_books_edit.png)

#### Getting Started with `DTedit`

You can download a simple shiny app using `DTedit` here: [inst/template/app.R](inst/template/app.R)

There are three steps to using `DTedit` in your shiny application.

1. Define callback function for inserting, updating, and deleting data.

```r
my.insert.callback <- function(data, row) {
  # 'data' contains the dataframe *after* the row has been inserted/added
  # 'row' is the row number where data has been inserted
  mydata <<- rbind(mydata, data[row,])
  # in this case, 'mydata' should just be the same as 'data'
  return(mydata)
}

my.update.callback <- function(data, olddata, row) {
  # 'data' contains the dataframe *after* the row has been updated
  # 'row' is the row number where data has been updated
  # 'olddata' is the previous version of the data
  mydata[row,] <<- data[row,]
  # in this case, 'mydata' should just be the same as 'data'
  return(mydata)
}

my.delete.callback <- function(data, row) {
  # 'data' contains the dataframe *before* the row has been deleted
  # 'row' is the row number where data is to be deleted
  mydata <<- mydata[-row,]
  # in this case, 'mydata' should just be the same as data[-c(row),]
  return(mydata)
}
```

Typically these functions would interact with a database. As written here, the data would be lost between shiny sessions.

2. Create the `dtedit` object within your `server` function. 

```r
DTedit::dtedit(
  input, output,
  name = 'mycontacts',
  thedata = mydata,
  edit.cols = c('name', 'email', 'useR', 'notes'),
  edit.label.cols = c('Name', 'Email Address', 'Are they an R user?', 'Additional notes'),
  input.types = c(notes='textAreaInput'),
  view.cols = c('name', 'email', 'useR'),
  callback.update = my.update.callback,
  callback.insert = my.insert.callback,
  callback.delete = my.delete.callback
  )
```

The `input` and `output` are passed from the `server` function. The `name` parameter will define the name of the object available to the `uiOutput`. The `thedata` is a `data.frame` for the initial view of the data table. This can be an empty (i.e. no rows) `data.frame`. The structure of the `data.frame` will define the inputs (e.g. `factor`s will be drop down, `Date` will use `dateInput`, `numeric`s will use `numericInput`, etc.). There are many other parameters to custom the behavior of `dtedit`, see `?dtedit` for the full list.

3. Use `uiOutput` in your UI to display the editable data table.

The `name` you will pass to `uiOutput` is the name you passed to the `dtedit` created on the server side.

```r
uiOutput('mycontacts')
```

#### `dteditmod` (modularized version of `dtedit`)

You can download a simple modularized shiny app using `DTedit` here: [inst/template/app_mod.R](inst/template/app_mod.R)

There are three steps to using `DTedit` in your modularized shiny application.

1. Define callback functions, as for `dtedit`.

2. `callModule` in the server function, with `dteditmod` as the shiny module server function.

```r
return_values <- callModule(
  DTedit::dteditmod,
  id = 'mycontacts',
  thedata = mydata,
  edit.cols = c('name', 'email', 'useR', 'notes'),
  edit.label.cols = c('Name', 'Email Address', 'Are they an R user?', 'Additional notes'),
  input.types = c(notes='textAreaInput'),
  view.cols = c('name', 'email', 'useR'),
  callback.update = my.update.callback,
  callback.insert = my.insert.callback,
  callback.delete = my.delete.callback
  )
```

The `id` parameter will define the name of the object available to the `dteditmodUI`. `thedata` is a `data.frame` for the initial view of the data table. This can be an empty (i.e. no rows) `data.frame`.

3. Use `DTedit::dteditmodUI` in your UI to display the editable data table.

The `id` you will pass to `dteditmodUI` is the id you passed to the `callModule` created on the server side.

```r
dteditmodUI('mycontacts')
```

#### Vignette

See vignette with `browseVignettes("DTedit")` or [on RPubs](https://rpubs.com/DavidFong/DTedit). 

#### More examples

A very simple example : [inst/examples/example.R](inst/examples/example.R).<br/>
This example can be seen with `example("dtedit")`

Example with two datatables : [inst/shiny_demo/app.R](inst/shiny_demo/app.R).<br/>
This demonstration can be seen with `DTedit::dtedit_demo()`.<br/>
This, and other demonstrations, can be called with `shinyAppDir` arguments.<br/>
e.g. `DTedit::dtedit_demo(options = list(display.mode = "showcase"))`<br/>
Note that *all* `.R` files in the `shiny_demo` directory are displayed in showcase mode, not just the `.R` file used in the actual demonstration!

Example with reactive dataframe : [inst/examples/example_reactivedataframe.R](inst/examples/example_reactivedataframe.R).<br/>
This demonstration can be seen with `DTedit::dtedit_reactive_demo()`

Example with reactive input choices : [inst/examples/example_selectInputReactive.R](inst/examples/example_selectInputReactive.R).<br/>
This demonstration can be seen with `DTedit::dtedit_selectInputReactive_demo()`

A very simple modularized example : [inst/examples/example_mod.R](inst/examples/example_mod.R).<br/>

A modularized example with reactive dataframes : [inst/examples/example_mod_reactivedataframe.R](inst/examples/example_mod_reactivedataframe.R).<br/>
This example can be seen with `example("dteditmodUI")`

A modularized version with multiple datatables and reactive input choices : [inst/shiny_demo/app_mod.R](inst/shiny_demo/app_mod.R).<br/>
This demonstration can be seen with `DTedit::dteditmod_demo()`

A modularized version with fileInput, binary blobs and action buttons : [inst/examples/fileinput_modular/app.R](inst/examples/fileinput_modular/app.R).<br/>
This demonstration can be seen with `DTedit::dteditmod_fileInput_demo()`
