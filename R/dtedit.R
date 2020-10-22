#' Create a DataTable with Add, Edit and Delete buttons.
#'
#' dtedit - editable DataTable
#'
#' \code{dtedit} is used in conjunction with \code{uiOutput} to create editable datatables.
#' \code{dtedit} is used in a shiny application's server definition, \code{uiOutput} is used
#' in the UI (user interface) definition.
#'
#' @param input Shiny input object passed from the server.
#' @param output Shiny output object passed from the server.
#' @param name (\code{name} is available in \code{dtedit} only). The \code{name} of the
#'   outputted editable datatable. The \code{name} passed to \code{dtedit} is the same
#'   as the name passed to \code{uiOutput}. Put \code{uiOutput(name)} where you want the
#'   editable datatable in the \code{ui.R}. When using more than one \code{dtedit} within a Shiny
#'   application the name must be unique. (\code{name} is converted
#'   to the \code{session} argument of dteditmod.)
#' @param ... \code{dtedit} passes options to \code{dteditmod},
#'   re-labelling \code{name} to \code{session}.
#'   Extra options not defined by \code{dteditmod} are passed to \code{DT::renderDataTable}.
#'
#' @family Datatable Edit functions
#' @seealso
#'
#'  \itemize{
#'  \item \code{example("dtedit")} a simple example.
#'  \item \code{dtedit_demo()} demonstration of dtedit.
#'  \item \code{dtedit_reactive_demo()} reactive dataframe
#'  \item \code{dtedit_selectInputReactive_demo()} reactive selectInput
#'  }
#'
#' @example inst/examples/example.R
#'
#' @export
dtedit <- function(input, output,
                   name,
                   thedata,
                   ...) {
  dteditmod(input, output, session = name, thedata = thedata, ...)
}

#' Create a DataTable with Add, Edit and Delete buttons.
#'
#' dteditmod - editable DataTable, adapted for use in modules
#'
#' \code{dteditmod} is used in conjunction with \code{callModule} and
#' \code{dteditmodUI} to create editable datatables in a module environment.
#' \code{dteditmod} is called through \code{callModule} in the 'server' section of
#' the shiny application.
#' \code{dteditmodUI} is called in the 'UI' (user-interface) section of the shiny app.
#'
#' This object will maintain data state. However, in order of the data to persist
#' between Shiny instances, data needs to be saved to some external format (e.g.
#' database or R data file). The callback functions provide a mechanism for this
#' function to interact with a permanent data storage scheme. The callback
#' functions are called when the user adds, updates, or deletes a row from the
#' data table. The callback must accept two parameters: \code{data} and \code{row}.
#' For inserting and updating, the \code{data} object is the current state of
#' data table including any additions or updates. The \code{row} parameter indicates
#' which row from \code{data} was modified (or added). For deletions, however,
#' the \code{data} represents the data table just before deleting the specified
#' row. That is, if \code{callback.delete} returns a \code{data.frame}, that will
#' be the new data table; otherwise this function will remove row \code{row} from
#' \code{data} and that will become the current data table.
#'
#' The callback functions may throw errors (see e.g. \code{stop}) if there are
#' problems with data. That is, if data validation checks indicate data problems
#' before inserting or updating a row the function may throw an error. Note that
#' the error message will be presented to the user so providing messages
#' meaningful to the user is recommended. Moreover, if an error is thrown, the
#' modal dialog is not dismissed and the user can further edit the data and
#' retry the insertion or update.
#'
#' Callback functions may return a \code{data.frame}. When a \code{data.frame} is
#' returned that will become the current state of the data table. If anything
#' else is returned then the internal \code{data.frame} will be used.
#'
#' @md
#'
#' @return Returns reactiveValues
#'  * `theData` - the current state of `DTedit`'s copy of the data
#'  * `view.cols`
#'  * `edit.cols`
#'  * `edit.count` - number of edits to data done within `DTedit` (does not
#'     include changes to `DTedit`'s copy of the data secondary to changes
#'     of a reactive `thedata`)
#'  * `rows_selected` - the row number selected. initially set to `NULL`
#'
#' @param input Shiny input object passed from the server.
#' @param output Shiny output object passed from the server.
#' @param session Shiny session object (an environment) passed from the server.
#'        Alternatively, the 'name' (character) of the outputted editable datatable.
#' @param thedata a data frame to view and edit. can be a reactive
#' @param view.cols character vector with the column names to show in the DataTable.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.cols character vector with the column names the user can edit/add.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.label.cols character vector with the labels to use on the edit
#'        and add dialogs. The length and order of \code{code.cols.labels} must
#'        correspond to \code{edit.cols}.
#' @param delete.info.cols character vector with the column names specifying
#'        which values are presented on the delete dialog.
#'        This can be a subset of the full \code{data.frame}. Defaults to \code{view.cols}.
#'        If \code{NULL}, no data values are shown on the delete dialog.
#' @param delete.info.label.cols character vector with the labels to use on the delete
#'        dialog. The length and order of \code{delete.info.label.cols} must
#'        correspond to \code{delete.info.cols}.
#' @param input.types a character vector where the name corresponds to a column
#'   in \code{edit.cols} and the value is the input type. Possible values
#'   are:
#'
#'  * `dateInput` - input changed by `date.width`
#'  * `datetimeInput` - input changed by `datetime.width`. needs
#'      `useairDatepicker` set to `TRUE` and requires package `shinyWidgets`.
#'  * `selectInput` - choices determined by `input.choices`,
#'    or the levels of the data column
#'    variable (if the column variable is of class `factor`),
#'    or the already present values in the data column.
#'  * `selectInputMultiple` - choices determined by
#'    `input.choices` or the already present values in the data
#'    column.
#'  * `selectInputReactive` - choices determined by a reactive
#'    variable, as defined by `input.choices` and
#'    `input.choices.reactive`.
#'  * `selectInputMultipleReactive` - choices determined by a
#'    reactive variable, as defined by `input.choices` and
#'    `input.choices.reactive`
#'  * `numericInput` - input changed by `numeric.width`
#'  * `textInput` - input changed by `text.width`
#'  * `textAreaInput` - input changed by `textarea.width` and `textarea.height`
#'  * `passwordInput`
#'  * `fileInput` - type of acceptable file types is defined by
#'    `input.choices`. Maximum file length is modifed by
#'    `max.fileInputLength`
#'
#'  One case where this parameter is desirable is when a text
#'  area is required instead of a simple text input.
#'
#' @param input.choices a list of character vectors. The names of each element
#'  in the list must correspond to a column name in the data. The value,
#'  a character vector, are the options presented to the user for data entry,
#'  in the case of input type \code{selectInput}).
#'
#'  In the case of input type `selectInputReactive`
#'  or `selectInputMultipleReactive``, the value is the name
#'  of the reactive in 'input.choices.reactive'
#'
#'  In the case of input type `fileInput`` this is the
#'  'accept' argument, which specifies the type of file which
#'  is acceptable. Can be a case insensitive file extension
#'  (e.g. '.csv' or '.rds') or a MIME type (e.g. 'text/plain' or
#'  'application/pdf').
#' @param input.choices.reactive a named list of reactives, referenced in 'input.choices'
#'  to use for input type \code{selectInputReactive} or \code{selectInputMultipleReactive}.
#'  The reactive itself is a character vector.
#' @param inputEvent a named list of functions. The names of each element in
#'  the list must correspond to an editable column name in the data. The
#'  function is called when the associated input widget event is observed
#'  during editing/adding a data row. Can be used, for example,
#'  with `shinyFeedback`.
#' @param action.buttons a named list of action button columns.
#'  Each column description is a list of \code{columnLabel}, \code{buttonLabel},
#'  \code{buttonPrefix}, \code{afterColumn}.
#'  * \code{columnLabel} label used for the column.
#'  * \code{buttonLabel} label used for each button
#'  * \code{buttonPrefix} used as the prefix for action button IDs.
#'    The suffix will be a number from '1' to the number of rows.
#'    Prefix and suffix will be separated with an underscore '_'.
#'  * \code{afterColumn} if provided, the action button column is
#'    placed after this named column.
#' @param selectize Whether to use selectize.js or not. See \code{\link{selectInput}} for more info.
#' @param defaultPageLength number of rows to show in the data table by default.
#' @param modal.size the size of the modal dialog. See \code{\link{modalDialog}}.
#' @param text.width width of text inputs.
#' @param textarea.width the width of text area inputs.
#' @param textarea.height the height of text area inputs.
#' @param date.width the width of data inputs
#' @param datetime.width the width of datetime inputs
#' @param numeric.width the width of numeric inputs.
#' @param select.width the width of drop down inputs.
#' @param max.fileInputLength the maximum length (in bytes) of \code{fileInput}.
#'  Shiny itself has a default limit of 5 megabytes per file.
#'  The limit can be modified by using shiny.maxRequestSize option.
#' @param title.delete the title of the dialog box for deleting a row.
#' @param title.edit the title of the dialog box for editing a row.
#' @param title.add the title of the dialog box for inserting a new row.
#' @param label.delete the label of the delete button.
#' @param label.edit the label of the edit button.
#' @param label.add the label of the add button.
#' @param label.copy the label of the copy button.
#' @param label.save the label of the save button.
#' @param label.cancel the label of the cancel button.
#' @param icon.delete the icon for the delete button, e.g. \code{icon("trash")}. Defaults to \code{NULL}.
#' @param icon.edit the icon for the edit button, e.g. \code{icon("edit")}. Defaults to \code{NULL}.
#' @param icon.add the icon for the add button, e.g. \code{icon("plus")}. Defaults to \code{NULL}.
#' @param icon.copy the icon for the copy button, e.g. \code{icon("copy")}. Defaults to \code{NULL}.
#' @param text.delete.modal the text shown in the delete modal dialog.
#' @param show.delete whether to show/enable the delete button.
#' @param show.update whether to show/enable the update button.
#' @param show.insert whether to show/enable the insert button.
#' @param show.copy whether to show/enable the copy button.
#' @param callback.delete a function called when the user deletes a row.
#'  This function should return an updated data.frame.
#' @param callback.update a function called when the user updates a row.
#'  This function should return an updated data.frame.
#' @param callback.insert a function called when the user inserts a new row.
#'  This function should return an updated data.frame.
#' @param callback.actionButton a function called when the user clicks an action button.
#'  called with arguments `data`, `row` and `buttonID`.
#'  This function can return an updated data.frame,
#'  alternatively return NULL if data is not to be changed.
#' @param click.time.threshold This is to prevent duplicate entries usually by double clicking the
#'  save or update buttons. If the user clicks the save button again within this amount of
#'  time (in seconds), the subsequent click will be ignored. Set to zero to disable this
#'  feature. For developers, a message is printed using the warning function.
#' @param useairDatepicker use `shinyWidgets` package `airDatepickerInput`
#' @param datatable.options options passed to \code{DT::renderDataTable}.
#'  See \url{https://rstudio.github.io/DT/options.html} for more information.
#' @param datatable.rownames show rownames as part of the datatable? `TRUE` or `FALSE`.
#'  Note that if datatable.call includes `DT::format*` calls,
#'  then `datatable.rownames` must equal `TRUE`
#' @param datatable.call pre-processing call when calling `DT::renderDataTable`. Can be defined,
#'  for example, to include `DT::format*` calls.
#'  `dtedit` will pass several arguments to the `datatable.call` function.
#'  * `data` a dataframe. may have been processed to add `actionButtons`
#'  * `options` - `datatable.options`
#'  * `rownames` - `datatable.rownames`
#'  * `escape` - escape all columns except those with action buttons.
#'  * `selection` - `single`
#' @param ... arguments not recognized by DTedit are passed to \code{DT::renderDataTable}
#'  By default, `datatable.call` uses `DT::dataframe`, so this limits the options that
#'  can be passed through this method.
#'
#' @seealso
#'
#'  \itemize{
#'  \item \code{\link{dteditmodUI}} : the companion user-interface function for \code{dteditmod}.\cr
#'  \item \code{example("dteditmodUI")} a simple module example with reactive dataframe
#'  \item \code{dteditmod_demo()} a more complex module example. Database interaction
#'    and interactions between the data of multiple datatables.
#'  \item \code{dteditmod_fileInput_demo()} a modular example including binary file input and action buttons.
#'  }
#'
#' @rdname dtedit
#'
#' @example inst/examples/example_mod.R
#'
#' @export
dteditmod <- function(input, output, session,
                      thedata,
                      view.cols = names(
                        shiny::isolate(
                          if (shiny::is.reactive(thedata)) {
                            thedata()
                          } else {
                            thedata
                          }
                        )
                      ),
                      edit.cols = names(
                        shiny::isolate(
                          if (shiny::is.reactive(thedata)) {
                            thedata()
                          } else {
                            thedata
                          }
                        )
                      ),
                      edit.label.cols = edit.cols,
                      delete.info.cols = view.cols,
                      delete.info.label.cols = delete.info.cols,
                      input.types,
                      input.choices = NULL,
                      input.choices.reactive = NULL,
                      inputEvent = NULL,
                      action.buttons = NULL,
                      selectize = TRUE,
                      modal.size = "m",
                      text.width = "100%",
                      textarea.width = "570px",
                      textarea.height = "200px",
                      date.width = "100px",
                      datetime.width = "200px",
                      numeric.width = "100px",
                      select.width = "100%",
                      defaultPageLength = 10,
                      max.fileInputLength = 100000000,
                      title.delete = "Delete",
                      title.edit = "Edit",
                      title.add = "New",
                      label.delete = "Delete",
                      label.edit = "Edit",
                      label.add = "New",
                      label.copy = "Copy",
                      label.save = "Save",
                      label.cancel = "Cancel",
                      icon.delete = NULL,
                      icon.edit = NULL,
                      icon.add = NULL,
                      icon.copy = NULL,
                      text.delete.modal =
                        "Are you sure you want to delete this record?",
                      show.delete = TRUE,
                      show.update = TRUE,
                      show.insert = TRUE,
                      show.copy = TRUE,
                      callback.delete = function(data, row) { },
                      callback.update = function(data, olddata, row) { },
                      callback.insert = function(data, row) { },
                      callback.actionButton = function(data, row, buttonID) { },
                      click.time.threshold = 2, # in seconds
                      useairDatepicker = FALSE,
                      datatable.options = list(pageLength = defaultPageLength),
                      datatable.rownames = FALSE,
                      datatable.call = function(...) {DT::datatable(...)},
                      ...) {
  if (!missing(session) && is.environment(session)) {
    # the function has been called as a module
    ns <- session$ns
    name <- "editdt"
    moduleMode <- TRUE # in 'module' mode
  } else if (is.character(session)) {
    # the function has not been called as a module
    # and 'session' is a character string,
    # then 'session' is the 'name' of the output
    name <- session
    ns <- function(x) return(x)
    # 'ns' becomes a 'change nothing' function
    moduleMode <- FALSE # not in 'module' mode
  }

  thedataCopy <- if (shiny::is.reactive(shiny::isolate(thedata))) {
    shiny::isolate(thedata())
  } else {
    thedata
  }
  # if a reactive has been passed, obtain the value
  # Some basic parameter checking
  if (!is.data.frame(thedataCopy) || ncol(thedataCopy) < 1) {
    stop("Must provide a data frame with at least one column.")
  } else if (length(edit.cols) != length(edit.label.cols)) {
    stop("edit.cols and edit.label.cols must be the same length.")
  } else if (length(delete.info.cols) != length(delete.info.label.cols)) {
    stop("delete.info.cols and delete.info.label.cols must be the same length.")
  } else if (!all(view.cols %in% names(thedataCopy))) {
    stop("Not all view.cols are in the data.")
  } else if (!all(edit.cols %in% names(thedataCopy))) {
    stop("Not all edit.cols are in the data.")
  } else if (!all(delete.info.cols %in% names(thedataCopy))) {
    stop("Not all delete.info.cols are in the data.")
  }

  DataTableName <- paste0(name, "dt")

  result <- shiny::reactiveValues()
  result$thedata <- thedataCopy
  result$view.cols <- view.cols
  result$edit.cols <- edit.cols
  result$edit.count <- 0 # number of edits (Add/Delete/Edit/Copy) through dtedit
  result$rows_selected <- NULL # no row selected initially

  dt.proxy <- DT::dataTableProxy(DataTableName)

  selectInputMultiple <- function(...) {
    shiny::selectInput(multiple = TRUE, selectize = selectize, ...)
  }

  valid.input.types <- c(
    "dateInput", "datetimeInput", "selectInput", "numericInput",
    "textInput", "textAreaInput", "passwordInput", "selectInputMultiple",
    "selectInputReactive", "selectInputMultipleReactive", "fileInput"
  )
  inputTypes <- sapply(thedataCopy[, edit.cols], FUN = function(x) {
    switch(class(x)[[1]],
           list = "selectInputMultiple",
           character = "textInput",
           Date = "dateInput",
           POSIXct = "datetimeInput",
           factor = "selectInput",
           integer = "numericInput",
           numeric = "numericInput",
           blob = "fileInput"
    )
  })
  if ("datetimeInput" %in% inputTypes && !useairDatepicker) {
    # standard dateInput does not have a time picker
    stop (
      "'datetimeInput', or POSIXct types, are not available if 'useairDatepicker' is set to false."
    )
  }
  if (!missing(input.types)) {
    if (!all(names(input.types) %in% edit.cols)) {
      stop(
        "input.types column not a valid editing column: ",
        paste0(names(input.types)[!names(input.types) %in% edit.cols])
      )
    }
    if (!all(input.types %in% valid.input.types)) {
      stop(paste0(
        "input.types must only contain values of: ",
        paste0(valid.input.types, collapse = ", ")
      ))
    }
    inputTypes[names(input.types)] <- input.types
  }
  # Convert any list columns to characters before displaying
  for (i in seq_len(ncol(thedataCopy))) {
    if (nrow(thedataCopy) == 0) {
      thedataCopy[, i] <- character()
    } else if (is.list(thedataCopy[, i])) {
      thedataCopy[, i] <- sapply(thedataCopy[, i], FUN = function(x) {
        paste0(x, collapse = ", ")
      })
    }
  }

  addActionButtons <- function(data, action.buttons) {
    # data : dataframe
    # action.buttons : named list of lists
    #  each list contains
    #   $columnLabel - the name of the created column
    #   $buttonLabel - the label to use for each button
    #   $buttonPrefix - the prefix of the button ID
    #    the suffix is a number
    #   $afterColumn - (optional)
    #    the name of the column after which created column is placed
    # returns list
    #  $dataframe
    #  $button.colNames - the column names of the action buttons

    # create a vector of shiny inputs
    # of length 'len'
    # input IDs have prefix 'id', a numeric suffix from '1' to 'len'
    #  separated by an underscore '_'
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, "_", i), ...))
      }
      return(inputs)
    }
    # https://stackoverflow.com/questions/45739303/
    #  r-shiny-handle-action-buttons-in-data-table
    # https://stackoverflow.com/questions/57969103/
    #  how-to-use-shiny-action-button-in-datatable-through-shiny-module
    # https://community.rstudio.com/t/
    #  how-to-use-shiny-action-button-in-datatable-through-shiny-module/39998
    # if used in a module environment, usage looks like :
    #  Actions = shinyInput(
    #   actionButton, 5,
    #   'button',
    #   label = "Fire",
    #   onclick = paste0('Shiny.setInputValue(\"' ,
    #    ns("select_button"),
    #    '\", this.id, {priority: \"event\"})')
    #   )

    view.cols.andButtons <- names(data) # used to store the order of columns
    # by default, view columns 'and buttons' are the same as view.cols
    button.colNames <- NULL # later will store vector of button column names
    if (!is.null(action.buttons)) {
      for (i in action.buttons) {
        button.colNames <- append(button.colNames, i$columnLabel)
        data[, i$columnLabel] <- shinyInput(
          shiny::actionButton,
          nrow(data),
          i$buttonPrefix,
          label = i$buttonLabel,
          onclick = paste0(
            'Shiny.setInputValue(\"',
            ns("select_button"),
            '\", this.id, {priority: \"event\"})'
          )
        )
        if (is.null(i$afterColumn)) {
          # this button column has no defined position,
          #  so place at end of view column vector
          view.cols.andButtons <- append(
            view.cols.andButtons,
            i$columnLabel
          )
        } else {
          # this button column has a defined position
          view.cols.andButtons <- append(
            view.cols.andButtons,
            i$columnLabel,
            after = which(view.cols.andButtons == i$afterColumn)
          )
        }
      }
    }
    data <- data[, view.cols.andButtons, drop = FALSE]
    # re-order columns as necessary
    # drop = FALSE necessary to stop converting single column
    #  data-frame to a vector
    return(list(data = data, button.colNames = button.colNames))
  }

  thedataWithButtons <- addActionButtons(
    thedataCopy[, view.cols, drop = FALSE], action.buttons
  )
  # was "thedata[,view.cols]", but requires drop=FALSE
  # to prevent return of vector (instead of dataframe)
  # if only one column in view.cols
  output[[DataTableName]] <- DT::renderDataTable({
    datatable.call(
      data = thedataWithButtons$data,
      options = datatable.options,
      rownames = datatable.rownames,
      escape = which(
        !names(thedataWithButtons$data) %in% thedataWithButtons$button.colNames
      ),
      # 'escaped' columns are those without HTML buttons etc.
      # escape the 'data' columns
      # but do not escape the columns which
      #  have been created by addActionButtons()
      selection = "single"
    )
  },
  server = TRUE,
  ...
  )
  outputOptions(output, DataTableName, suspendWhenHidden = FALSE)
  # without turning off suspendWhenHidden, changes are
  #  not rendered if containing tab is not visible

  # if a row is selected in the dataframe named in 'DataTableName'
  # then set result$rows_selected to that row
  # this will be returned to the caller
  shiny::observeEvent(
    input[[paste0(DataTableName, "_rows_selected")]], {
      result$rows_selected <- input[[paste0(DataTableName, "_rows_selected")]]
    })

  getFields <- function(typeName, values) {
    # creates input fields when adding or editing a row
    # 'typeName' is either '_add_' or '_edit_'
    # 'values' are current values of the row
    #  (if already existing, or being copied)
    # if adding a 'new' row, then 'values' will be 'missing'
    #
    # returns a list of shiny inputs, 'fields'

    fields <- list()
    for (i in seq_along(edit.cols)) {
      if (inputTypes[i] == "dateInput") {
        value <- ifelse(missing(values),
                        as.character(Sys.Date()),
                        as.character(values[, edit.cols[i]])
        )
        if (!useairDatepicker) {
          fields[[i]] <- shiny::dateInput(
            ns(paste0(name, typeName, edit.cols[i])),
            label = edit.label.cols[i],
            value = value,
            width = date.width
          )
        } else {
          fields[[i]] <- shinyWidgets::airDatepickerInput(
            ns(paste0(name, typeName, edit.cols[i])),
            label = edit.label.cols[i],
            value = value,
            timepicker = FALSE,
            addon = "none",
            width = date.width
          )
        }
      } else if (inputTypes[i] == "datetimeInput") {
        # note that this uses shinyWidgets::airDatepickerInput
        value <- ifelse(missing(values),
                        as.character(Sys.time()),
                        as.character(values[, edit.cols[i]])
        )
        fields[[i]] <- shinyWidgets::airDatepickerInput(
          ns(paste0(name, typeName, edit.cols[i])),
          label = edit.label.cols[i],
          value = value,
          timepicker = TRUE,
          addon = "none",
          width = datetime.width
        )
      } else if (inputTypes[i] == "selectInputMultiple") {
        value <- ifelse(missing(values), "", values[, edit.cols[i]])
        if (is.list(value)) {
          value <- value[[1]]
        }
        choices <- ""
        if (!is.null(input.choices) &&
            edit.cols[i] %in% names(input.choices)) {
          choices <- input.choices[[edit.cols[i]]]
        } else if (nrow(result$thedata) > 0) {
          choices <- unique(unlist(result$thedata[, edit.cols[i]]))
          # no choices explicitly defined
          #
          # use choices defined in other rows, if available
          # this is a bad choice. even if a column starts
          # with just 'Yes/No', it is quite possible that with
          # further table editing valid choices will become
          # unavailable if, after editing, no rows have that valid choice
          warning(paste0(
            "No choices explicitly defined for ", edit.cols[i],
            ". Specify them using the input.choices parameter"
          ))
        }
        if (length(choices) == 1 && choices[[1]] == "") {
          warning(paste0(
            "No choices available for ", edit.cols[i],
            ". Specify them using the input.choices parameter"
          ))
        }
        fields[[i]] <- selectInputMultiple(
          ns(paste0(name, typeName, edit.cols[i])),
          label = edit.label.cols[i],
          choices = choices,
          selected = value,
          width = select.width
        )
      } else if (inputTypes[i] == "selectInput") {
        value <- ifelse(
          missing(values),
          "",
          as.character(values[, edit.cols[i]])
        )
        if (is.list(value)) {
          value <- value[[1]]
        }
        choices <- ""
        if (!is.null(input.choices) &&
            edit.cols[i] %in% names(input.choices)) {
          choices <- input.choices[[edit.cols[i]]]
        } else if (is.factor(result$thedata[, edit.cols[i]])) {
          choices <- levels(result$thedata[, edit.cols[i]])
        } else if (nrow(result$thedata) > 0) {
          choices <- unique(unlist(result$thedata[, edit.cols[i]]))
          # no choices explicitly defined
          #
          # use choices defined in other rows, if available
          # this is a bad choice. even if a column starts
          # with just 'Yes/No', it is quite possible that with
          # further table editing valid choices will become
          # unavailable if, after editing, no rows have that valid choice
          warning(paste0(
            "No choices explicitly defined for ", edit.cols[i],
            ". Specify them using the input.choices parameter"
          ))
        }
        if (length(choices) == 1 && choices[[1]] == "") {
          warning(paste0(
            "No choices available for ", edit.cols[i],
            ". Specify them using the input.choices parameter"
          ))
        }
        fields[[i]] <- shiny::selectInput(
          ns(paste0(name, typeName, edit.cols[i])),
          label = edit.label.cols[i],
          choices = choices,
          selected = value,
          width = select.width
        )
      } else if (inputTypes[i] == "selectInputMultipleReactive") {
        value <- ifelse(missing(values), "", values[, edit.cols[i]])
        if (is.list(value)) {
          value <- value[[1]]
        }
        choices <- NULL
        if (!is.null(input.choices.reactive)) {
          if (edit.cols[i] %in% names(input.choices)) {
            choices <- input.choices.reactive[[input.choices[[edit.cols[i]]]]]()
            # it is the responsiblity of the
            #  calling functions/reactive variable handlers
            # that the list of choices includes all CURRENT choices
            #  that have already been chosen in the data.
          }
        }
        if (is.null(choices)) {
          warning(paste0(
            "No choices available for ", edit.cols[i], ". ",
            "Specify them using the input.choices and ",
            "input.choices.reactive parameter"
          ))
        }
        fields[[i]] <- selectInputMultiple(
          ns(paste0(name, typeName, edit.cols[i])),
          label = edit.label.cols[i],
          choices = choices,
          selected = value,
          width = select.width
        )
      } else if (inputTypes[i] == "selectInputReactive") {
        value <- ifelse(
          missing(values),
          "",
          as.character(values[, edit.cols[i]])
        )
        choices <- NULL
        if (!is.null(input.choices.reactive)) {
          if (edit.cols[i] %in% names(input.choices)) {
            choices <- input.choices.reactive[[input.choices[[edit.cols[i]]]]]()
            # it is the responsiblity of
            #  the calling functions/reactive variable handlers
            # that the list of choices includes all CURRENT choices
            #  that have already been chosen in the data.
          }
        }
        if (is.null(choices)) {
          warning(paste0(
            "No choices available for ", edit.cols[i], ". ",
            "Specify them using the input.choices and ",
            "input.choices.reactive parameter"
          ))
        }
        fields[[i]] <- shiny::selectInput(
          ns(paste0(name, typeName, edit.cols[i])),
          label = edit.label.cols[i],
          choices = choices,
          selected = value,
          width = select.width
        )
      } else if (inputTypes[i] == "numericInput") {
        value <- ifelse(missing(values), 0, values[, edit.cols[i]])
        fields[[i]] <- shiny::numericInput(
          ns(paste0(name, typeName, edit.cols[i])),
          label = edit.label.cols[i],
          value = value,
          width = numeric.width
        )
      } else if (inputTypes[i] == "textAreaInput") {
        value <- ifelse(missing(values), "", values[, edit.cols[i]])
        fields[[i]] <- shiny::textAreaInput(
          ns(paste0(name, typeName, edit.cols[i])),
          label = edit.label.cols[i],
          value = value,
          width = textarea.width, height = textarea.height
        )
      } else if (inputTypes[i] == "textInput") {
        value <- ifelse(missing(values), "", values[, edit.cols[i]])
        fields[[i]] <- shiny::textInput(
          ns(paste0(name, typeName, edit.cols[i])),
          label = edit.label.cols[i],
          value = value,
          width = text.width
        )
      } else if (inputTypes[i] == "passwordInput") {
        value <- ifelse(missing(values), "", values[, edit.cols[i]])
        fields[[i]] <- shiny::passwordInput(
          ns(paste0(name, typeName, edit.cols[i])),
          label = edit.label.cols[i],
          value = value,
          width = text.width
        )
      } else if (inputTypes[i] == "fileInput") {
        # current value(if not 'missing') is actually irrelevant!
        # will always present a file selector
        fields[[i]] <- shiny::fileInput(
          ns(paste0(name, typeName, edit.cols[i])),
          label = edit.label.cols[i],
          accept = input.choices[[edit.cols[i]]]
          # acceptable file input choices
          # e.g. case insensitive file extension '.csv'
          #      MIME types "text/plain"
        )
      } else {
        stop("Invalid input type!")
      }
    }
    return(fields)
  }

  output[[paste0(name, "_message")]] <- shiny::renderUI("")

  updateData <- function(proxy, data, ...) {
    # updates data displayed in DT datatable
    #
    # will reference 'global' action.buttons variable
    #  when callling function 'addActionButtons'

    # Convert any list columns to characters before displaying
    for (i in seq_len(ncol(data))) {
      if (is.list(data[, i])) {
        data[, i] <- as.character(sapply(data[, i], FUN = function(x) {
          paste0(x, collapse = ", ")
        }))
        # convert to as.character, because if data[,i] is empty,
        # sapply can return an empty list
        # cannot assign empty list() to data[,i], because that
        # causes data[,i] column to be deleted!
      }
    }

    DT::replaceData(proxy, addActionButtons(data, action.buttons)$data, ...)
    result$rows_selected <- NULL # no row selected after each edit
  }

  ##### inputEvent observeEvent helper ####################################

  inputEvent_handler <- function(input_infix) {
    # parameters : input_infix e.g. "_add", or "_edit"
    # returns    : list of observeEvents (already created)
    #
    # creates observeEvents watching the edit/add input widgets
    # e.g. to use package 'shinyFeedback'
    # for edit.cols with functions defined in parameter 'inputEvent'
    #
    # the return list is required so the caller can explicitly destroy the
    # observeEvents when the add/edit/copy modal is closed. Otherwise the
    # observeEvents accumulate

    handler <- lapply(
      X = edit.cols[grepl(names(inputEvent), edit.cols)],
      # choose only edit.cols which are defined in 'inputEvent'
      FUN = function(x) {
        input_name <- paste0(name, input_infix, "_", x)
        observeEvent(input[[input_name]], {
          inputEvent[[x]](input_name)
        })
      }
    )

    return(handler)
  }

  inputEvent_handles <- NULL
  # will be used by the functions which call inputEvent_handler

  ##### Insert functions #####################################################

  observeEvent(input[[paste0(name, "_add")]], {
    # if the 'Add' button is clicked then
    # the 'addModal' popup is generated, with 'missing' values
    if (!is.null(row)) {
      shiny::showModal(addModal())
      inputEvent_handles <- inputEvent_handler("_add")
    }
  })

  addModal <- function(row, values) {
    # 'addModal' popup is generated when
    # the '_add' button event is observed (with missing 'values')
    # the '_copy' button event is observed (with prefilled 'values')
    #
    # other than being closed/cancelled
    # the 'addModal' popup can create an '_insert' event
    output[[paste0(name, "_message")]] <- shiny::renderUI("")
    fields <- getFields("_add_", values)
    shiny::modalDialog(
      title = title.add,
      shiny::div(
        shiny::htmlOutput(
          ns(paste0(name, "_message"))
        ),
        style = "color:red"
      ),
      fields,
      footer = shiny::column(
        shiny::modalButton(label.cancel),
        shiny::actionButton(ns(paste0(name, "_insert")), label.save),
        width = 12
      ),
      size = modal.size
    )
  }

  insert.click <- NA # click timer (to avoid overly fast double-click)

  observeEvent(input[[paste0(name, "_insert")]], {
    # '_insert' event generated from the 'addModal' popup
    if (!is.na(insert.click)) {
      lastclick <- as.numeric(Sys.time() - insert.click, units = "secs")
      if (lastclick < click.time.threshold) {
        warning(
          paste0("Double click detected. Ignoring insert call for ",
                 name, "."))
        return()
      }
    }
    insert.click <<- Sys.time()
    newdata <- result$thedata
    row <- nrow(newdata) + 1 # the new row number
    new_row <- list() # to contain a 'blank' new row
    # the following loop can be tested on the following dataframes
    # data.frame(a = character(), b = numeric(),
    #            x = as.Date(numeric(), origin = "1970-01-01"), y = raw())
    # data.frame(a = "a", b = 7,
    #            x = as.Date(NA, origin = "1970-01-01"), y = raw(1))
    #  'raw(1)' can be changed to as.blob(raw(0))
    #  but as.blob can't be used to create a NULL blob object!
    for (i in seq_len(ncol(newdata))) {
      new_row[[i]] <- switch(
        class(newdata[, i])[[1]],
        "factor" = as.factor(NA),
        "Date" = as.Date(NA, origin = "1970-01-01"),
        "raw" = list(blob::as.blob(raw(1))),
        "blob" = list(blob::as.blob(raw(1))),
        "character" = as.character(NA),
        "numeric" = as.numeric(NA),
        "POSIXct" = as.POSIXct(NA),
        "AsIs" = as.list(NA), # for lists
        methods::as(NA, class(newdata[, i])[[1]]))
    }
    newdata[row, ] <- data.frame(new_row, stringsAsFactors = FALSE)
    # create a new empty row, compatible with blob columns
    # the new row is ready for filling
    for (i in edit.cols) {
      if (inputTypes[i] %in%
          c("selectInputMultiple", "selectInputMultipleReactive")) {
        newdata[[i]][row] <- list(input[[paste0(name, "_add_", i)]])
      } else if (inputTypes[i] == "fileInput") { # file read into binary blob
        datapath <- input[[paste0(name, "_add_", i)]]$datapath
        if (!is.null(datapath)) {
          newdata[[i]][row] <- blob::blob(
            readBin(
              datapath,
              what = "raw",
              n = max.fileInputLength
            )
          )
        }
      } else {
        newdata[row, i] <- input[[paste0(name, "_add_", i)]]
      }
    }
    tryCatch({
      callback.data <- callback.insert(data = newdata, row = row)
      if (!is.null(callback.data) && is.data.frame(callback.data)) {
        result$thedata <- callback.data
      } else {
        result$thedata <- newdata
      }
      updateData(dt.proxy,
                 result$thedata[, view.cols, drop = FALSE],
                 # was "result$thedata[,view.cols]",
                 # but that returns vector if view.cols is a single column
                 rownames = datatable.rownames
      )
      result$edit.count <- result$edit.count + 1
      shiny::removeModal()
      return(TRUE)
    }, error = function(e) {
      output[[paste0(name, "_message")]] <<-
        shiny::renderUI(shiny::HTML(geterrmessage()))
      return(FALSE)
    })
  })

  ##### Copy functions #######################################################

  observeEvent(input[[paste0(name, "_copy")]], {
    # if '_copy' event is observed, call the 'addModal' popup
    # with pre-filled values
    # (the same 'addModal' popup is used with missing values for '_insert')
    row <- input[[paste0(name, "dt_rows_selected")]]
    if (!is.null(row)) {
      if (row > 0) {
        shiny::showModal(addModal(values = result$thedata[row, , drop = FALSE]))
        inputEvent_handler("_add") # shares the same input names as '_add'
      }
    }
  })

  ##### Update functions #####################################################

  observeEvent(input[[paste0(name, "_edit")]], {
    # if '_edit' event is observed, call the 'editModal' popup
    row <- input[[paste0(name, "dt_rows_selected")]]
    if (!is.null(row) && row > 0) {
      shiny::showModal(editModal(row))
      inputEvent_handler("_edit")
    }
  })

  editModal <- function(row) {
    # 'editModal' popup created when '_edit' event is observed
    #
    # other than being closed/cancelled, the 'editModal' popup
    # can also be closed when the '_update' event is observed
    output[[paste0(name, "_message")]] <- renderUI("")
    fields <- getFields("_edit_", values = result$thedata[row, , drop = FALSE])
    shiny::modalDialog(
      title = title.edit,
      shiny::fluidPage(
        shiny::div(
          if (datatable.rownames) # rownames are being displayed
            shiny::h4(rownames(thedata)[row])
        ),
        shiny::div(
          shiny::htmlOutput(
            ns(paste0(name, "_message"))),
          style = "color:red"),
        fields
      ),
      footer = column(
        shiny::modalButton(label.cancel),
        shiny::actionButton(ns(paste0(name, "_update")), label.save),
        width = 12
      ),
      size = modal.size
    )
  }

  update.click <- NA # a timer to avoid 'double-clicks'

  observeEvent(input[[paste0(name, "_update")]], {
    # the '_update' event is observed from the 'editModal' popup

    if (!is.na(update.click)) {
      lastclick <- as.numeric(Sys.time() - update.click, units = "secs")
      if (lastclick < click.time.threshold) {
        warning(paste0("Double click detected. Ignoring update call for ",
                       name, "."))
        return()
      }
    }
    update.click <- Sys.time()

    row <- input[[paste0(name, "dt_rows_selected")]]
    if (!is.null(row) && row > 0) {
      newdata <- result$thedata
      for (i in edit.cols) {
        if (inputTypes[i] %in% c("selectInputMultiple",
                                 "selectInputMultipleReactive")) {
          newdata[[i]][row] <- list(input[[paste0(name, "_edit_", i)]])
        } else if (inputTypes[i] == "fileInput") {
          datapath <- input[[paste0(name, "_edit_", i)]]$datapath
          if (!is.null(datapath) && file.exists(datapath)) {
            # only if file actually uploaded, otherwise we won't update
            newdata[[i]][row] <- blob::blob(
              readBin( # file read into binary raw (blob) column
                datapath,
                what = "raw",
                n = max.fileInputLength
              )
            )
          }
        } else if (inputTypes[i] %in% c("dateInput", "datetimeInput")) {
          if (length(input[[paste0(name, "_edit_", i)]]) == 0) {
            newdata[row, i] <- as.Date(NA)
            # 'dateInput' returns length 0 if empty, but date of length 0
            # fails to 'replace' the current contents of newdata[row, i]!
            # address issue #21
          } else {
            newdata[row, i] <- input[[paste0(name, "_edit_", i)]]
          }
        } else {
          newdata[row, i] <- input[[paste0(name, "_edit_", i)]]
        }
      }
      tryCatch({
        callback.data <- callback.update(
          data = newdata,
          olddata = result$thedata,
          row = row
        )
        if (!is.null(callback.data) && is.data.frame(callback.data)) {
          result$thedata <- callback.data
        } else {
          result$thedata <- newdata
        }
        updateData(dt.proxy,
                   result$thedata[, view.cols, drop = FALSE],
                   # was "result$thedata[,view.cols]",
                   # but that returns vector (not dataframe) if
                   # view.cols is only a single column
                   rownames = datatable.rownames
        )
        result$edit.count <- result$edit.count + 1
        shiny::removeModal()
        return(TRUE)
      }, error = function(e) {
        output[[paste0(name, "_message")]] <<-
          shiny::renderUI(shiny::HTML(geterrmessage()))
        return(FALSE)
      })
    }
    return(FALSE)
  })

  ##### Delete functions #####################################################

  observeEvent(input[[paste0(name, "_remove")]], {
    # if the '_remove' event is observed, the 'deleteModal' popup is opened
    row <- input[[paste0(name, "dt_rows_selected")]]
    if (!is.null(row)) {
      if (row > 0) {
        shiny::showModal(deleteModal(row))
      }
    }
  })

  deleteModal <- function(row) {
    # if the '_remove' event is observed, the 'deleteModal' popup is opened
    #
    # other than being closed/cancelled, the 'deleteModal' popup
    # can also be closed if the '_delete' event is observed
    fields <- list()
    for (i in seq_along(delete.info.cols)) {
      fields[[i]] <- div(paste0(delete.info.label.cols[i], " = ",
                                result$thedata[row, delete.info.cols[i]]))
    }
    output[[paste0(name, "_message")]] <- shiny::renderUI("")
    shiny::modalDialog(
      title = title.delete,
      shiny::fluidPage(
        shiny::div(
          if (datatable.rownames) # rownames are being displayed
            shiny::h4(rownames(thedata)[row])
        ),
        shiny::div(
          shiny::htmlOutput(
            ns(paste0(name, "_message"))), style = "color:red"
        ),
        shiny::p(text.delete.modal),
        fields
      ),
      footer = shiny::column(modalButton(label.cancel),
                             shiny::actionButton(
                               ns(paste0(name, "_delete")), label.delete
                             ),
                             width = 12
      ),
      size = modal.size
    )
  }

  observeEvent(input[[paste0(name, "_delete")]], {
    # the '_delete' event is observed from the 'deleteModal' popup

    row <- input[[paste0(name, "dt_rows_selected")]]
    if (!is.null(row) && row > 0) {
      tryCatch({
        callback.data <- callback.delete(data = result$thedata, row = row)
        if (!is.null(callback.data) && is.data.frame(callback.data)) {
          result$thedata <- callback.data
        } else {
          result$thedata <- result$thedata[-row, , drop = FALSE]
          # 'drop=FALSE' prevents the dataframe being reduced to a vector
          # especially if only a single column
        }
        updateData(dt.proxy,
                   result$thedata[, view.cols, drop = FALSE],
                   # was "result$thedata[,view.cols]",
                   # but that only returns a vector (instead of dataframe)
                   # if view.cols is single column
                   rownames = datatable.rownames
        )
        result$edit.count <- result$edit.count + 1
        shiny::removeModal()
        return(TRUE)
      },
      error = function(e) {
        output[[paste0(name, "_message")]] <<-
          shiny::renderUI(shiny::HTML(geterrmessage()))
        return(FALSE)
      }
      )
    }
    return(FALSE)
  })

  ##### Action button callbacks ################################################

  observeEvent(input$select_button, {
    # triggered by an 'action' button being clicked

    # row <- input[[paste0(name, "dt_rows_selected")]]
    # unfortunately, the 'row' selected this way seems to be unreliable
    # determine the row number from the button selected
    #  the buttons have been 'numbered' in the suffix
    x <- strsplit(input$select_button, "_")[[1]]
    selectedRow <- as.numeric(x[length(x)])

    newdata <- result$thedata
    tryCatch({
      callback.data <- callback.actionButton(
        data = result$thedata,
        row = selectedRow,
        buttonID = input$select_button
      )
      if (!is.null(callback.data) && is.data.frame(callback.data)) {
        result$thedata <- callback.data
      } else {
        result$thedata <- newdata
        # 'drop=FALSE' prevents the dataframe being reduced to a vector
        # especially if only a single column
      }
      updateData(dt.proxy,
                 result$thedata[, view.cols, drop = FALSE],
                 # was "result$thedata[,view.cols]",
                 # but that only returns a vector (instead of dataframe)
                 # if view.cols is single column
                 rownames = datatable.rownames
      )
      result$edit.count <- result$edit.count + 1
      shiny::removeModal()
      return(TRUE)
    },
    error = function(e) {
      output[[paste0(name, "_message")]] <<-
        shiny::renderUI(shiny::HTML(geterrmessage()))
      return(FALSE)
    }
    )
  })

  ##### React to changes in 'thedata' if that variable is a reactive ######

  if (shiny::is.reactive(thedata)) {
    observeEvent(thedata(), {
      result$thedata <- as.data.frame(shiny::isolate(thedata()))
      updateData(dt.proxy,
                 result$thedata[, view.cols, drop = FALSE],
                 # was "result$thedata[,view.cols]",
                 # but that returns vector (not dataframe)
                 # if view.cols is only a single column
                 rownames = datatable.rownames
      )
    })
  }

  ##### Build the UI for the DataTable and buttons ###########################

  output[[name]] <- shiny::renderUI({
    shiny::div(
      if (show.insert) {
        shiny::actionButton(
          ns(paste0(name, "_add")), label.add, icon = icon.add
        )
      },
      if (show.update) {
        shiny::actionButton(
          ns(paste0(name, "_edit")), label.edit, icon = icon.edit
        )
      },
      if (show.delete) {
        shiny::actionButton(
          ns(paste0(name, "_remove")), label.delete, icon = icon.delete
        )
      },
      if (show.copy) {
        shiny::actionButton(
          ns(paste0(name, "_copy")), label.copy, icon = icon.copy
        )
      },
      shiny::br(), shiny::br(), DT::dataTableOutput(ns(DataTableName))
    )
  })
  outputOptions(output, name, suspendWhenHidden = FALSE)
  # if suspendWhenHidden is true, then
  # the table is not rendered if the tab is hidden

  return(result)
  # $edit.count only incremented by changes made through dtedit GUI
  # does not include edits created through response
  #  to changes in reactiveval 'thedata'
  # this might help determine the source of changes in result$thedata
}

#' Create a DataTable with Add, Edit and Delete buttons.
#'
#' dteditmodUI - user-interface function for module use
#'
#' Use in conjunction with \code{callModule} and \code{dtedit} to create
#' editable datatables. \code{dteditUI} is used in the 'user interface'
#' component of the shiny app.
#'
#' @param id the namespace of the module
#' @family Datatable Edit functions
#' @seealso \code{\link{dteditmod}} : the companion server-component function.\cr
#'
#'  \itemize{
#'  \item \code{example("dteditmodUI")} a simple example
#'   with a reactive dataframe
#'  \item \code{dteditmod_demo()} a more complex example.
#'   Includes database interaction and interactions between
#'   the data of multiple datatables.
#'  }
#' @example inst/examples/reactivedataframe_modular/app.R
#' @export
dteditmodUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("editdt"))
  )
}
