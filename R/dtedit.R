#' Create a DataTable with Add, Edit and Delete buttons.
#'
#' dtedit - server function
#'
#' Use in conjunction with \code{callModule} and \code{dteditUI} to create
#' editable datatables. \code{dtedit} is used in the 'server' component of the
#' shiny app.
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
#' @return Returns a list of reactive values. \code{return_values$data()} contains
#'  the current state of DTedit's copy of the data. \code{return_values$edit.count()}
#'  contains the number of edits done within DTedit (does not include changes to DTedit's
#'  copy of the data secondary to changes in \code{thedataframe}, if \code{thedataframe} is a reactive)
#'
#' @param input Shiny input object passed from the server.
#' @param output Shiny output object passed from the server.
#' @param session Shiny session object passed from the server
#' @param thedataframe a data frame to view and edit. can be a reactive
#' @param view.cols character vector with the column names to show in the DataTable.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.cols character vector with the column names the user can edit/add.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.label.cols character vector with the labels to use on the edit
#'        and add dialogs. The length and order of \code{code.cols.labels} must
#'        correspond to \code{edit.cols}.
#' @param input.types a character vector where the name corresponds to a column
#'        in \code{edit.cols} and the value is the input type. Possible values
#'        are \code{dateInput}, \code{selectInput}, \code{selectInputMultiple},
#'        \code{selectInputReactive}, \code{selectInputMultipleReactive}, \code{numericInput}, \code{textInput}, \code{textAreaInput},
#'        or \code{passwordInput}.
#'        The most common case where this parameter is desirable is when a text
#'        area is required instead of a simple text input.
#' @param input.choices a list of character vectors. The names of each element in the list must
#'        correspond to a column name in the data. The value, a character vector, are the options
#'        presented to the user for data entry, in the case of input type \code{selectInput}).
#'        In the case of input type \code{selectInputReactive} or \code{selectInputMultipleReactive}, the value
#'        is the name of the reactive. in 'input.choices.reactive'
#' @param input.choices.reactive a named list of reactives, referenced in 'input.choices'
#'        to use for input type \code{selectInputReactive} or \code{selectInputMultipleReactive}.
#'        The reactive itself is a character vector.
#' @param selectize Whether to use selectize.js or not. See \code{\link{selectInput}} for more info.
#' @param defaultPageLength number of rows to show in the data table by default.
#' @param modal.size the size of the modal dialog. See \code{\link{modalDialog}}.
#' @param text.width width of text inputs.
#' @param textarea.width the width of text area inputs.
#' @param textarea.height the height of text area inputs.
#' @param date.width the width of data inputs
#' @param numeric.width the width of numeric inputs.
#' @param select.width the width of drop down inputs.
#' @param title.delete the title of the dialog box for deleting a row.
#' @param title.edit the title of the dialog box for editing a row.
#' @param title.add the title of the dialog box for inserting a new row.
#' @param label.delete the label of the delete button.
#' @param label.edit the label of the edit button.
#' @param label.add the label of the add button.
#' @param label.copy the label of the copy button.
#' @param show.delete whether to show/enable the delete button.
#' @param show.update whether to show/enable the update button.
#' @param show.insert whether to show/enable the insert button.
#' @param show.copy whether to show/enablre the copy button.
#' @param callback.delete a function called when the user deletes a row. This function should
#'        return an updated data.frame.
#' @param callback.update a function called when the user updates a row. This function should
#'        return an updated data.frame.
#' @param callback.insert a function called when the user inserts a new row. This function should
#'        return an updated data.frame.
#' @param click.time.threshold This is to prevent duplicate entries usually by double clicking the
#'        save or update buttons. If the user clicks the save button again within this amount of
#'        time (in seconds), the subsequent click will be ignored. Set to zero to disable this
#'        feature. For developers, a message is printed using the warning function.
#' @param datatable.options options passed to \code{\link{DT::renderDataTable}}.
#'        See \url{https://rstudio.github.io/DT/options.html} for more information.
#' @family Datatable Edit functions
#' @seealso \code{\link{dteditUI}} : the companion user-interface function.\cr
#'
#'  \itemize{
#'  \item \code{example("dtedit")} for a simple example.
#'  \item \code{dtedit_demo()} for a more complex example. Includes database interaction
#'  and interactions between the data of multiple datatables.
#'  }
#' @example inst/examples/example.R
#'
#' @export
dtedit <- function(input, output, session, thedataframe,
		   view.cols = names(isolate(if(is.reactive(thedataframe))
		   {thedataframe()} else {thedataframe})),
		   edit.cols = names(isolate(if(is.reactive(thedataframe))
		   {thedataframe()} else {thedataframe})),
		   edit.label.cols = edit.cols,
		   input.types,
		   input.choices = NULL,
		   input.choices.reactive = NULL,
		   selectize = TRUE,
		   modal.size = 'm',
		   text.width = '100%',
		   textarea.width = '570px',
		   textarea.height = '200px',
		   date.width = '100px',
		   numeric.width = '100px',
		   select.width = '100%',
		   defaultPageLength = 10,
		   title.delete = 'Delete',
		   title.edit = 'Edit',
		   title.add = 'New',
		   label.delete = 'Delete',
		   label.edit = 'Edit',
		   label.add = 'New',
		   label.copy = 'Copy',
		   show.delete = TRUE,
		   show.update = TRUE,
		   show.insert = TRUE,
		   show.copy = TRUE,
		   callback.delete = function(data, row) { },
		   callback.update = function(data, olddata, row) { },
		   callback.insert = function(data, row) { },
		   click.time.threshold = 2, # in seconds
		   datatable.options = list(pageLength=defaultPageLength)
) {
	thedata <- if(is.reactive(isolate(thedataframe)))
	{isolate(thedataframe())} else {thedataframe}
	# if a reactive has been passed, obtain the value
	# Some basic parameter checking
	if(!is.data.frame(thedata) | ncol(thedata) < 1) {
		stop('Must provide a data frame with at least one column.')
	} else if(length(edit.cols) != length(edit.label.cols)) {
		stop('edit.cols and edit.label.cols must be the same length.')
	} else if(!all(view.cols %in% names(thedata))) {
		stop('Not all view.cols are in the data.')
	} else if(!all(edit.cols %in% names(thedata))) {
		stop('Not all edit.cols are in the data.')
	}

	name <- "editdt"
	DataTableName <- paste0(name, 'dt')

	result <- shiny::reactiveValues()
	result$thedata <- thedata
	result$view.cols <- view.cols
	result$edit.cols <- edit.cols
	result$edit.count <- 0 # number of edits (Add/Delete/Edit/Copy) through dtedit

	dt.proxy <- DT::dataTableProxy(DataTableName)

	selectInputMultiple <- function(...) {
		shiny::selectInput(multiple = TRUE, selectize = selectize, ...)
	}

	valid.input.types <- c('dateInput', 'selectInput', 'numericInput',
			       'textInput', 'textAreaInput', 'passwordInput', 'selectInputMultiple',
			       'selectInputReactive', 'selectInputMultipleReactive')
	inputTypes <- sapply(thedata[,edit.cols], FUN=function(x) {
		switch(class(x),
		       list = 'selectInputMultiple',
		       character = 'textInput',
		       Date = 'dateInput',
		       factor = 'selectInput',
		       integer = 'numericInput',
		       numeric = 'numericInput',
		       factor = 'selectInputReactive',
		       list = 'selectInputMultipleReactive')
	})
	if(!missing(input.types)) {
		if(!all(names(input.types) %in% edit.cols)) {
			stop('input.types column not a valid editing column: ',
			     paste0(names(input.types)[!names(input.types) %in% edit.cols]))
		}
		if(!all(input.types %in% valid.input.types)) {
			stop(paste0('input.types must only contain values of: ',
				    paste0(valid.input.types, collapse = ', ')))
		}
		inputTypes[names(input.types)] <- input.types
	}
	# Convert any list columns to characters before displaying
	for(i in 1:ncol(thedata)) {
		if(nrow(thedata) == 0) {
			thedata[,i] <- character()
		} else if(is.list(thedata[,i])) {
			thedata[,i] <- sapply(thedata[,i], FUN = function(x) { paste0(x, collapse = ', ') })
		}
	}

	output[[DataTableName]] <- DT::renderDataTable({
		thedata[,view.cols, drop=FALSE]
		# was "thedata[,view.cols]", but requires drop=FALSE
		# to prevent return of vector (instead of dataframe)
		# if only one column in view.cols
	}, options = datatable.options, server=TRUE, selection='single', rownames=FALSE)
	outputOptions(output, DataTableName, suspendWhenHidden = FALSE)
	# without turning off suspendWhenHidden, changes are not rendered if containing tab is not visible

	getFields <- function(typeName, values) {
		# 'values' are current values of the row (if already existing, or being copied)
		ns <- session$ns # need to use namespace for id elements in module
		fields <- list()
		for(i in seq_along(edit.cols)) {
			if(inputTypes[i] == 'dateInput') {
				value <- ifelse(missing(values),
						as.character(Sys.Date()),
						as.character(values[,edit.cols[i]]))
				fields[[i]] <- dateInput(ns(paste0(name, typeName, edit.cols[i])),
							 label=edit.label.cols[i],
							 value=value,
							 width=date.width)
			} else if(inputTypes[i] == 'selectInputMultiple') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				if(is.list(value)) {
					value <- value[[1]]
				}
				choices <- ''
				if(!missing(values)) {
					choices <- unique(unlist(values[,edit.cols[i]]))
				}
				if(!is.null(input.choices)) {
					if(edit.cols[i] %in% names(input.choices)) {
						choices <- input.choices[[edit.cols[i]]]
					}
				}
				if(length(choices) == 1 & choices == '') {
					warning(paste0('No choices available for ', edit.cols[i],
						       '. Specify them using the input.choices parameter'))
				}
				fields[[i]] <- selectInputMultiple(ns(paste0(name, typeName, edit.cols[i])),
								   label=edit.label.cols[i],
								   choices=choices,
								   selected=value,
								   width=select.width)

			} else if(inputTypes[i] == 'selectInputMultipleReactive') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				if(is.list(value)) {
					value <- value[[1]]
				}
				choices <- NULL
				if(!is.null(input.choices.reactive)) {
					if(edit.cols[i] %in% names(input.choices)) {
						choices <- input.choices.reactive[[input.choices[[edit.cols[i]]]]]()
						# it is the responsiblity of the calling functions/reactive variable handlers
						# that the list of choices includes all CURRENT choices that have already
						# been chosen in the data.
					}
				}
				if(is.null(choices)) {
					warning(paste0("No choices available for ", edit.cols[i],
						       '. Specify them using the input.choices and input.choices.reactive parameter'))
				}
				fields[[i]] <- selectInputMultiple(ns(paste0(name, typeName, edit.cols[i])),
								   label=edit.label.cols[i],
								   choices=choices,
								   selected=value,
								   width=select.width)
			} else if (inputTypes[i] == 'selectInputReactive') {
				value <- ifelse(missing(values), '', as.character(values[,edit.cols[i]]))
				choices <- NULL
				if(!is.null(input.choices.reactive)) {
					if(edit.cols[i] %in% names(input.choices)) {
						choices <- input.choices.reactive[[input.choices[[edit.cols[i]]]]]()
						# it is the responsiblity of the calling functions/reactive variable handlers
						# that the list of choices includes all CURRENT choices that have already
						# been chosen in the data.
					}
				}
				if(is.null(choices)) {
					warning(paste0("No choices available for ", edit.cols[i],
						       '. Specify them using the input.choices and input.choices.reactive parameter'))
				}
				fields[[i]] <- shiny::selectInput(ns(paste0(name, typeName, edit.cols[i])),
								  label=edit.label.cols[i],
								  choices=choices,
								  selected=value,
								  width=select.width)
			} else if(inputTypes[i] == 'selectInput') {
				value <- ifelse(missing(values), '', as.character(values[, edit.cols[i], drop=FALSE]))
				fields[[i]] <- shiny::selectInput(ns(paste0(name, typeName, edit.cols[i])),
								  label=edit.label.cols[i],
								  choices=levels(result$thedata[,edit.cols[i]]),
								  selected=value,
								  width=select.width)
			} else if(inputTypes[i] == 'numericInput') {
				value <- ifelse(missing(values), 0, values[,edit.cols[i]])
				fields[[i]] <- shiny::numericInput(ns(paste0(name, typeName, edit.cols[i])),
								   label=edit.label.cols[i],
								   value=value,
								   width=numeric.width)
			} else if(inputTypes[i] == 'textAreaInput') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				fields[[i]] <- shiny::textAreaInput(ns(paste0(name, typeName, edit.cols[i])),
								    label=edit.label.cols[i],
								    value=value,
								    width=textarea.width, height=textarea.height)
			} else if(inputTypes[i] == 'textInput') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				fields[[i]] <- shiny::textInput(ns(paste0(name, typeName, edit.cols[i])),
								label=edit.label.cols[i],
								value=value,
								width=text.width)
			} else if(inputTypes[i] == 'passwordInput') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				fields[[i]] <- shiny::passwordInput(ns(paste0(name, typeName, edit.cols[i])),
								    label=edit.label.cols[i],
								    value=value,
								    width=text.width)
			} else {
				stop('Invalid input type!')
			}
		}
		return(fields)
	}

	output[[paste0(name, '_message')]] <- shiny::renderText('')

	updateData <- function(proxy, data, ...) {
		# Convert any list columns to characters before displaying
		for(i in 1:ncol(data)) {
			if(is.list(data[,i])) {
				data[,i] <- as.character(sapply(data[,i], FUN = function(x) { paste0(x, collapse = ', ') }))
				# convert to as.character, because if data[,i] is empty, sapply can return an empty list
				# cannot assign empty list() to data[,i], because that causes data[,i] column to be deleted!
			}
		}
		DT::replaceData(proxy, data, ...)
	}

	##### Insert functions #####################################################

	observeEvent(input[[paste0(name, '_add')]], {
		if(!is.null(row)) {
			shiny::showModal(addModal())
		}
	})

	insert.click <- NA

	observeEvent(input[[paste0(name, '_insert')]], {
		if(!is.na(insert.click)) {
			lastclick <- as.numeric(Sys.time() - insert.click, units = 'secs')
			if(lastclick < click.time.threshold) {
				warning(paste0('Double click detected. Ignoring insert call for ', name, '.'))
				return()
			}
		}
		insert.click <<- Sys.time()

		newdata <- result$thedata
		row <- nrow(newdata) + 1
		newdata[row,] <- NA
		for(i in edit.cols) {
			if(inputTypes[i] %in% c('selectInputMultiple', 'selectInputMultipleReactive')) {
				newdata[[i]][row] <- list(input[[paste0(name, '_add_', i)]])
			} else {
				newdata[row,i] <- input[[paste0(name, '_add_', i)]]
			}
		}
		tryCatch({
			callback.data <- callback.insert(data = newdata, row = row)
			if(!is.null(callback.data) & is.data.frame(callback.data)) {
				result$thedata <- callback.data
			} else {
				result$thedata <- newdata
			}
			updateData(dt.proxy,
				   result$thedata[,view.cols, drop=FALSE],
				   # was "result$thedata[,view.cols]",
				   # but that returns vector if view.cols is a single column
				   rownames = FALSE)
			result$edit.count <- result$edit.count + 1
			shiny::removeModal()
			return(TRUE)
		}, error = function(e) {
			output[[paste0(name, '_message')]] <<- shiny::renderText(geterrmessage())
			return(FALSE)
		})
	})

	addModal <- function(row, values) {
		ns <- session$ns # necessary to use namespace for id elements in modaldialogs within modules
		output[[paste0(name, '_message')]] <- shiny::renderText('')
		fields <- getFields('_add_', values)
		shiny::modalDialog(title = title.add,
				   shiny::div(shiny::textOutput(ns(paste0(name, '_message'))), style='color:red'),
				   fields,
				   footer = shiny::column(shiny::modalButton('Cancel'),
				   		       shiny::actionButton(ns(paste0(name, '_insert')), 'Save'),
				   		       width=12),
				   size = modal.size
		)
	}

	##### Copy functions #######################################################

	observeEvent(input[[paste0(name, '_copy')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				shiny::showModal(addModal(values=result$thedata[row,, drop=FALSE]))
			}
		}
	})

	##### Update functions #####################################################

	observeEvent(input[[paste0(name, '_edit')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				shiny::showModal(editModal(row))
			}
		}
	})

	update.click <- NA

	observeEvent(input[[paste0(name, '_update')]], {
		if(!is.na(update.click)) {
			lastclick <- as.numeric(Sys.time() - update.click, units = 'secs')
			if(lastclick < click.time.threshold) {
				warning(paste0('Double click detected. Ignoring update call for ', name, '.'))
				return()
			}
		}
		update.click <- Sys.time()

		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				newdata <- result$thedata
				for(i in edit.cols) {
					if(inputTypes[i] %in% c('selectInputMultiple', 'selectInputMultipleReactive')) {
						newdata[[i]][row] <- list(input[[paste0(name, '_edit_', i)]])
					} else {
						newdata[row,i] <- input[[paste0(name, '_edit_', i)]]
					}
				}
				tryCatch({
					callback.data <- callback.update(data = newdata,
									 olddata = result$thedata,
									 row = row)
					if(!is.null(callback.data) & is.data.frame(callback.data)) {
						result$thedata <- callback.data
					} else {
						result$thedata <- newdata
					}
					updateData(dt.proxy,
						   result$thedata[,view.cols, drop=FALSE],
						   # was "result$thedata[,view.cols]",
						   # but that returns vector (not dataframe) if
						   # view.cols is only a single column
						   rownames = FALSE)
					result$edit.count <- result$edit.count + 1
					shiny::removeModal()
					return(TRUE)
				}, error = function(e) {
					output[[paste0(name, '_message')]] <<- shiny::renderText(geterrmessage())
					return(FALSE)
				})
			}
		}
		return(FALSE)
	})

	editModal <- function(row) {
		ns <- session$ns # necessary to use namespace for id elements in modaldialogs within modules
		output[[paste0(name, '_message')]] <- renderText('')
		fields <- getFields('_edit_', values=result$thedata[row,, drop=FALSE])
		shiny::modalDialog(title = title.edit,
				   shiny::div(shiny::textOutput(ns(paste0(name, '_message'))), style='color:red'),
				   fields,
				   footer = column(shiny::modalButton('Cancel'),
				   		shiny::actionButton(ns(paste0(name, '_update')), 'Save'),
				   		width=12),
				   size = modal.size
		)
	}

	##### Delete functions #####################################################

	observeEvent(input[[paste0(name, '_remove')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				shiny::showModal(deleteModal(row))
			}
		}
	})

	observeEvent(input[[paste0(name, '_delete')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				tryCatch({
					newdata <- callback.delete(data = result$thedata, row = row)
					if(!is.null(newdata) & is.data.frame(newdata)) {
						result$thedata <- newdata
					} else {
						result$thedata <- result$thedata[-row,,drop=FALSE]
						# 'drop=FALSE' prevents the dataframe being reduced to a vector
						# especially if only a single column
					}
					updateData(dt.proxy,
						   result$thedata[,view.cols, drop=FALSE],
						   # was "result$thedata[,view.cols]",
						   # but that only returns a vector (instead of dataframe)
						   # if view.cols is single column
						   rownames = FALSE)
					result$edit.count <- result$edit.count + 1
					shiny::removeModal()
					return(TRUE)
				},
				error = function(e) {
					output[[paste0(name, '_message')]] <<- shiny::renderText(geterrmessage())
					return(FALSE)}
				)
			}
		}
		return(FALSE)
	})

	deleteModal <- function(row) {
		ns <- session$ns # necessary to use namespace for id elements in modaldialogs within modules
		fields <- list()
		for(i in view.cols) {
			fields[[i]] <- div(paste0(i, ' = ', result$thedata[row,i]))
		}
		output[[paste0(name, '_message')]] <- shiny::renderText('')
		shiny::modalDialog(title = title.delete,
				   shiny::div(shiny::textOutput(ns(paste0(name, '_message'))), style='color:red'),
				   shiny::p('Are you sure you want to delete this record?'),
				   fields,
				   footer = shiny::column(modalButton('Cancel'),
				   		       shiny::actionButton(ns(paste0(name, '_delete')), 'Delete'),
				   		       width=12),
				   size = modal.size
		)
	}

	##### React to changes in 'thedataframe' if that variable is a reactive ######

	if (is.reactive(thedataframe)) {
		observeEvent(thedataframe(), {
			result$thedata <- as.data.frame(isolate(thedataframe()))
			updateData(dt.proxy,
				   result$thedata[,view.cols, drop=FALSE],
				   # was "result$thedata[,view.cols]",
				   # but that returns vector (not dataframe)
				   # if view.cols is only a single column
				   rownames = FALSE)
		})
	}

	##### Build the UI for the DataTable and buttons ###########################

	output[[name]] <- shiny::renderUI({
		ns <- session$ns # namespace for module
		shiny::div(
			if(show.insert) { shiny::actionButton(ns(paste0(name, '_add')), label.add) },
			if(show.update) { shiny::actionButton(ns(paste0(name, '_edit')), label.edit) },
			if(show.delete) { shiny::actionButton(ns(paste0(name, '_remove')), label.delete) },
			if(show.copy) { shiny::actionButton(ns(paste0(name, '_copy')), label.copy) },
			shiny::br(), shiny::br(), DT::dataTableOutput(ns(DataTableName))
		)
	})
	outputOptions(output, name, suspendWhenHidden = FALSE)
	# if suspendWhenHidden is true, then the table is not rendered if the tab is hidden

	return(list(thedata = reactive({result$thedata}),
		    edit.count = reactive({result$edit.count})))
	# edit.count only incremented by changes made through dtedit GUI
	# does not include edits created through response to changes in reactiveval 'thedataframe'
	# this might help determine the source of changes in result$thedata
}

#' Create a DataTable with Add, Edit and Delete buttons.
#'
#' dteditUI - user-interface function
#'
#' Use in conjunction with \code{callModule} and \code{dtedit} to create
#' editable datatables. \code{dteditUI} is used in the 'user interface' component
#' of the shiny app.
#'
#' @param id the namespace of the module
#' @family Datatable Edit functions
#' @seealso \code{\link{dtedit}} : the companion server-component function.\cr
#!
#'  \itemize{
#'  \item \code{example("dtedit")} for a simple example.
#'  \item \code{dtedit_demo()} for a more complex example. Includes database interaction
#'  and interactions between the data of multiple datatables.
#'  }
#' @example inst/examples/example.R
#' @export
dteditUI <- function(id) {
	ns <- NS(id)

	tagList(
		uiOutput(ns("editdt"))
	)
}
