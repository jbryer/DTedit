#' Function to create a DataTable with Add, Edit, and Delete buttons.
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
#' @param input Shiny input object passed from the server.
#' @param output Shiny output object passed from the server.
#' @param name the name of the UI output. That is, put \code{uiOutput(name)} where
#'        you want the DataTable in \code{ui.R}. When using more that one \code{dtedit}
#'        within a Shiny application the name must be unique.
#' @param thedata a data frame to view and edit.
#' @param view.cols character vector with the column names to show in the DataTable.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.cols character vector with the column names the user can edit/add.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.cols.labels character vector with the labels to use on the edit
#'        and add dialogs. The length and order of \{code.cols.labels} must
#'        correspond to \code{edit.cols}.
#' @param input.types a character vector where the name corresponds to a column
#'        in \code{edit.cols} and the value is the input type. Possible values
#'        are \code{dateInput}, \code{selectInput}, \code{numericInput},
#'        \code{textInput}, \code{textAreaInput}, or \code{passwordInput}.
#'        The most common case where this parameter is desirable is when a text
#'        area is required instead of a simple text input.
#' @param input.choices
#' @param modal.size the size of the modal dialog. See \code{\link{modalDialog}}.
#' @param text.width
#' @param textarea.width
#' @param textarea.height
#' @param date.width
#' @param numeric.width
#' @param select.width
#' @param title.delete
#' @param title.edit
#' @param title.add
#' @param label.delete
#' @param label.edit
#' @param label.add
#' @param label.copy
#' @param show.delete
#' @param show.update
#' @param show.insert
#' @param show.copy
#' @param callback.delete
#' @param callback.update
#' @param callback.insert
#' @param click.time.threshold This is to prevent duplicate entries usually by double clicking the
#'        save or update buttons. If the user clicks the save button again within this amount of
#'        time (in seconds), the subsequent click will be ignored. Set to zero to disable this
#'        feature. For developers, a message is printed using the warning function.
#' @export
dtedit <- function(input, output, name, thedata,
				   view.cols = names(thedata),
				   edit.cols = names(thedata),
				   edit.label.cols = edit.cols,
				   input.types,
				   input.choices = NULL,
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
				   click.time.threshold = 2 # in seconds
) {
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

	DataTableName <- paste0(name, 'dt')

	result <- reactiveValues()
	result$thedata <- thedata
	result$view.cols <- view.cols
	result$edit.cols <- edit.cols

	dt.proxy <- DT::dataTableProxy(DataTableName)

	selectInputMultiple <- function(...) {
		selectInput(multiple = TRUE, selectize = selectize, ...)
	}

	valid.input.types <- c('dateInput', 'selectInput', 'numericInput',
						   'textInput', 'textAreaInput', 'passwordInput', 'selectInputMultiple')
	inputTypes <- sapply(thedata[,edit.cols], FUN=function(x) {
		switch(class(x),
			   list = 'selectInputMultiple',
			   character = 'textInput',
			   Date = 'dateInput',
			   factor = 'selectInput',
			   integer = 'numericInput',
			   numeric = 'numericInput')
	})
	if(!missing(input.types)) {
		if(!all(names(input.types) %in% edit.cols)) {
			stop('input.types column not a valid editting column: ',
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
		thedata[,view.cols]
	}, options = list(pageLength=defaultPageLength), server=TRUE, selection='single', rownames=FALSE)

	getFields <- function(typeName, values) {
		fields <- list()
		for(i in seq_along(edit.cols)) {
			if(inputTypes[i] == 'dateInput') {
				value <- ifelse(missing(values),
								as.character(Sys.Date()),
								as.character(values[,edit.cols[i]]))
				fields[[i]] <- dateInput(paste0(name, typeName, edit.cols[i]),
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
				fields[[i]] <- selectInputMultiple(paste0(name, typeName, edit.cols[i]),
										   label=edit.label.cols[i],
										   choices=choices,
										   selected=value,
										   width=select.width)

			} else if(inputTypes[i] == 'selectInput') {
				value <- ifelse(missing(values), '', as.character(values[,edit.cols[i]]))
				fields[[i]] <- selectInput(paste0(name, typeName, edit.cols[i]),
										   label=edit.label.cols[i],
										   choices=levels(result$thedata[,edit.cols[i]]),
										   selected=value,
										   width=select.width)
			} else if(inputTypes[i] == 'numericInput') {
				value <- ifelse(missing(values), 0, values[,edit.cols[i]])
				fields[[i]] <- numericInput(paste0(name, typeName, edit.cols[i]),
											label=edit.label.cols[i],
											value=value,
											width=numeric.width)
			} else if(inputTypes[i] == 'textAreaInput') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				fields[[i]] <- textAreaInput(paste0(name, typeName, edit.cols[i]),
											 label=edit.label.cols[i],
											 value=value,
											 width=textarea.width, height=textarea.height)
			} else if(inputTypes[i] == 'textInput') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				fields[[i]] <- textInput(paste0(name, typeName, edit.cols[i]),
										 label=edit.label.cols[i],
										 value=value,
										 width=text.width)
			} else if(inputTypes[i] == 'passwordInput') {
				value <- ifelse(missing(values), '', values[,edit.cols[i]])
				fields[[i]] <- passwordInput(paste0(name, typeName, edit.cols[i]),
										 label=edit.label.cols[i],
										 value=value,
										 width=text.width)
			} else {
				stop('Invalid input type!')
			}
		}
		return(fields)
	}

	output[[paste0(name, '_message')]] <- renderText('')

	updateData <- function(proxy, data, ...) {
		# Convert any list columns to characters before displaying
		for(i in 1:ncol(data)) {
			if(is.list(data[,i])) {
				data[,i] <- sapply(data[,i], FUN = function(x) { paste0(x, collapse = ', ') })
			}
		}
		DT::replaceData(proxy, data, ...)
	}

	##### Insert functions #####################################################

	observeEvent(input[[paste0(name, '_add')]], {
		if(!is.null(row)) {
			showModal(addModal())
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
			if(inputTypes[i] %in% c('selectInputMultiple')) {
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
						result$thedata[,view.cols],
						rownames = FALSE)
			removeModal()
			return(TRUE)
		}, error = function(e) {
		 	output[[paste0(name, '_message')]] <<- renderText(geterrmessage())
		 	return(FALSE)
		})
	})

	addModal <- function(row, values) {
		output[[paste0(name, '_message')]] <- renderText('')
		fields <- getFields('_add_', values)
		modalDialog(title = title.add,
					div(textOutput(paste0(name, '_message')), style='color:red'),
					fields,
					footer = column(modalButton('Cancel'),
									actionButton(paste0(name, '_insert'), 'Save'),
									width=12),
					size = modal.size
		)
	}

	##### Copy functions #######################################################

	observeEvent(input[[paste0(name, '_copy')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				showModal(addModal(values=result$thedata[row,]))
			}
		}
	})


	##### Update functions #####################################################

	observeEvent(input[[paste0(name, '_edit')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				showModal(editModal(row))
			}
		}
	})

	update.click <<- NA

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
					if(inputTypes[i] %in% c('selectInputMultiple')) {
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
								result$thedata[,view.cols],
								rownames = FALSE)
					removeModal()
					return(TRUE)
				}, error = function(e) {
					output[[paste0(name, '_message')]] <<- renderText(geterrmessage())
					return(FALSE)
				})
			}
		}
		return(FALSE)
	})

	editModal <- function(row) {
		output[[paste0(name, '_message')]] <- renderText('')
		fields <- getFields('_edit_', values=result$thedata[row,])
		modalDialog(title = title.edit,
			div(textOutput(paste0(name, '_message')), style='color:red'),
			fields,
			footer = column(modalButton('Cancel'),
							actionButton(paste0(name, '_update'), 'Save'),
							width=12),
			size = modal.size
		)
	}

	##### Delete functions #####################################################

	observeEvent(input[[paste0(name, '_remove')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				showModal(deleteModal(row))
			}
		}
	})

	observeEvent(input[[paste0(name, '_delete')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				newdata <- callback.delete(data = result$thedata, row = row)
				if(!is.null(newdata) & is.data.frame(newdata)) {
					result$thedata <- newdata
				} else {
					result$thedata <- result$thedata[-row,]
				}
				updateData(dt.proxy,
							result$thedata[,view.cols],
							rownames = FALSE)
				removeModal()
				return(TRUE)
			}
		}
		return(FALSE)
	})

	deleteModal <- function(row) {
		fields <- list()
		for(i in view.cols) {
			fields[[i]] <- div(paste0(i, ' = ', result$thedata[row,i]))
		}
		modalDialog(title = title.delete,
					p('Are you sure you want to delete this record?'),
					fields,
					footer = column(modalButton('Cancel'),
									actionButton(paste0(name, '_delete'), 'Delete'),
									width=12),
					size = modal.size
		)
	}

	##### Build the UI for the DataTable and buttons ###########################

	output[[name]] <- renderUI({
		div(
			if(show.insert) { actionButton(paste0(name, '_add'), label.add) },
			if(show.update) { actionButton(paste0(name, '_edit'), label.edit) },
			if(show.delete) { actionButton(paste0(name, '_remove'), label.delete) },
			if(show.copy) { actionButton(paste0(name, '_copy'), label.copy) },
			br(), br(), DT::dataTableOutput(DataTableName)

		)
	})

	return(result)
}
