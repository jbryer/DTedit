#' Run a shiny app showing how the DTedit function works.
#' 
#' Demonstrates adding/editing/deleting data rows,
#'  callbacks, interacting with a database,
#'  selectInput and selectInputMultiple.
#'  
#' @param ... arguments pass to \code{runApp}
#'  
#'  For example, can launch in showcase mode:
#'  
#'  \code{DTedit::dtedit_demo(display.mode = "showcase")}
#'  
#'  Note that 'showcase' mode shows all
#'  the .R files in the 'shiny_demo' directory, not
#'  just the .R file used for this demonstration!
#'  
#' @export
dtedit_demo <- function(...) {
  dir <- paste0(find.package("DTedit"), "/shiny_demo")
  message(paste0("Running shiny app from ", dir))
  shiny::shinyAppDir(appDir = dir, ...)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' reactive dataframe
#' 
#' @param ... arguments pass to \code{runApp}
#'  
#'  For example, can launch in showcase mode:
#'  
#'  \code{DTedit::dtedit_reactive_demo(display.mode = "showcase")}
#'  
#'  Note that 'showcase' mode shows all
#'  the .R files in the 'shiny_demo' directory, not
#'  just the .R file used for this demonstration!
#'
#' @export
dtedit_reactive_demo <- function(...) {
  dir <- paste0(find.package("DTedit"), "/examples/reactivedataframe")
  message(paste0("Running shiny app from ", dir))
  shiny::shinyAppDir(appDir = dir, ...)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' demonstrates interaction between datatables using
#'  selectInputReactive and selectInputMultipleReactive
#'
#' @param ... arguments pass to \code{runApp}
#'  
#'  For example, can launch in showcase mode:
#'  
#'  \code{DTedit::dtedit_selectInputReactive_demo(display.mode = "showcase")}
#'  
#'  Note that 'showcase' mode shows all
#'  the .R files in the 'shiny_demo' directory, not
#'  just the .R file used for this demonstration!
#'
#' @export
dtedit_selectInputReactive_demo <- function(...) {
  dir <- paste0(find.package("DTedit"), "/examples/selectInputReactive")
  message(paste0("Running shiny app from ", dir))
  shiny::shinyAppDir(appDir = dir, ...)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' modularized version of dtedit_demo and dtedit_selectInputReactive_demo.
#'  Uses dteditmod/dteditmodUI version of dtedit.
#' 
#' Demonstrates adding/editing/deleting data rows, callbacks,
#'  interaction with database.
#'  
#' Demonstrates interaction between datatables using reactives
#'
#' @param ... arguments pass to \code{runApp}
#'  
#'  For example, can launch in showcase mode:
#'  
#'  \code{DTedit::dteditmod_demo(display.mode = "showcase")}
#'  
#'  Note that 'showcase' mode shows all
#'  the .R files in the 'shiny_demo' directory, not
#'  just the .R file used for this demonstration!
#'
#' @export
dteditmod_demo <- function(...) {
  filename <- paste0(find.package("DTedit"), "/shiny_demo/app_mod.R")
  message(paste0("Running shiny app from ", filename))
  shiny::shinyAppFile(appFile = filename, ...)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' Demonstrates file input, blobs and action buttons
#'
#' (modularized, uses dteditmod/dteditmodUI)
#'
#' @param ... arguments pass to \code{runApp}
#'  
#'  For example, can launch in showcase mode:
#'  
#'  \code{DTedit::dteditmod_fileInput_demo(display.mode = "showcase")}
#'  
#'  Note that 'showcase' mode shows all
#'  the .R files in the 'shiny_demo' directory, not
#'  just the .R file used for this demonstration!
#'
#' @export
dteditmod_fileInput_demo <- function(...) {
  dir <- paste0(find.package("DTedit"), "/examples/fileInput_modular")
  message(paste0("Running shiny app from ", dir))
  shiny::shinyAppDir(appDir = dir, ...)
}