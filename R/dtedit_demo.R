#' Run a shiny app showing how the DTedit function works.
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
  dir <- paste0(find.package("DTedit"), "/examples/example_reactivedataframe.R")
  message(paste0("Running shiny app from ", dir))
  shiny::shinyAppDir(appDir = dir, ...)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' selectInputReactive
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
  dir <- paste0(find.package("DTedit"), "/examples/example_selectInputReactive.R")
  message(paste0("Running shiny app from ", dir))
  shiny::shinyAppDir(appDir = dir, ...)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' modularized version
#' 
#' selectInputReactive
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
  dir <- paste0(find.package("DTedit"), "/shiny_demo/app_mod.R")
  message(paste0("Running shiny app from ", dir))
  shiny::shinyAppDir(appDir = dir, ...)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' modularized version
#' 
#' file input, blobs and action buttons
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
  dir <- paste0(find.package("DTedit"), "/examples/example_mod_fileInput.R")
  message(paste0("Running shiny app from ", dir))
  shiny::shinyAppDir(appDir = dir, ...)
}