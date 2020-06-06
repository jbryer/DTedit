#' Run a shiny app showing how the DTedit function works.
#'
#' @export
dtedit_demo <- function() {
  dir <- paste0(find.package("DTedit"), "/shiny_demo")
  message(paste0("Running shiny app from ", dir))
  shiny::runApp(appDir = dir)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' reactive dataframe
#'
#' @export
dtedit_reactive_demo <- function() {
  dir <- paste0(find.package("DTedit"), "/shiny_demo/app_reactivedataframe.R")
  message(paste0("Running shiny app from ", dir))
  shiny::runApp(appDir = dir)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' selectInputReactive
#'
#' @export
dtedit_selectInputReactive_demo <- function() {
  dir <- paste0(find.package("DTedit"), "/shiny_demo/app_selectInputReactive.R")
  message(paste0("Running shiny app from ", dir))
  shiny::runApp(appDir = dir)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' modularized version
#' 
#' selectInputReactive
#'
#' @export
dteditmod_demo <- function() {
  dir <- paste0(find.package("DTedit"), "/shiny_demo/app_mod.R")
  message(paste0("Running shiny app from ", dir))
  shiny::runApp(appDir = dir)
}

#' Run a shiny app showing how the DTedit function works.
#'
#' modularized version
#' 
#' file input, blobs and action buttons
#'
#' @export
dteditmod_fileInput_demo <- function() {
  dir <- paste0(find.package("DTedit"), "/shiny_demo/app_mod_fileInput.R")
  message(paste0("Running shiny app from ", dir))
  shiny::runApp(appDir = dir)
}