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
#' modularized version
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
#' @export
dteditmod_fileInput_demo <- function() {
  dir <- paste0(find.package("DTedit"), "/shiny_demo/app_mod_fileInput.R")
  message(paste0("Running shiny app from ", dir))
  shiny::runApp(appDir = dir)
}