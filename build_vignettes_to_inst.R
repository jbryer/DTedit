build_vignettes_to_inst <- function() {
  devtools::build_vignettes() # Builds vignettes to 'doc' and 'Meta'. Updates '.gitignore'.
  unlink(c("inst/doc", "inst/Meta"), recursive = TRUE) # Remove the directories if they exist
  dir.create("inst/doc"); dir.create("inst/Meta") # Create empty directories
  has_worked <- c( # Copy files to 'inst' subfolders
    file.copy(list.files("doc", full.names = TRUE), to = "inst/doc")
    , file.copy(list.files("Meta", full.names = TRUE), to = "inst/Meta")
  )
  unlink(c("doc", "Meta"), recursive = TRUE) # Optional: Remove unwanted directories
  return(all(has_worked)) # Returns TRUE if everything worked OK
}
