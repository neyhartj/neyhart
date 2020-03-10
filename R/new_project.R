#' Set up a new project
#' 
#' @description 
#' This function uses the \code{\link[usethis]{usethis-package}} package to set up a new
#' R project in RSudio.
#' 
#' @param project.name The name of the project. 
#' @param dir The root directory of the new project.
#' @param use.this Logical: should a git repository be initialized?
#' @param use.msi Logical: should the project be setup for use on a remote server?
#' 
#' @details 
#' This function create a new project and does the following:
#' 
#' 
#' 
#' @import usethis
#' 
#' @export
#' 
new_project <- function(project.name, dir = ".", use.git = TRUE, use.msi = TRUE) {
  
  ## Error
  stopifnot(is.character(project.name))
  stopifnot(is.character(dir))
  stopifnot(is.logical(use.git))
  stopifnot(is.logical(use.msi))

  # Create a path for the new project
  path <- file.path(dir, project.name)
  
  ## Create the project
  create_project(path = path, rstudio = TRUE, open = FALSE)
  
  # Set the active project to this project
  proj_set(path = path)
  
  ## Create folders in the directory
  use_directory(path = "Data")
  use_directory(path = "Figures")
  use_directory(path = "Results")
  
  ## Create a readme
  use_readme_rmd(open = FALSE)
  
  # Create startup.R and functions.R
  use_template(template = "startup.R", save_as = "startup.R", package = "neyhart")
  use_template(template = "functions.R", save_as = "functions.R", package = "neyhart")
  
  # If using MSI; use the startup_MSI template
  if (use.msi) {
    use_template(template = "startup_MSI.R", save_as = "startup_MSI.R", package = "neyhart")
    use_template(template = "pbs_template.sh", save_as = "pbs_template.sh", package = "neyhart")
    
  }

  ## Initialize a git repo, if called
  if (use.git) {
    
    use_git()

    ## Set results and figures and readme to gitignore
    use_git_ignore(ignores = c(".Rhistory", ".Rdata", ".Ruserdata", ".DS_Store"))
    use_git_ignore(ignores = c("Figures", "Results"))
    use_git_ignore(ignores = "README.Rmd")
    
    ## Also add *.o* and *.e* for MSI output
    if (use.msi) use_git_ignore(ignores = c("*.o*", "*.e*"))

  }
  
  # Notify the user
  cat("\n")
  ui_done(paste0("Project '", project.name, "' created."))
  
}
  
  
  
  