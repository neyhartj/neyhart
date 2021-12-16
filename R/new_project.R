#' Create a new project
#' 
#' @description 
#' This function performs the setup for creating new projects of various types, 
#' including analyses, posters, and packages.
#' 
#' @param project.name The name of the project. 
#' @param dir The root directory of the new project.
#' 
#' @details 
#' The function supports the creation of projects of the following types:
#' 
#' \describe{
#'   \item{analysis}{Directory setup and template scripts for conducting an analysis}
#'   \item{poster}{Directory setup and template copying for producing a poster in LaTeX}
#'   \item{package}{Setup for creating a new R package}
#' } 
#' 
#' 
#' @import usethis
#' 
#' @export
#' 
new_project <- function(project.name, dir = ".", type = c("analysis", "poster", "package")) {
  
  ## Error handling
  stopifnot(is.character(project.name))
  stopifnot(is.character(dir))
  stopifnot(dir.exists(dir))
  
  # Match arguments
  type <- match.arg(type)
  
  ## Prompt user for using git or msi
  use_git <- ui_yeah("Initialize a git repository?")
  
  ## Prompt for github
  use_github <- ui_yeah("Create and push to a GitHub repository?")
  
  # # MSI only if the project type is analysis
  # if (type == "analysis") {
  #   use_msi <- ui_yeah("Provide templates for submitting scripts to MSI?")
  # } else {
  #   use_msi <- FALSE
  # }
  
  # SCINET only if the project type is analysis
  if (type == "analysis") {
    use_hpc <- ui_yeah("Provide templates for submitting scripts to SCINet?")
  } else {
    use_hpc <- FALSE
  }
  
  
  ## Choose function based on the project type
  fun <- switch(type, analysis = new_analysis, poster = new_poster, package = new_package)
  # Run the function
  fun(project.name = project.name, dir = dir, use.git = use_git, use.github = use_github, use.hpc = use_hpc)
         
} # End

#' @describeIn new_project
#' 
#' @import usethis
#' @export
#' 
new_analysis <- function(project.name, dir = ".", use.hpc = TRUE, use.git = TRUE, use.github = TRUE) {
  
  ## Error
  stopifnot(is.character(project.name))
  stopifnot(is.character(dir))
  stopifnot(is.logical(use.git))
  stopifnot(is.logical(use.hpc))

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
  # Rename the R directory to "Scripts"
  invisible(file.rename(from = "R/", to = "Scripts/"))
  
  ## Create a readme
  use_readme_rmd(open = FALSE)
  
  # Create startup.R and functions.R
  use_template(template = "project/startup.R", save_as = "startup.R", package = "neyhart")
  use_template(template = "project/functions.R", save_as = "functions.R", package = "neyhart")
  
  # If using MSI; use the startup_MSI template
  if (use.hpc) {
    # use_template(template = "project/startup_MSI.R", save_as = "startup_MSI.R", package = "neyhart")
    # use_template(template = "project/pbs_template.sh", save_as = "pbs_template.sh", package = "neyhart")
    
    use_template(template = "project/startup_SCINet.R", save_as = "startup_SCINet.R", package = "neyhart")
    use_template(template = "project/scinet_template.sh", save_as = "scinet_template.sh", package = "neyhart")
    
  }

  ## Initialize a git repo, if called
  if (use.git) {
    
    use_git()

    ## Set results and figures and readme to gitignore
    use_git_ignore(ignores = c(".Rhistory", ".Rdata", ".Ruserdata", ".DS_Store"))
    use_git_ignore(ignores = c("Figures", "Results"))
    use_git_ignore(ignores = "README.Rmd")
    
    ## Also add *.o* and *.e* for MSI output
    if (use.hpc) use_git_ignore(ignores = c("*.o*", "*.e*"))
    
    
    ## Push to github, if called
    if (use.github) {
      use_github()
      
    }

  }

  
  
  # Notify the user
  cat("\n")
  ui_done(paste0("Project '", project.name, "' created."))
  
}


#' @describeIn new_project
#' 
#' @import usethis
#' @export
#' 
new_poster <- function(project.name, dir = ".", use.hpc = FALSE, use.github = TRUE, use.git = TRUE) {
  
  ## Error
  stopifnot(is.character(project.name))
  stopifnot(is.character(dir))
  stopifnot(is.logical(use.git))
  stopifnot(is.logical(use.hpc))
  
  # Create a path for the new project
  path <- file.path(dir, project.name)
  
  ## Create the project
  create_project(path = path, rstudio = TRUE, open = FALSE)
  
  # Set the active project to this project
  proj_set(path = path)
  
  ## Create directories
  use_directory("figures")
  ## Copy folders and templates to the new directory
  latex_to_copy <- system.file("templates/poster/supporting_files/", package = "neyhart")
  invisible(file.copy(from = latex_to_copy, to = path, overwrite = TRUE, recursive = TRUE))
  # Notify
  ui_done("Creating {ui_path(basename(latex_to_copy))}")
  
  # Copy templates
  # Tex file
  to_copy <- system.file("templates/poster/skeleton.tex", package = "neyhart")
  save_as <- file.path(path, paste0(project.name, "_poster.tex"))
  invisible(file.copy(from = to_copy, to = path, overwrite = TRUE, recursive = TRUE))
  invisible(file.rename(from = file.path(path, "skeleton.tex"), to = save_as))
  ui_done("Writing {ui_path(basename(save_as))}")
  
  
  # R file
  to_copy <- system.file("templates/poster/poster_support_code.R", package = "neyhart")
  save_as <- file.path(path, paste0(project.name, "_poster_support_code.R"))
  invisible(file.copy(from = to_copy, to = path, overwrite = TRUE, recursive = TRUE))
  invisible(file.rename(from = file.path(path, "poster_support_code.R"), to = save_as))
  ui_done("Writing {ui_path(basename(save_as))}")
  
  # Latex theme files
  to_copy <- system.file("templates/poster/beamerposter.sty", package = "neyhart")
  save_as <- file.path(path, "beamerposter.sty")
  invisible(file.copy(from = to_copy, to = path, overwrite = TRUE, recursive = TRUE))
  invisible(file.rename(from = file.path(path, "beamerposter.sty"), to = save_as))
  ui_done("Writing {ui_path(basename(save_as))}")
  
  to_copy <- system.file("templates/poster/beamerthemeconfposter.sty", package = "neyhart")
  save_as <- file.path(path, "beamerthemeconfposter.sty")
  invisible(file.copy(from = to_copy, to = path, overwrite = TRUE, recursive = TRUE))
  invisible(file.rename(from = file.path(path, "beamerthemeconfposter.sty"), to = save_as))
  ui_done("Writing {ui_path(basename(save_as))}")
  
  
  # If using MSI; use the startup_MSI template
  if (use.hpc) {
    use_template(template = "project/pbs_template.sh", save_as = "pbs_template.sh", package = "neyhart")
    
  }
  
  
  ## Create a readme
  use_readme_rmd(open = FALSE)
  
  ## Initialize a git repo, if called
  if (use.git) {
    
    use_git()
    
    ## Set results and figures and readme to gitignore
    use_git_ignore(ignores = c(".Rhistory", ".Rdata", ".Ruserdata", ".DS_Store"))
    use_git_ignore(ignores = c("figures", "latex"))
    use_git_ignore(ignores = "README.Rmd")
    
    ## Also add *.o* and *.e* for MSI output
    if (use.hpc) use_git_ignore(ignores = c("*.o*", "*.e*"))
    
    ## Push to github, if called
    if (use.github) {
      use_github()
      
    }
    
  }
  
  # Notify the user
  cat("\n")
  ui_done(paste0("Project '", project.name, "' created."))
  
}
  
  
 
#' @describeIn new_project
#' 
#' @import usethis
#' @export
#' 
new_package <- function(project.name, dir = ".", use.hpc = FALSE, use.git = TRUE, use.github = TRUE) {
  
  ## Error
  stopifnot(is.character(project.name))
  stopifnot(is.character(dir))
  stopifnot(is.logical(use.git))

  # Create a path for the new project
  path <- file.path(dir, project.name)
  
  ## Create the package
  create_package(path = path, rstudio = TRUE, open = FALSE)
  
  # Set the active project to this project
  proj_set(path = path)
  
  ## Create a readme
  use_readme_rmd(open = FALSE)
  # Create a license
  use_mit_license("Author")
  
  ## Initialize a git repo, if called
  if (use.git) {
    
    use_git()
    
    ## Set results and figures and readme to gitignore
    use_git_ignore(ignores = "README.Rmd")
    
    ## Push to github, if called
    if (use.github) {
      use_github()
      
    }
    
  }
  
  # Notify the user
  cat("\n")
  ui_done(paste0("Package '", project.name, "' created."))
  
}

  