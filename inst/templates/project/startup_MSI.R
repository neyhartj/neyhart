## Project name here
## 
## This is a script to startup a project on MSI
## 

# Load packages
packages <- c("")

invisible(lapply(packages, library, character.only = TRUE))

## Directories
proj_dir <- "/path/to/project/on/remote/server/"



# Other directories
fig_dir <- file.path(proj_dir, "Figures")
data_dir <- file.path(proj_dir, "Data")
result_dir <- file.path(proj_dir, "Results")


## Source the 'startup.R' script from a define starting point
source_lines <- readLines(file.path(proj_dir, "startup.R"))
source_lines_discard <- seq(which(grepl(pattern = "^# MSI", x = source_lines)))
source_lines_run <- source(textConnection(source_lines[-source_lines_discard]))

