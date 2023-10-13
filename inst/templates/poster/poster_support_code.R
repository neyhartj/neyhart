## Poster support code
## 
## Generate tables and figures for the poster
## 

# Packages to load
library(tidyverse)

# Project directories
proj_dir <- "path/to/project"

# Directories
pheno_dir <- file.path(proj_dir, "Data")
result_dir <- file.path(proj_dir, "Results")
fig_dir <- file.path(getwd(), "poster_code/figures/")


## Dimensions to help with producting figures
paper_width <- 34 # (inches)
paper_height <- 46

one_col <- 0.3013333 * paper_width
two_col <- 0.6266666 * paper_width
three_col <- 0.9279999 * paper_width

