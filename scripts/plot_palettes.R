## Print palettes from the paletteer package

library(paletteer)
library(neyhart)
library(tidyverse)

# Get the df of discretepalettes
palette_df <- palettes_d_names

## Set some parameters
palettes_per_page <- 5

## Split the df
palette_df_split <- palette_df %>%
  assign_cores(n_core = ceiling(nrow(palette_df) / palettes_per_page), split = TRUE)


## Open a PDF
pdf(file = "palettes%03d.pdf", width = 8.5, height = 11)

# Set plotting parameters
par(mfrow = c(palettes_per_page, 1), mar = rep(20, 4))

## Iterate over pages
for (p in seq_along(palette_df_split)) {
  
  page <- palette_df_split[[p]]
  
  # Iterate over palettes
  for (i in seq_len(nrow(page))) {
    
    row <- page[i,]
    pkg <- row$package
    pal <- row$palette
    
    # Create a palette object
    pal_obj <- structure(palettes_d[[pkg]][[pal]], class = "palette", name = paste0(pkg, ":", pal))
    # Print
    print(pal_obj)
    
  }
  
}


# Close the PDF
dev.off()
