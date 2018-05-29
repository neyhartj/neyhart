#' Plotting theme for GWAS results
#' 
#' @description 
#' Themes for ggplot2 designed for GWAS results.
#' 
#' @import ggplot2
#' 
#' @export
#' 
theme_manhattan <- function() {
  
  theme_bw() +
    theme(panel.spacing.x = unit(x = 0, units = "cm"),
          panel.grid = element_blank(),
          panel.border = element_rect(color = "grey75"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
}

