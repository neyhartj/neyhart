#' ggplot2 themes for GWAS results
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


#' ggplot2 theme for a poster
#' 
#' @import ggplot2
#' 
#' @export
#' 
theme_poster <- function() {
  
  theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16))
  
}
