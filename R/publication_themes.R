#' Plotting theme for a poster
#' 
#' @description 
#' Theme for ggplot2 designed for scientific poster displays.
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
          legend.title = element_text(size = 16),
          title = element_text(size = 16),
          panel.grid = element_blank())
}


#' Plotting theme for publications
#' 
#' @description 
#' Themes for ggplot2 designed for compliance with variance scientific journals.
#' 
#' @rdname themes
#' 
#' @details 
#' 
#' \describe{
#'   \item{\code{theme_acs}}{Theme for the American Society of Agronomy (ASA), 
#'   Crop Science Society of America (CSSA), and Soil Science Society of America (SSSA).}
#'   \item{\code{theme_pnas}}{Theme for the Proceedings of the National Academy of Sciences.}
#' }
#' 
#' @import ggplot2
#' 
#' @export
#' 
#' 
theme_acs <- function(base_size = 8) {
  
  theme_bw(base_size = base_size) %+replace%
    theme(panel.grid = element_blank())
  
}


#'
#' @rdname themes
#' 
#' @export
#' 
theme_pnas <- function(base_size = 6) {
  
  theme_classic(base_size = base_size) %+replace%
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey85", size = 0),
        axis.title = element_text(size = 6),
        legend.text = element_text(size = 6))
  
}





