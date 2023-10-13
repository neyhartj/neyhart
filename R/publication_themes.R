#' Plotting theme for presentations or posters
#' 
#' @description 
#' Theme for ggplot2 designed for posters or presentations
#' 
#' @rdname pres_themes
#' 
#' @details 
#' 
#' \describe{
#'   \item{\code{theme_poster}}{Theme for posters.}
#'   \item{\code{theme_presentation}}{Theme for presentations.}
#'   \item{\code{theme_presentation2}}{Alternate theme for presentations.}
#' }
#' 
#' @import ggplot2
#' 
#' @export
#' 
theme_poster <- function(base_size = 16) {
  
  theme_bw() +
    theme(axis.text = element_text(size = base_size * 0.75),
          axis.title = element_text(size = base_size),
          strip.text = element_text(size = base_size * 0.875),
          legend.text = element_text(size = base_size * 0.875),
          legend.title = element_text(size = base_size),
          title = element_text(size = base_size),
          panel.grid = element_blank())
}


#' @rdname pres_themes
#' 
#' @export
#' 
theme_presentation <- function(base_size = 16) {
  theme_minimal(base_size = base_size) %+replace%
    theme(strip.background = element_rect(fill = "grey85", linetype = 0),
          legend.background = element_rect(fill = "white", linetype = 0), 
          complete = TRUE)
} 

#' @rdname pres_themes
#' 
#' @export
#' 
theme_presentation2 <- function(base_size = 16) {
  theme_minimal(base_size = base_size) %+replace%
    theme(strip.background = element_rect(fill = "grey85", linetype = 0),
          legend.background = element_rect(fill = "white", linetype = 0), 
          axis.ticks = element_line(), 
          panel.grid = element_blank(),
          panel.border = element_rect(fill = alpha("white", 0), color = "grey85"),
          complete = TRUE)
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
#'   \item{\code{theme_genetics}}{Theme for GENETICS/G3.}
#' }
#' 
#' @import ggplot2
#' 
#' @examples 
#' g <- qplot(x = disp, y = hp, data = mtcars)
#' 
#' 
#' @export
#' 
theme_neyhart <- function(base_size = 8) {
  # # Set text sizes
  # theme_classic(base_size = base_size * 1.5) %+replace%
  #   theme(axis.text = element_text(size = base_size),
  #         legend.text = element_text(size = base_size),
  #         axis.title = element_text(size = base_size * (4/3)),
  #         legend.title = element_text(size = base_size * (4/3)),
  #         strip.text = element_text(size = base_size * (4/3)),
  #         plot.title = element_text(size = base_size * 1.5),
  #         plot.subtitle = element_text(size = base_size * 1.25),
  #         plot.caption = element_text(size = base_size))
  
  theme_classic(base_size = base_size)
  
}

#' 
#' 
#' @rdname themes
#' @export
#' 
theme_acs <- function(base_size = 8) {
  
  theme_neyhart(base_size = base_size) %+replace%
    theme(panel.grid = element_blank(), panel.border = element_rect(fill = alpha("white", 0)))
  
}


#' Plotting theme for publications
#'
#' @rdname themes
#' 
#' @export
#' 
theme_pnas <- function(base_size = 8) {
  
  theme_neyhart(base_size = base_size) %+replace%
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey85", size = 0))
  
}


#' Plotting theme for publications
#'
#' @rdname themes
#' 
#' @export
#' 
theme_genetics <- function(base_size = 8) {
  
  theme_neyhart(base_size = base_size) %+replace%
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "grey85", color = "gray85", size = 0))
  
}




