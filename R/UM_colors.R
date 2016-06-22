#' University of Minnesota Color Palette
#'
#' @description A vector of hexadecimal color codes that are designed to work well together with the University of Minnesota color scheme.
#'
#' @format A character vector of hexidecimal codes.
#'
#' @source \url{https://www.ur.umn.edu/brand/assets/pdf/secondary_colors_rgb.pdf}
#'
"UMN.colors"


#' University of Minnesota Color Generator
#'
#' @description Generates hexidecimal colors from the University of Minnesota
#' color palette.
#'
#' @param n Integer number of colors to be generated
#'
#' @return A character vector of hexidecimal color codes from the University
#' of Minnesota color palette.
#'
#' @details
#'
#' @export
#'

UMN.color.palette <- function(n) {
  #Error
  if (n < 1) stop("n cannot be less than 1.")
  if (n > length(UMN.colors)) stop("n cannot be greater than the total length of the palette.")

  return(UMN.colors[1:n])

}
