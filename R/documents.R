#' Render a custom lab document from the Neyhart template
#' 
#' A template for laboratory documents, mostly for create the "Lab Philosophy" 
#' document.
#' 
#' @inheritParams rmarkdown::pdf_document
#' @param ... Arguments to [`rmarkdown::pdf_document`].
#' @md
#' @export
#'
labdocument <- function(...){
  templ <- system.file("rmarkdown", "templates", "lab_overview", "resources", "template.tex", package = "neyhart")
  rmarkdown::pdf_document(template = templ, ...)
}

#' @rdname labdocument
#' @export
templ_labdocument <- function() {
  print(system.file("rmarkdown", "templates", "lab_overview", "resources", "template.tex", package = "neyhart"))
}


#' Render a laboratory protocol document from the Neyhart template
#' 
#' A template for protocols in the Neyhart Lab.
#' 
#' @inheritParams rmarkdown::pdf_document
#' @param ... Arguments to [`rmarkdown::pdf_document`].
#' @md
#' @export
#'
protocol <- function(...){
  templ <- system.file("rmarkdown", "templates", "protocol", "resources", "template.tex", package = "neyhart")
  rmarkdown::pdf_document(template = templ, ...)
}

#' @rdname protocol
#' @export
templ_protocol <- function() {
  print(system.file("rmarkdown", "templates", "protocol", "resources", "template.tex", package = "neyhart"))
}

