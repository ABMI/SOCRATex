#' SOCRATex function
#'
#' This is a main function which can implement SOCRATex application.
#'
#' @import shiny
#' @import shinyWidgets
#' @import htmlwidgets
#' @import rJava
#' @importFrom NLP ngrams
#' @importFrom NLP words
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom shinyjs useShinyjs
#' @importFrom shinythemes shinytheme
#'
#' @export

SOCRATex <- function(){

  if(exists('jsonSchemaList')==FALSE){
    jsonSchemaList <<- c()
  }
  if(exists('jsonList')==FALSE){
    jsonList <<- c()
  }

  filePath <- paste0(.libPaths()[1],"/SOCRATex")
  shiny::runApp(filePath)
}
