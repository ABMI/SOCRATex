#' SOCRATex function
#'
#' This is a main function which can implement SOCRATex system and other functions in SOCRATex package.
#'
#' @import shiny
#' @import shinyWidgets
#' @import dplyr
#' @import SnowballC
#' @import tm
#' @import topicmodels
#' @import htmlwidgets
#' @import plotly
#' @import DatabaseConnector
#' @import rJava
#' @import SqlRender
#' @importFrom NLP ngrams
#' @importFrom NLP words
#' @importFrom stringi stri_count
#' @importFrom modeltools posterior
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom shinyjs useShinyjs
#' @importFrom shinythemes shinytheme
#' @importFrom elastic index_exists
#' @importFrom elastic index_delete
#' @importFrom elastic connect
#' @importFrom elastic docs_bulk
#' @importFrom elastic index_create
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom jsonvalidate json_validator
#' @importFrom listviewer jsoneditOutput
#' @importFrom listviewer renderJsonedit
#' @importFrom listviewer jsonedit
#' @importFrom LDAvis createJSON
#' @importFrom LDAvis renderVis
#' @importFrom LDAvis visOutput
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
