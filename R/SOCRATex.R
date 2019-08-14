#' SOCRATex functiogithun
#'
#' This is a main function which can implement SOCRATex system and other functions in SOCRATex package.
#'
#' @import shiny
#' @import shinyjs
#' @import shinythemes
#' @import shinyWidgets
#' @import elastic
#' @import jsonlite
#' @import jsonvalidate
#' @import listviewer
#' @import LDAvis
#' @import SnowballC
#' @import tm
#' @import topicmodels
#' @import htmlwidgets
#' @import plotly
#' @import DatabaseConnector
#' @import SqlRender
#' @import stringi
#' @import xlsx
#' @import rJava
#' @import shinyFiles
#' @import DT
#'
#' @export

SOCRATex <- function(){
  filePath <- paste0(.libPaths()[1],"/SOCRATex")
  shiny::runApp(filePath)
}
