#' SOCRATex functiogithun
#'
#' This is a main function which can implement SOCRATex system and other functions in SOCRATex package.
#'
#' @import shiny (>= 1.3.2.9001),
#' @import shinyjs (>= 1.0),
#' @import shinythemes (>= 1.1.2),
#' @import shinyWidgets (>= 0.4.8.920),
#' @import elastic (>= 1.0.0),
#' @import jsonlite (>= 1.6.9000),
#' @import jsonvalidate (>= 1.1.0),
#' @import listviewer (>= 2.1.0),
#' @import LDAvis (>= 0.3.2),
#' @import SnowballC (>= 0.6.0),
#' @import tm,
#' @import topicmodels,
#' @import htmlwidgets (>= 1.3)
#' @import ployly,
#' @import DatabaseConnector,
#' @import SqlRender,
#' @import stringi,
#' @import xlsx,
#' @import rJava,
#' @import shinyFiles,
#' @import DT
#'
#' @export

SOCRATex <- function(){
  filePath <- paste0(.libPaths()[1],"/SOCRATex")
  shiny::runApp(filePath)
}
