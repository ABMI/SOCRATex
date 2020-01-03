#' useDictionary
#'
#' This function preprocess data based on the user customized pre-defined dictionary. Currerntly, it used the dictionary in Sql Server.
#' The dictionary needs to has two columns, first is the exporession
#' @param useDictionary If it is TRUE then it reads the dictionary database and perform preprocessing
#'
#' @import DatabaseConnector
#' @export

useDictionary <- function(useDictionary = FALSE){
  if(useDictionary==T){
    Dict <- DatabaseConnector::dbReadTable(connection, input$dictionary_table)
    Text_corpus <<- dictionary(Dict, Text$NOTE_TEXT)
  } else{
    Text_corpus <<- Text$NOTE_TEXT
  }
}
