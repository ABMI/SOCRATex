#' SOCRATex functiogithun
#'
#' This is a main function which can implement SOCRATex system and other functions in SOCRATex package.
#'
#' @export
SOCRATex <- function(){
  filePath <- paste0(.libPaths()[1],"/SOCRATex")
  shiny::runApp(filePath)
}
