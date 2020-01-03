#' jsonEditor
#'
#' jsonEditor is a JSON viewer using listviewer. JsonEditor enables to upload or download the JSON files and easily can edit.
#'
#' @param filePath  The file path which indicates the location of the JSON file when uploading it.
#' @import listviewer
#' @import jsonlite
#' @export

jsonEditor <- function(filePath){
  listviewer::jsonedit(jsonSchemaList <<- jsonlite::fromJSON(
    if(is.null(filePath)){
      jsonSchemaList <- jsonlite::toJSON('{"jsonSchema":"Please upload or create your own JSON schema here. This will be used as a validation process in annotation step."}')
    }

    else{
      jsonSchemaList <- filePath
    }
  )
  ,onChange=htmlwidgets::JS("function(){
                                  Shiny.onInputChange(
                                    'Schema_change',
                                    HTMLWidgets.getInstance(document.getElementById('Schema')).editor.getText()
                                  );
                            }"
  ))
}
