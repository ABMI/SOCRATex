#' jsonTemplateEditor
#'
#' jsonTemplateEditor is a JSON viewer using listviewer package.
#'
#' @param filePath  The file path which indicates the location of the JSON file when uploading it.
#' @import listviewer
#' @import jsonlite
#' @export

jsonTemplateEditor <- function(filePath){
  listviewer::jsonedit(jsonList <<- jsonlite::fromJSON(
    if(is.null(filePath)){
      jsonList <- jsonlite::toJSON('{"jsonTemplate":"Please upload or create your own JSON template for annotation. This will be used for actual annotation process in next step."}')
    }
    else{
      jsonList <- filePath
    }
  )
  ,onChange=htmlwidgets::JS("function(){
                                  Shiny.onInputChange(
                                    'Template_change',
                                    HTMLWidgets.getInstance(document.getElementById('Template')).editor.getText()
                                  );
                            }"
  ))
}
