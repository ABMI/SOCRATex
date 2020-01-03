#' saveJson
#'
#' This function save previously annotated data using SOCRATex into JSON files.
#'
#' @param JSON Previously defined empty object to conatin JSON data
#' @import tm
#' @export

saveJson <- function(JSON){
  if(dir.exists(paste0(getwd(), "/JSON"))==FALSE){dir.create(path = paste0(getwd(), "/JSON"))}
  for(i in 1:length(JSON)){
    JSON[i] <- tm::stripWhitespace(JSON[i])
    JSON[i] <- jsonlite::toJSON(JSON[i])
    JSON[i] <- gsub('["{', '[{', JSON[i], fixed = T)
    JSON[i] <- gsub('}"]', '}]', JSON[i], fixed = T)
    JSON[i] <- gsub('} "]', '}]', JSON[i], fixed = T)
    JSON[i] <- gsub('\\"', '"', JSON[i], fixed = T)
    write(JSON[i], paste0(getwd(), "/JSON", "/json",i,".json"))
  }
}
