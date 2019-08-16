#' Dictionary function
#'
#' @param x   an expression want to be replaced or removed
#' @param y   an expression want to replace
#'
#' @export

dictionary <- function(x, y){
  for(i in 1:nrow(x)){
    y <- gsub(x[i,1], x[i,2], y)
  }
  return(y)
}
