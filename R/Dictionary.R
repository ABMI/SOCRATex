dictionary <- function(x, y){
  for(i in 1:nrow(x)){
    y <- gsub(x[i,1], x[i,2], y)
  }
  return(y)
}