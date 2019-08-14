#' jsonToES function
#'
#' This is a function to send JSON documents to Elasticsearch
#'
#' @import elastic
#'
#' @param esConnection  a connection with Elasticsearch
#' @param indexName     an index name to send Elasticsearch
#' @param jsonFolder    a folder which contains the JSON documents
#' @param dropIfExist  If TRUE then delete existing Index in Elasticsearch
#'
#' @export

jsonToES <- function(esConnection, indexName, jsonFolder, dropIfExist = F){
  json_list<- list.files(jsonFolder,pattern = "*.json$",full.names = T)
  dataset <- lapply(json_list, function(json) {fromJSON(json)})
  if(elastic::index_exists(esConnection,indexName)){
    if(dropIfExist){
      elastic::index_delete(esConnection,indexName)
    }
  }
  else{
    elastic::index_create(esConnection, indexName)
  }

  for(i in 1:length(dataset)){
    print(paste0(i,"/",length(dataset)," bulk uploading..."))
    elastic::docs_bulk(esConnection,dataset[[i]],indexName)
  }
  print("Jobs done!")
}
