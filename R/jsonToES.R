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

jsonToES <- function(esConnection, indexName, jsonFolder, dropIfExist = FALSE){
  json_list<- list.files(json_path,pattern = "*.json$",full.names = T)
  dataset <- sapply(json_list, read_json)

  if(elastic::index_exists(esConnection,indexName)){
    if(dropIfExist){
      elastic::index_delete(esConnection, indexName)
      elastic::index_create(conn = esConnection,
                            index = indexName,
                            body = NULL,
                            raw = F,
                            verbose = T)
    }
  }
  else{
    elastic::index_create(conn = esConnection,
                          index = indexName,
                          body = NULL,
                          raw = F,
                          verbose = T)
  }
  for(i in 1:length(dataset)){
    print(paste0(i,"/",length(dataset)," bulk uploading..."))
    elastic::docs_bulk(conn = esConnection,
                       x = dataset[i],
                       index = indexName,
                       config = c(httr::verbose()))
  }
  print("Jobs done!")
}
