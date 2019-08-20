jsonToES <- function(esConnection, indexName, jsonFolder, dropIfExist = F){
  json_list<- list.files(json_path,pattern = "*.json$",full.names = T)
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