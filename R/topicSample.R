#' topicSample
#'
#' This function shows the sample documents of the LDA clustering result. To implement the function LDAvis analysis need to be performed.
#' @param Text The text data want to analyze
#' @param topic The number of topics in LDA analysis
#' @param num The number of sample documents want to observe
#' @import dplyr
#' @export

topicSample <- function(Text, topic, num){
  for(i in 1:topic){
    assign(paste0("Sample", i), Text %>% mutate(TOPIC = paste0("TOPIC",i)) %>% select(TOPIC, NOTE_ID, NOTE_TEXT) %>% filter(NOTE_ID %in% names(head(sort(theta[,i], decreasing = T), num))))
    assign(paste0("Sample",1), bind_rows(get(paste0("Sample",1)), get(paste0("Sample",i))))
  }
  return(unique(Sample1))
}
