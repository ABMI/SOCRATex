#' shinyLDA
#'
#' This function is implementing LDAvis into RShint form especially for SOCRATex application.
#'
#' @param dtm DocumentTerm Matrix which can be used for LDA analysis.
#' @param k The number of topics(clusters) want to analyze.
#' @param iter The number of Learning iterations.
#' @param alpha Tha alpha parameter of the Latent Dirichlet Allocation.
#'
#' @import topicmodels
#' @import modeltools
#' @import dplyr
#' @import LDAvis
#' @import stringi
#' @export

shinyLDA <- function(dtm, k, iter, alpha){
  fit <- topicmodels::LDA(dtm, k=k, method='Gibbs', control=list(iter=iter, alpha=alpha))
  phi <- modeltools::posterior(fit)$terms %>% as.matrix
  theta <<- modeltools::posterior(fit)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- c()
  temp <- paste(Text_corpus, collapse=" ")

  for(i in 1:length(Text_corpus)) {
    doc_length <- c(doc_length, stringi::stri_count(temp, regex='\\S+'))
  }

  temp_frequency <- as.matrix(dtm)
  freq_matrix <- data.frame(ST=colnames(temp_frequency),
                            Freq=colSums(temp_frequency))
  rm(temp_frequency)

  json_lda <- LDAvis::createJSON(phi=phi,
                                 theta=theta,
                                 vocab=vocab,
                                 doc.length=doc_length,
                                 term.frequency=freq_matrix$Freq)
  return(json_lda)
}
