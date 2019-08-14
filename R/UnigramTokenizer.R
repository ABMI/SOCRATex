#' UnigranTokenizer function
#'
#' This is a uni-gram tokenizer for creating Document-Term Matrix
#'
#' @param x   text data which can be tokenized
#' @import tm
#'
#' @export

UnigramTokenizer <- function(x){unlist(lapply(NLP::ngrams(NLP::words(x), 1), paste, collapse = " "), use.names = FALSE)}
