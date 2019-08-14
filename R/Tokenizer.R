#' UnigranTokenizer function
#'
#' This is a uni-gram tokenizer for creating Document-Term Matrix
#'
#' @import tm
#'
#' @export

UnigramTokenizer <- function(x){unlist(lapply(NLP::ngrams(NLP::words(x), 1), paste, collapse = " "), use.names = FALSE)}
