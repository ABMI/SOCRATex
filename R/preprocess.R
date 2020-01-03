#' preProcess
#'
#' This is a preprocessing function which can perform basic proprocessing rules.
#'
#' @param text        text corpus want to analyze.
#' @param english     if TRUE then alphabets except English will be deleted from the courpus.
#' @param whitespace  if TRUE then extra whitespaces will be deleted from the courpus.
#' @param stopwords   if TRUE then only English stopwords will be deleted. The stopwords are from tm package.
#' @param punc        if TRUE then punctuations will be deleted from the courpus.
#' @param number      if TRUE then numbers will be deleted from the corpus
#' @param stem        if TRUE then terms in the corpus will be stemmed. The stemming logic is from tm package.
#' @param lower       if TRUE then English characters will be lowered. If other languages except English are included the function will not work.
#'
#' @import tm
#'
#' @export

preProcess <- function(text, english = F, whitespace = F, stopwords = F, number = F, punc = F, stem = F, lower = F){

  if(english == T){Text_corpus <- gsub('[^[:ascii:]]', '', Text_corpus, perl = T)}
  Text_corpus <- tm::VCorpus(tm::VectorSource(Text_corpus))
  if(whitespace == T){Text_corpus <- tm::tm_map(Text_corpus, stripWhitespace)}
  if(stopwords == T){Text_corpus <- tm::tm_map(Text_corpus, removeWords, stopwords('en'))}
  if(number == T){Text_corpus <- tm::tm_map(Text_corpus, removeNumbers)}
  if(punc == T){Text_corpus <- tm::tm_map(Text_corpus, removePunctuation)}
  if(stem == T){Text_corpus <- tm::tm_map(Text_corpus, stemDocument)}
  if(lower == T){Text_corpus <- tm::tm_map(Text_corpus, tolower)}

  Text_corpus <- tm::tm_map(Text_corpus, PlainTextDocument)
  dtm <<- tm::DocumentTermMatrix(Text_corpus, control = list(wordLength=c(2,Inf), tokenizer = unigramTokenizer))

  return(dtm)
}
