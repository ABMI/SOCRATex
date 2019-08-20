preprocess <- function(text, english = F, whitespace = F, stopwords = F, number = F, punc = F, stem = F, lower = F){
  
  if(english == T){Text_corpus <- gsub('[ㄱ-힣]', '', Text_corpus)} #영어 이외의 글자를 제거하는 코드 작성 필요    /^[a-zA-Z]+$/
  Text_corpus <- tm::VCorpus(tm::VectorSource(Text_corpus))
  if(whitespace == T){Text_corpus <- tm::tm_map(Text_corpus, stripWhitespace)}
  if(stopwords == T){Text_corpus <- tm::tm_map(Text_corpus, removeWords, stopwords('en'))}
  if(number == T){Text_corpus <- tm::tm_map(Text_corpus, removeNumbers)}
  if(punc == T){Text_corpus <- tm::tm_map(Text_corpus, removePunctuation)}
  if(stem == T){Text_corpus <- tm::tm_map(Text_corpus, stemDocument)}
  if(lower == T){Text_corpus <- tm::tm_map(Text_corpus, tolower)}
  
  Text_corpus <- tm::tm_map(Text_corpus, PlainTextDocument)
  DTM <- tm::DocumentTermMatrix(Text_corpus, control = list(wordLength=c(2,Inf), tokenizer = UnigramTokenizer))
  
  return(DTM)
}