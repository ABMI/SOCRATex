######################################################
#install.packages("xlsx")
library(xlsx)
library(tm)
library(topicmodels)
library(LDAvis)
library(dplyr)
library(stringi)
library(tm)
library(SnowballC)
library(rJava)
library(shiny)
library(DataCombine)
#install.packages("DataCombine")

UnigramTokenizer <- function(x){unlist(lapply(NLP::ngrams(NLP::words(x), 1), paste, collapse = " "), use.names = FALSE)}
# BigramTokenizer <- function(x){unlist(lapply(NLP::ngrams(NLP::words(x), 1:2), paste, collapse = " "), use.names = FALSE)}
# TrigramTokenizer <- function(x){unlist(lapply(NLP::ngrams(NLP::words(x), 1:3), paste, collapse = " "), use.names = FALSE)}
# nGramFunction <- c(UnigramTokenizer,BigramTokenizer,TrigramTokenizer)

######################################################
# 
# CRC <- read.xlsx("CRC total.xlsx", sheetName = "test")
# #CRC <- read.xlsx("CRC In.xlsx", sheetName = "Sheet1")
# 
# CRC$note_text <- gsub('[ㄱ-힣]', '', CRC$note_text)
# note <- gsub('Result', '', CRC$note_text, ignore.case = T)
# note <- gsub('Specimen', '', note, ignore.case = T)
# note <- gsub('Report', '', note, ignore.case = T)
# note <- gsub('Method', '', note, ignore.case = T)
# note <- gsub('Formalin', '', note, ignore.case = T)
# note <- gsub('Clinical', '', note, ignore.case = T)
# note <- gsub('disnosis', '', note, ignore.case = T)
# note <- gsub('Block', '', note, ignore.case = T)
# note <- gsub('Summary', '', note, ignore.case = T)
# note <- gsub('the', '', note, ignore.case = T)
# note <- gsub('this', '', note, ignore.case = T)
# note <- gsub('&lt;', ' ', note, ignore.case = T)
# note <- gsub('&gt;', ' ', note, ignore.case = T)
# note <- gsub('H&amp;E',' ', note, ignore.case = T)
# note <- gsub('T&amp;C',' ', note, ignore.case = T)
# note <- removeWords(note, stopwords("en"))
# 
# note <- VCorpus(VectorSource(note))
# note <- tm_map(note, tolower)
# note <- tm_map(note, stripWhitespace)
# note <- tm_map(note, removePunctuation)
# note <- tm_map(note, removeNumbers)
# #note <- tm_map(note, stemDocument)
# 
# note <- tm_map(note, PlainTextDocument)
# 
# DTM1 <- DocumentTermMatrix(note, control=list(wordLength=c(2,Inf), tokenizer=UnigramTokenizer))
# # DTM2 <- DocumentTermMatrix(note, control=list(wordLength=c(2,Inf), tokenizer=BigramTokenizer))
# # DTM3 <- DocumentTermMatrix(note, control=list(wordLength=c(2,Inf), tokenizer=TrigramTokenizer))
# 
# dtm <- removeSparseTerms(DTM1, 0.9)
# 
# rownames(dtm) <- CRC$note_id

######################################################

runApp(shinyApp(
  ui <- (navbarPage(theme = shinythemes::shinytheme("flatly")
                    , "NLP"
                    , tabsetPanel(tabPanel("LDA Visualization"
                                           , fluidPage(
                                             fluidRow(column(3, sliderInput('topicNum','Number of Topics',min=1,max=200,value = 20, step=1))
                                                      , column(3, sliderInput('learningNum','Repeated learning numbers',min=1,max=200,value = 20, step=1))
                                                      , column(3, sliderInput('alphaNum','alpha',min=0,max=1,value = 0.2, step=0.01)
                                                               , column(9, offset = 1, actionButton('visButton','GO')))
                                                      , column(3, sliderInput('sample','Number of Samples',min=1,max=20,value = 5, step=1))
                                             )
                                             
                                             , fluidRow(column(12, visOutput('LDAModel')))
                                           )
                                          ), tabPanel("LDA Document"
                                                      , fluidPage(fluidRow(
                                                              DT::dataTableOutput("SampleTopic")
                                                        ))
                                          ))
                    
  ))
  , server <- (function(input, output){
    
    # LDAvis
    VisSetting <- eventReactive(input$visButton,{
      fit <<- topicmodels::LDA(dtm, k=input$topicNum, method='Gibbs', control=list(iter=input$learningNum, alpha=input$alphaNum))
      phi <<- posterior(LDA_5)$terms %>% as.matrix
      theta <<- posterior(LDA_5)$topics %>% as.matrix
      vocab <<- colnames(phi)
      doc_length <- c()

      for(i in 1:length(note)) {
        temp <<- paste(note, collapse=" ")
        doc_length <<- c(doc_length, stri_count(temp, regex='\\S+'))
      }

      temp_frequency <<- as.matrix(dtm)
      freq_matrix <<- data.frame(ST=colnames(temp_frequency),
                                Freq=colSums(temp_frequency))
      rm(temp_frequency)

      json_lda <- createJSON(phi=phi,
                             theta=theta,
                             vocab=vocab,
                             doc.length=doc_length,
                             term.frequency=freq_matrix$Freq)
      LDA_5
      
      
      
    })
    
    output$LDAModel <- renderVis({VisSetting()})
    
    # output$SampleTopic <- DT::renderDataTable({
    #                   for(i in 1:3){
    #                     assign(paste0("test", fromJSON(LDA_3)$topic.order[i]), CRC %>% mutate(Topic = paste0("Topic",fromJSON(LDA_3)$topic.order[i])) %>% select(Topic, note_id, note_text) %>% filter(note_id %in% names(head(sort(theta[,i], decreasing = T), 10))))
    #                     
    #                   } 
    #                   for(i in 1:3){
    #                     assign(paste0("test",1), bind_rows(get(paste0("test",1)), get(paste0("test",i))))
    #                   }
    #                   unique(test1)
    # })
  })
))


###################################

# for(i in 1:5){
#   assign(paste0("test", i), CRC %>% mutate(Topic = paste0("Topic",i)) %>% select(Topic, note_id, note_text) %>% filter(note_id %in% names(head(sort(theta[,i], decreasing = T), 10))))
#   #assign(paste0("test",1), bind_rows(get(paste0("test",1)), get(paste0("test",i))))
# }
# unique(test1)
# 
# str(theta)
# rm(test1, test2, test3, test4, test5)
# 
# fromJSON(json_lda)
# 
# CRC %>% mutate(Topic = paste0("Topic",3)) %>% select(Topic, note_id, note_text) %>% filter(note_id %in% names(head(sort(theta[,3], decreasing = T), 5)))
# head(theta)
# head(phi)
