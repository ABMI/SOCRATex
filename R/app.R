#devtools::install_github("timelyportfolio/listviewer")
#devtools::install_github("ropensci/plotly")
#devtools::install_github("dreamRs/shinyWidgets")

library(DatabaseConnector)
library(SqlRender)
library(tm)
library(SnowballC)
library(rJava)
library(wordcloud)
library(LDAvis)
library(topicmodels)
library(dplyr)
library(stringi)
library(servr)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(listviewer)
library(shiny)
library(shinyjs)

# N gram tokenizers
UnigramTokenizer <- function(x){unlist(lapply(NLP::ngrams(NLP::words(x), 1), paste, collapse = " "), use.names = FALSE)}

# pre-processing using dictionary
dictionary <- function(x, y){
      for(i in 1:nrow(x)){
        y <- gsub(x[i,1], x[i,2], y)
      }
      return(y)
}

# pre-processing function
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


# Rshiny Application
runApp(shinyApp(
  ui <- (navbarPage(theme = shinythemes::shinytheme("flatly")
                    , "SOCRATex"
                    , navbarMenu("Extraction"
                                 , tabPanel("DB Connection"
                                            , fluidRow(column(12
                                                              , align='center'
                                                              , useShinyjs()
                                                              , textInput('ip_address', 'IP address', '', placeholder = 'ex) ???.???.???.???')
                                                              , textInput('database_schema', 'Database schema', '', placeholder = 'ex) DBName')
                                                              , textInput('user', 'User ID', '', placeholder = 'ex) Admin')
                                                              , passwordInput('password', 'Password', '', placeholder = 'ex) 1234')
                                                              , textInput('resultdb', 'Result DB', '', placeholder = 'ex) WEBAPI_CDM.results')
                                                              , textInput('cohort', 'Cohort ID', '', placeholder = 'ex) 123')
                                                              , actionButton('connect', 'Connect'))
                                            )
                                 )
                                 , tabPanel("Preprocessing"
                                            , sidebarPanel(switchInput("type", "Type", value=F)
                                                           , uiOutput("typeOutput")
                                                           , sliderInput("num", "Number of reports", min = 10, max = 3000, value = 500, step = 1)
                                                           , sliderInput("date", "Dates", min = 1990, max = 2019, value = c(2012, 2018), step = 1)
                                                           , prettyCheckbox('english', "English only", T, icon=icon("check"), plain=T)
                                                           , prettyCheckbox('whitespace', "Remove whitespace", F, icon=icon("check"), plain=T)
                                                           , prettyCheckbox('stopwords', "Remove stopword", F, icon=icon("check"), plain=T)
                                                           , prettyCheckbox('number', "Remove number", F, icon=icon("check"), plain=T)
                                                           , prettyCheckbox('punc', "Remove punctuation", F, icon=icon("check"), plain=T)
                                                           , prettyCheckbox('stem', "Stemming", F, icon=icon("check"), plain=T)
                                                           , prettyCheckbox('lower', "To lower", F, icon=icon("check"), plain=T)
                                                           , textInput('dictionary_table', 'Dictionary table', '',  placeholder = 'ex) DBName.dbo.tableName')
                                                           , actionButton('process', 'Pre-Process')
                                                           , width="2"))
                                 )
                    
                    , navbarMenu("Exploration"
                                 , tabPanel("Characteristics"
                                            , fluidRow(column(6, align='center', tableOutput("count"))
                                                       , column(6, plotlyOutput("age"))
                                                       )
                                            , fluidRow(column(6,plotlyOutput("pie"))
                                                       , column(6, plotlyOutput("date"))
                                                       )
                                 )
                                 , tabPanel("Latent Dirichlet Allocation"
                                            , fluidPage(
                                              fluidRow(column(3, sliderInput('topicNum','Number of Topics',min=1,max=200,value = 20, step=1))
                                                       , column(3, sliderInput('learningNum','Repeated learning numbers',min=1,max=200,value = 20, step=1))
                                                       , column(3, sliderInput('alphaNum','alpha',min=0,max=1,value = 0.2, step=0.01)
                                                                , column(9, offset = 1, actionButton('visButton','GO')))
                                              )
                                              
                                              , fluidRow(column(12, visOutput('LDAModel')))
                                            )
                                 ))
                    
                  , navbarMenu("Annotation" 
                               , tabPanel("JSON schema"
                                           , fluidPage(sidebarPanel(width = 3
                                                                    , fileInput("uploadjson", "Upload"
                                                                                ,accept = c("text/rds","text/plain",".rds", ".json")) 
                                                                    , actionButton("update", 'Update JSON') #아직 server 와 연동되지 않음, 서버에서 작업 필요
                                                                    , downloadButton('save_JSON', 'Save JSON'))
                                                       , mainPanel(jsoneditOutput("jsed", height ="800px", width="1000px"))
                                          ))
                               , tabPanel("JSON structure"
                                          , fluidPage(sidebarPanel(width = 3
                                                                   , fileInput("uploadjson", "Upload"
                                                                               ,accept = c("text/rds","text/plain",".rds", ".json")) 
                                                                   , actionButton("update", 'Update JSON') #아직 server 와 연동되지 않음, 서버에서 작업 필요
                                                                   , downloadButton('save_JSON', 'Save JSON'))
                                                      , mainPanel(jsoneditOutput("jsed", height ="800px", width="1000px"))
                                          ))
                               , tabPanel("Annotation"
                                          , fluidPage(fluidRow(column(2, textInputAddon("num", label = NULL, placeholder = 1, addon = icon("info")),
                                                                      actionButton("click", "Click")
                                          )),
                                          fluidRow(column(6, verbatimTextOutput("note", placeholder = T)),
                                                   column(6, jsoneditOutput("jsed", height ="600px"#, width="700px"
                                                   )))
                                          , fluidRow(column(1,offset = 11, actionButton('button','SAVE')))
                                          , fluidRow(column(12, verbatimTextOutput("errorReport", placeholder = T)))
                                          ))
                              )
  ))
  
  , server <- (function(input, output){
                  # Database Connection
                  DBconnection <- reactive({
                                      connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms='sql server'
                                                                                                       , server=input$ip_address
                                                                                                       , schema=input$database_schema
                                                                                                       , user=input$user
                                                                                                       , password=input$password)
                                      connection <<- DatabaseConnector::connect(connectionDetails)
                                  })
                  
                  observeEvent(input$connect,{

                                      DBcon <<- DBconnection()
                                      
                                      if(DatabaseConnector::dbIsValid(connection)==TRUE){
                                                showModal(modalDialog(
                                                              title = "Messeage", "Database connection success!!", easyClose = T, footer=modalButton("cancel"), size = "l"
                                                ))
                                      }
                              })
                       
                  # picker    
                  output$typeOutput <- renderUI({
                        if(input$type == T){
                              sql <- "select distinct note_type_concept_id from NOTE" 
                              typeResult <- as.character(DatabaseConnector::querySql(connection=connection, sql)[,1]) 
                              pickerInput("note_type", label="note_type", choices = typeResult
                                          ,options = list('actions-box'=T, size=10, 'selected-text-format'="count>3")
                                          ,multiple=T)
                        }
                  })
                  
                  # preprocessing
                  observeEvent(input$process,{
                    sql <- "select top @num * from NOTE
                            where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                              and person_id in (select subject_id from @resultdb where cohort_definition_id=@cohort)
                              and note_type_concept_id in (@note_type)
                            order by newid()"
                    sql <- SqlRender::render(sql, num = input$num, min=input$date[1], max=input$date[2], resultdb=input$resultdb, cohort=input$cohort, note_type=input$note_type)
                    
                    Text <<- DatabaseConnector::querySql(connection, sql)
                    Dict <- DatabaseConnector::dbReadTable(connection, input$dictionary_table)
                    
                    Text_corpus <<- dictionary(Dict, Text$NOTE_TEXT)
                    
                    filedata <<- preprocess(text = Text_corpus, english = input$english, whitespace = input$whitespace, stopwords = input$stopwords
                                            , number = input$number, punc = input$punc, stem = input$stem, lower = input$lower)
                    
                    if(exists("filedata")==TRUE){
                      showModal(modalDialog(title="Message", "Preprocessing has completed!!", easyClose = T, footer = modalButton("cancel"), size = "l"))
                    }
                  })
                  
                  # table
                  output$count <- renderTable({
                                      sql1 <- "select count(distinct a.person_id) 
                                               from person a, note b 
                                               where a.person_id in (select subject_id from @resultdb where cohort_definition_id=@cohort)
                                                  and a.person_id=b.person_id
                                                  and and note_type_concept_id in (@note_type)
                                                  and where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)"
                                      sql2 <- "select count(*) from NOTE
                                               where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                                                and a.person_id in (select subject_id from @resultdb where cohort_definition_id=@cohort)
                                                and note_type_concept_id in (@note_type)"
                                      
                                      result1 <- data.frame(DatabaseConnector::querySql(connection = connection, sql1))
                                      result2 <- data.frame(DatabaseConnector::querySql(connection = connection, sql2))
                                      
                                      table <- cbind(description = c("Number of persons", "Number of notes"), count=rbind(result1, result2))
                                 })
                  
                  # pie chart
                  output$pie <- renderPlotly({
                                      sql <- "select distinct note_type_concept_id, count(note_id) cnt from NOTE 
                                              where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                                                        and a.person_id in (select subject_id from @resultdb where cohort_definition_id=@cohort)
                                                        and note_type_concept_id in (@note_type)
                                              group by note_type_concept_id order by cnt desc"
                                      result <- data.frame(DatabaseConnector::querySql(connection=connection, sql))
                                      
                                      plot_ly(result, labels = ~result$NOTE_TYPE_CONCEPT_ID, values=result$CNT, type="pie") %>% layout(title="NOTE type")
                                })
                  
                  # person
                  output$age <- renderPlotly({
                                      sql <- "select year_of_birth, gender_concept_id, count(*) as cnt
                                                  from PERSON a, NOTE b
                                                  where a.person_id=b.person_id
                                                    and (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                                                    and a.person_id in (select subject_id from @resultdb where cohort_definition_id=@cohort)
                                                    and note_type_concept_id in (@note_type)
                                                  group by gender_concept_id, year_of_birth
                                                  order by year_of_birth"
                                      
                                      result <- data.frame(DatabaseConnector::querySql(connection=connection, sql))
                                      result$GENDER_CONCEPT_ID <- factor(result$GENDER_CONCEPT_ID)
                                      
                                      plotly::ggplotly(ggplot(result, aes(x=YEAR_OF_BIRTH, y=CNT, color=GENDER_CONCEPT_ID))
                                                       + geom_line(size=1) + theme_minimal() + scale_color_manual(values = c("#3300FF", "#FF9933")))
                                })
                  
                  # date  
                  output$date <- renderPlotly({
                                      sql <- "select left(note_date,4) as date, note_type_concept_id, count(note_id) cnt 
                                              from NOTE 
                                              where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                                                and a.person_id in (select subject_id from @resultdb where cohort_definition_id=@cohort)
                                                and note_type_concept_id in (@note_type)
                                              group by left(note_date,4), note_type_concept_id"
                                      
                                      result <- data.frame(DatabaseConnector::querySql(connection=connection, sql))
                                      result$NOTE_TYPE_CONCEPT_ID <- factor(result$NOTE_TYPE_CONCEPT_ID)
                                      
                                      plotly::ggplotly(ggplot(result, aes(x=DATE, y=CNT, fill=NOTE_TYPE_CONCEPT_ID)) + geom_bar(stat = "identity", position = "stack") + 
                                                         theme(axis.text.x = element_text(angle=45)) + scale_fill_brewer(palette = "Set1"))
                                })
                 
                  # downloas button
                  # output$process <- downloadHandler(
                  #                       filename = function(){
                  #                                     paste0('process', ".rds")
                  #                       }
                  #                       , content = function(file){
                  #                                     saveRDS(processSetting(),file)
                  #                 })
                  
                  
                  # LDAvis
                  VisSetting <- eventReactive(input$visButton,{
                                              fit <- topicmodels::LDA(fileDTM, k=input$topicNum, method='Gibbs', control=list(iter=input$learningNum, alpha=input$alphaNum))
                                              phi <- posterior(fit)$terms %>% as.matrix
                                              theta <- posterior(fit)$topics %>% as.matrix
                                              vocab <- colnames(phi)
                                              doc_length <- c()
                                            
                                              for(i in 1:length(Text_corpus)) {
                                                              temp <- paste(Text_corpus, collapse=" ")
                                                              doc_length <- c(doc_length, stri_count(temp, regex='\\S+'))
                                              }
                                              
                                              temp_frequency <- as.matrix(fileDTM)
                                              freq_matrix <- data.frame(ST=colnames(temp_frequency),
                                                                        Freq=colSums(temp_frequency))
                                              rm(temp_frequency)
                                              
                                              json_lda <- createJSON(phi=phi,
                                                                     theta=theta,
                                                                     vocab=vocab,
                                                                     doc.length=doc_length,
                                                                     term.frequency=freq_matrix$Freq)
                                              json_lda
                              })
                  
                  output$LDAModel <- renderVis({VisSetting()})
                
                  # JSON Editor
                  output$jsed <- renderJsonedit({
                                        #if(exists(input$uploadjson)==T){jsonList <<- input$uploadjson} else{jsonList <- '{"a":{}}'}
                                        jsonedit(jsonList <- input$uploadjson
                                                 ,"onChange" = htmlwidgets::JS("() => {var txt = HTMLWidgets.findAll('.jsonedit').filter(function(item){return item.editor.container.id === 'jsed'})[0].editor.getText()
                                                                                      console.log(txt)
                                                                                      Shiny.onInputChange('saveJson',txt)}")
                                        )})
                  
                  # output$save_JSON <- downloadHandler(
                  #                           filename = function(){paste0('save_JSON', ".json")}
                  #                           , content = function(file){write(input$saveJson, file)}
                  #                    )
        })
))