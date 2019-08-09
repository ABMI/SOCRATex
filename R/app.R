
library(tm)
library(rJava)
library(dplyr)
library(stringi)
library(servr)
library(ggplot2)
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
  ui <- (navbarPage(theme = shinythemes::shinytheme("sandstone")
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
                                            , fluidRow(column(6, align='center', DT::dataTableOutput("count"))
                                                       , column(6, plotly::plotlyOutput("age"))
                                            )
                                            , fluidRow(column(6,plotly::plotlyOutput("pie"))
                                                       , column(6, plotly::plotlyOutput("date"))
                                            )
                                 )
                                 , tabPanel("Latent Dirichlet Allocation"
                                            , tabsetPanel(tabPanel("LDA Visualization"
                                                                   , fluidPage(
                                                                     fluidRow(column(3, sliderInput('topicNum','Number of Topics',min=1,max=200,value = 20, step=1))
                                                                              , column(3, sliderInput('learningNum','Repeated learning numbers',min=1,max=200,value = 20, step=1))
                                                                              , column(3, sliderInput('alphaNum','alpha',min=0,max=1,value = 0.2, step=0.01))
                                                                              , column(3, sliderInput('sample','Number of Samples',min=1,max=20,value = 5, step=1))
                                                                              , column(11, align='right', offset = 1, actionButton('visButton','GO')))
                                                                     , fluidRow(column(12, LDAvis::visOutput('LDAModel'))
                                                                     )
                                                                   )
                                            ), tabPanel("LDA Sample"
                                                        , fluidPage(fluidRow(
                                                          DT::dataTableOutput("SampleTopic")
                                                        )))
                                            ))
                    )
                    
                    , navbarMenu("Annotation"
                                 , tabPanel("JSON schema"
                                            , fluidPage(sidebarPanel(width = 3
                                                                     , fileInput("UploadSchema", "Upload"
                                                                                 ,accept = c("text/rds","text/plain",".rds", ".json"))
                                                                     , actionButton("UpdateSchema", 'Update JSON') 
                                                                     , downloadButton('DownloadSchema', 'Download Schema'))
                                                        , mainPanel(listviewer::jsoneditOutput("Schema"))
                                            ))
                                 
                                 , tabPanel("JSON Annotation"
                                            , fluidPage(fluidRow(column(2, textInputAddon("num", label = NULL, placeholder = 1, addon = icon("info")),
                                                                        actionButton("click", "Click")
                                            )),
                                            fluidRow(column(6, verbatimTextOutput("note", placeholder = T)),
                                                     column(6, listviewer::jsoneditOutput("annot", height ="800px"#, width="700px"
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
        shinyWidgets::pickerInput("note_type", label="note_type", choices = typeResult
                    ,options = list('actions-box'=T, size=10, 'selected-text-format'="count>3")
                    ,multiple=T)
      }
    })
    
    # preprocessing
    observeEvent(input$process,{
      
      if(exists("input$resultdb")==T){
        sql <- "select top @num a.*, b.YEAR_OF_BIRTH, b.GENDER_CONCEPT_ID from NOTE a, PERSON b
                            where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                              and person_id in (select subject_id from @resultdb where cohort_definition_id=@cohort)
                              and note_type_concept_id in (@note_type)
                              and a.person_id=b.person_id
                            order by newid()"
        sql <- SqlRender::render(sql, num = input$num, min=input$date[1], max=input$date[2], resultdb=input$resultdb, cohort=input$cohort, note_type=input$note_type)  
        
        Text <<- DatabaseConnector::querySql(connection, sql)
        
        if(exists("input$dictionary_table")==T){
          Dict <- DatabaseConnector::dbReadTable(connection, input$dictionary_table)  
          Text_corpus <<- dictionary(Dict, Text$NOTE_TEXT)
        } else{Text_corpus <<- Text$NOTE_TEXT}
        
        filedata <<- preprocess(text = Text_corpus, english = input$english, whitespace = input$whitespace, stopwords = input$stopwords
                                , number = input$number, punc = input$punc, stem = input$stem, lower = input$lower)
        rownames(filedata) <<- Text$NOTE_ID
      } else{
        sql <- "select top @num a.*, b.YEAR_OF_BIRTH, b.GENDER_CONCEPT_ID from NOTE a, PERSON b
                            where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                              and note_type_concept_id in (@note_type)
                              and a.person_id=b.person_id
                            order by newid()"
        sql <- SqlRender::render(sql, num = input$num, min=input$date[1], max=input$date[2], resultdb=input$resultdb, cohort=input$cohort, note_type=input$note_type)  
        
        Text <<- DatabaseConnector::querySql(connection, sql)
        
        if(exists("input$dictionary_table")==T){
          Dict <- DatabaseConnector::dbReadTable(connection, input$dictionary_table)  
          Text_corpus <<- dictionary(Dict, Text$NOTE_TEXT)
        } else{Text_corpus <<- Text$NOTE_TEXT}
        
        filedata <<- preprocess(text = Text_corpus, english = input$english, whitespace = input$whitespace, stopwords = input$stopwords
                                , number = input$number, punc = input$punc, stem = input$stem, lower = input$lower)
        rownames(filedata) <<- Text$NOTE_ID
      }
      
      if(exists("filedata")==TRUE){
        showModal(modalDialog(title="Message", "Preprocessing has completed!!", easyClose = T, footer = modalButton("cancel"), size = "l"))
      }
    })
    
    # table
    output$count <- renderDataTable({
      person <- Text %>% select(PERSON_ID) %>% distinct() %>% count()
      note <- Text %>% select(NOTE_ID) %>% count()
      
      table <- as.data.frame(rbind(person, note))
      
      rownames(table) <- c("Person", "Note")  
      colnames(table) <- c("Count")
      
      table
    })
    
    # pie chart
    output$pie <- plotly::renderPlotly({
      pie <- Text %>% group_by(NOTE_TYPE_CONCEPT_ID) %>% select(NOTE_ID) %>% count(NOTE_TYPE_CONCEPT_ID)
      pie <- as.data.frame(pie)
      colnames(pie) <- c("NoteType", "Count")
      
      plot_ly(pie, labels = ~pie$NoteType, values=pie$Count, type="pie") %>% layout(title="The Proportions of Note Types of Data")
    })
    
    # Age
    output$age <- plotly::renderPlotly({
      age <- Text %>% group_by(YEAR_OF_BIRTH, GENDER_CONCEPT_ID) %>% select(YEAR_OF_BIRTH, GENDER_CONCEPT_ID) %>% count(YEAR_OF_BIRTH, GENDER_CONCEPT_ID)
      age <-as.data.frame(age)
      colnames(age) <- c("BirthYear", "Gender", "Count")
      age$Gender <- factor(age$Gender)
      
      plotly::ggplotly(ggplot(age, aes(x=BirthYear, y=Count, color=Gender))
                       + geom_line(size=1) + theme_minimal() + scale_color_manual(values = c("#3300FF", "#FF9933")))  
    })
    
    # date  
    output$date <- plotly::renderPlotly({
      Text$NOTE_DATE <- Text$NOTE_DATE %>% substring(1, 4)
      date <- Text %>% group_by(NOTE_DATE, NOTE_TYPE_CONCEPT_ID) %>% select(NOTE_DATE, NOTE_TYPE_CONCEPT_ID) %>% count(NOTE_DATE, NOTE_TYPE_CONCEPT_ID)
      date <- as.data.frame(date)
      date$NOTE_TYPE_CONCEPT_ID <- factor(date$NOTE_TYPE_CONCEPT_ID)
      colnames(date) <- c("NoteDate", "NoteType", "Count")
      
      plotly::ggplotly(ggplot(date, aes(x=NoteDate, y=Count, fill=NoteType)) + geom_bar(stat = "identity", position = "stack") + 
                         theme(axis.text.x = element_text(angle=45)) + scale_fill_brewer(palette = "Set1"))
    })
    
    # LDAvis
    VisSetting <- eventReactive(input$visButton,{
      fit <- topicmodels::LDA(filedata, k=input$topicNum, method='Gibbs', control=list(iter=input$learningNum, alpha=input$alphaNum))
      phi <- posterior(fit)$terms %>% as.matrix
      theta <<- posterior(fit)$topics %>% as.matrix
      vocab <- colnames(phi)
      doc_length <- c()
      
      for(i in 1:length(Text_corpus)) {
        temp <- paste(Text_corpus, collapse=" ")
        doc_length <- c(doc_length, stri_count(temp, regex='\\S+'))
      }
      
      temp_frequency <- as.matrix(filedata)
      freq_matrix <- data.frame(ST=colnames(temp_frequency),
                                Freq=colSums(temp_frequency))
      rm(temp_frequency)
      
      json_lda <- LDAvis::createJSON(phi=phi,
                                     theta=theta,
                                     vocab=vocab,
                                     doc.length=doc_length,
                                     term.frequency=freq_matrix$Freq)
      json_lda
    })
    
    output$LDAModel <- LDAvis::renderVis({VisSetting()})
    
    output$SampleTopic <- DT::renderDataTable({
      for(i in 1:input$topicNum){
        assign(paste0("Sample", i), Text %>% mutate(TOPIC = paste0("TOPIC",i)) %>% select(TOPIC, NOTE_ID, NOTE_TEXT) %>% filter(NOTE_ID %in% names(head(sort(theta[,i], decreasing = T), input$sample))))
        assign(paste0("Sample",1), bind_rows(get(paste0("Sample",1)), get(paste0("Sample",i))))
      } 
      return(unique(Sample1))
    })
    
    #JSON Schema
    ## jsonList <- "\"Sample\":\"JSON\"\"
    ## jsonList <- jsonlite::fromJSON((input$UploadSchema)$datapath)
    output$Schema <- listviewer::renderJsonedit({
      listviewer::jsonedit(jsonList <- jsonlite::fromJSON((input$UploadSchema)$datapath)
               ,"onChange" = htmlwidgets::JS("() => {var txt = HTMLWidgets.findAll('.jsonedit').filter(function(item){return item.editor.container.id === 'Schema'})[0].editor.getText()
                                                             console.log(txt)
                                                             Shiny.onInputChange('jsedOutput',txt)}")
      )
    })
    
    eventReactive(input$UpdateSchema,{
      test <<- jsonlite::fromJSON(input$jsedOutput)
    })
    
    output$DownloadSchema <- downloadHandler(
      filename = function(){paste0('DownloadSchema', ".json")}
      , content = function(file){write(jsonlite::fromJSON(input$Save_Schema), 'JSONSchema.json')}
    )
    
    # Text output
    output$note <- renderText({noteText[as.numeric(input$num)]})
    
    # JSON Annotation
    output$annot <- listviewer::renderJsonedit({
      listviewer::jsonedit(jsonList <<- Json[as.numeric(input$num)]
               ,"onChange" = htmlwidgets::JS("() => {var txt = HTMLWidgets.findAll('.jsonedit').filter(function(item){return item.editor.container.id === 'jsed'})[0].editor.getText()
                                                             console.log(txt)
                                                             Shiny.onInputChange('saveJson',txt)}")
      )
      
    })
    
    errorReportSetting <- eventReactive(input$button,{
      if(is.null(input$saveJson)){
        v <- jsonvalidate::json_validator(schema)
        errorInfo <- v(jsonList, verbose=TRUE, greedy=TRUE)
        # if(errorInfo[1] == TRUE){
        #   #save
        #   jsonFile <- jsonFile[as.numeric(input$num),'json']
        #   write.xlsx(jsonFile,"./Clustering/json_sample100.xlsx", sheetName = "Sheet1", stringsAsFactors=F)
        # }
        
        if(errorInfo[1] ==TRUE)
          errorInfo[1] = 'Validate!'
        else{
          df <- attr(errorInfo,'error')
          errorInfo <- paste(errorInfo[1],paste(df[,1],df[,2],collapse ='\n'),collapse ='\n')
        }
      }
      else{
        jsonList[] <- input$saveJson
        v <- jsonvalidate::json_validator(schema)
        errorInfo <- v(input$saveJson, verbose=TRUE, greedy=TRUE)
        if(errorInfo[1] == TRUE){
          #save
          jsonFile <- jsonFile[as.numeric(input$num),'json']
          write.xlsx(jsonFile,"./Clustering/json_sample100.xlsx", sheetName = "Sheet1", stringsAsFactors=F)
        }
        else{
          df <- attr(errorInfo,'error')
          errorInfo <- paste(errorInfo[1],paste(df[,1],df[,2],collapse ='\n'),collapse ='\n')
        }
        
      }
      
      errorInfo
      
    })
    
    output$errorReport <- renderText({
      as.character(errorReportSetting())
    })    
  })
))