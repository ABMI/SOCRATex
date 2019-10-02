shinyApp(
  ui <- (navbarPage(theme = shinythemes::shinytheme("flatly")
                    , "SOCRATex",id = 'id'
                    , navbarMenu("Data Extraction"
                                 , tabPanel("DB Connection"
                                            , fluidRow(column(12
                                                              , align='center'
                                                              , shinyjs::useShinyjs()
                                                              , textInput('ip_address', 'IP address', '', placeholder = 'ex) ???.???.???.???')
                                                              , textInput('database_schema', 'Database schema', '', placeholder = 'ex) DBName')
                                                              , textInput('user', 'User ID', '', placeholder = 'ex) Admin')
                                                              , passwordInput('password', 'Password', '', placeholder = 'ex) 1234')
                                                              , textInput('resultdb', 'Result DB', '', placeholder = 'ex) WEBAPI_CDM.results')
                                                              , textInput('cohort', 'Cohort ID', '', placeholder = 'ex) 123')
                                                              , actionButton('connect', 'Connect'))
                                            )
                                 )
                                 , tabPanel("File Upload"
                                            , sidebarPanel(
                                              fileInput("file1", "Choose CSV File",
                                                        multiple = FALSE,
                                                        accept = c("text/csv",
                                                                   "text/comma-separated-values,text/plain",
                                                                   ".csv")),
                                              checkboxInput("header", "Header", TRUE),
                                              radioButtons("sep", "Separator",
                                                           choices = c(Comma = ",",
                                                                       Semicolon = ";",
                                                                       Tab = "\t"),
                                                           selected = ","),
                                              radioButtons("quote", "Quote",
                                                           choices = c(None = "",
                                                                       "Double Quote" = '"',
                                                                       "Single Quote" = "'"),
                                                           selected = '"'),
                                              radioButtons("disp", "Display",
                                                           choices = c(Head = "head",
                                                                       All = "all"),
                                                           selected = "head")
                                            ),
                                            mainPanel(
                                              tableOutput("contents")
                                            )
                                 )
                    )
                    , tabPanel("Preprocessing"
                               , sidebarPanel(shinyWidgets::switchInput("type", "Type", value=F)
                                              , uiOutput("typeOutput")
                                              , sliderInput("ReportNum", "Number of reports", min = 10, max = 3000, value = 500, step = 1)
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
                                              , width="3"))
                    , navbarMenu("Exploration"
                                 , tabPanel("Characteristics"
                                            , fluidRow(column(6, align='center', DT::dataTableOutput("count"))
                                                       , column(6, plotly::plotlyOutput("age"))
                                            )
                                            , fluidRow(column(6,plotly::plotlyOutput("pie"))
                                                       , column(6, plotly::plotlyOutput("date"))
                                            )
                                 )
                                 , tabPanel("LDAtuning"
                                            , fluidRow(
                                              sidebarPanel(
                                                sliderInput("TopicNum", "Number of reports", min = 1, max = 300, value = c(5,30), step = 1)
                                                , numericInput("By", "Toipcs, by:", min = 1, max = 30, step = 1, value = 1)
                                                , shinyWidgets::pickerInput("metrics", label="Evulation Methods", choices = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014")
                                                                            , options = list('actions-box'=T, size=10, 'selected-text-format'="count>3")
                                                                            , multiple=T)
                                                , actionButton("Calc", "Calculate!")
                                              ),
                                              mainPanel(plotOutput("TuningGraph"))
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
                                                                     , fluidRow(column(12, LDAvis::visOutput('LDAModel')))
                                                                   )
                                            ), tabPanel("LDA Sample"
                                                        , fluidPage(fluidRow(
                                                          DT::dataTableOutput("SampleTopic")
                                                        )))
                                            ))
                    )
                    , navbarMenu("Annotation"
                                 , tabPanel("JSON Schema"
                                            , fluidPage(sidebarPanel(width = 3
                                                                     , fileInput("UploadSchema", "Upload"
                                                                                 ,accept = c("text/rds","text/plain",".rds", ".json"))
                                                                     , actionButton("UpdateSchema", 'Update JSON')
                                                                     , downloadButton('DownloadSchema', 'Download Schema'))
                                                        , mainPanel(listviewer::jsoneditOutput("Schema", height="800px")
                                                                    , verbatimTextOutput('SchemaText')
                                                        )
                                            ))
                                 , tabPanel("JSON Structure"
                                            , fluidPage(sidebarPanel(width = 3
                                                                     , fileInput("UploadTemplate", "Upload"
                                                                                 ,accept = c("text/rds","text/plain",".rds", ".json"))
                                                                     , actionButton("UpdateTemplate", 'Update JSON')
                                                                     , downloadButton('DownloadTemplate', 'Download Template'))
                                                        , mainPanel(listviewer::jsoneditOutput("Template", height="800px")
                                                                    , verbatimTextOutput('TemplateText')
                                                        )
                                            ))
                                 , tabPanel("JSON Annotation"
                                            , fluidPage(fluidRow(column(2, textInputAddon("num", label = NULL, placeholder = 1, addon = icon("info")),
                                                                        actionButton("click", "Click"))
                                                                        , column(2, actionButton("save", "Save"))
                                            ),
                                            fluidRow(column(6, verbatimTextOutput("note", placeholder = T)),
                                                     column(6, listviewer::jsoneditOutput("annot", height ="800px"#, width="700px"
                                                     )))
                                            , fluidRow(column(1,offset = 11, actionButton('button','SAVE')))
                                            , fluidRow(column(12, verbatimTextOutput("errorReport", placeholder = T)))
                                            ))

                    )
                    , tabPanel("Elasticsearch"
                               , fluidRow(column(12
                                                 , align='center'
                                                 , textInput('host', 'Host', '', placeholder = 'If it is a localhost, leave it blank')
                                                 #, textInput('port', 'Port', '', placeholder = 'ex) If it is a Local Elasticsearch, leave it blank')
                                                 , textInput('indexName', 'Index Name', '', placeholder = 'ex) PathologyABMI')
                                                 , textInput('filepath', 'Folder Path', '', placeholder = 'Input folder path')
                                                 , actionButton('send', 'Send'))
                               )
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

      if(exists("connection")==TRUE){
        showModal(modalDialog(
          title = "Messeage", "Database connection success!!", easyClose = T, footer=modalButton("cancel"), size = "l"
        ))
      }
    })

    # picker
    output$typeOutput <- renderUI({
      if(input$type == T){
        if(exists("connection")==TRUE){
          sql <- "select distinct note_type_concept_id from NOTE"
          typeResult <- as.character(DatabaseConnector::querySql(connection=connection, sql)[,1])
          shinyWidgets::pickerInput("note_type", label="note_type", choices = typeResult
                                    ,options = list('actions-box'=T, size=10, 'selected-text-format'="count>3")
                                    ,multiple=T)
        } else{
          typeResult <- as.character(distinct(Text, NOTE_TYPE_CONCEPT_ID)[,1])
          shinyWidgets::pickerInput("note_type", label="note_type", choices = typeResult
                                    ,options = list('actions-box'=T, size=10, 'selected-text-format'="count>3")
                                    ,multiple=T)
        }
      }
    })

    # Data extraction using file upload
    output$contents <- renderTable({
      req(input$file1)

      tryCatch({
        Text <<- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote,
                          stringsAsFactors = F)
        colnames(Text) <<- toupper(colnames(Text))
      },error = function(e) {stop(safeError(e))}
      )

      if(input$disp == "head") {
        return(head(Text))
      }
      else {
        return(Text)
      }
    })


    # preprocessing
    observeEvent(input$process,{
      if(exists("connection")==TRUE){
        if(exists("input$resultdb")==T){
          sql <- "select top @num a.*, b.YEAR_OF_BIRTH, b.GENDER_CONCEPT_ID from NOTE a, PERSON b
                            where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                              and a.person_id in (select subject_id from @resultdb where cohort_definition_id=@cohort)
                              and note_type_concept_id in (@note_type)
                              and a.person_id=b.person_id
                            order by newid()"
          sql <- SqlRender::render(sql, num = input$ReportNum, min=input$date[1], max=input$date[2], resultdb=input$resultdb, cohort=input$cohort, note_type=input$note_type)

          Text <<- DatabaseConnector::querySql(connection, sql)
          colnames(Text) <<- toupper(colnames(Text))
          JSON <<- c()

          if(exists("input$dictionary_table")==T){
            Dict <- DatabaseConnector::dbReadTable(connection, input$dictionary_table)
            Text_corpus <<- dictionary(Dict, Text$NOTE_TEXT)
          } else{Text_corpus <<- Text$NOTE_TEXT}

          dtm <<- preprocess(text = Text_corpus, english = input$english, whitespace = input$whitespace, stopwords = input$stopwords
                             , number = input$number, punc = input$punc, stem = input$stem, lower = input$lower)
          rownames(dtm) <<- Text$NOTE_ID
        } else{
          sql <- "select top @num a.*, b.YEAR_OF_BIRTH, b.GENDER_CONCEPT_ID from NOTE a, PERSON b
                            where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                              and note_type_concept_id in (@note_type)
                              and a.person_id=b.person_id
                            order by newid()"
          sql <- SqlRender::render(sql, num = input$ReportNum, min=input$date[1], max=input$date[2], resultdb=input$resultdb, cohort=input$cohort, note_type=input$note_type)

          Text <<- DatabaseConnector::querySql(connection, sql)
          JSON <<- c()

          if(exists("input$dictionary_table")==T){
            Dict <- DatabaseConnector::dbReadTable(connection, input$dictionary_table)
            Text_corpus <<- dictionary(Dict, Text$NOTE_TEXT)
          } else{Text_corpus <<- Text$NOTE_TEXT}

          dtm <<- preprocess(text = Text_corpus, english = input$english, whitespace = input$whitespace, stopwords = input$stopwords
                             , number = input$number, punc = input$punc, stem = input$stem, lower = input$lower)
          rownames(dtm) <<- Text$NOTE_ID
        }} else{
          JSON <<- c()
          Text <- Text %>% filter(NOTE_TYPE_CONCEPT_ID %in% input$note_type)
          Text <- Text %>% filter(substring(NOTE_DATE, 1, 4) >= input$date[1] & substring(NOTE_DATE, 1, 4) <= input$date[2])
          Text <<- head(Text, input$ReportNum)
          Text_corpus <<- Text$NOTE_TEXT ## Why it's not working...?

          dtm <<- preprocess(text = Text_corpus, english = input$english, whitespace = input$whitespace, stopwords = input$stopwords
                             , number = input$number, punc = input$punc, stem = input$stem, lower = input$lower)
          rownames(dtm) <<- Text$NOTE_ID
        }

      if(exists("dtm")==TRUE){
        showModal(modalDialog(title="Message", "Preprocessing has completed!!", easyClose = T, footer = modalButton("cancel"), size = "l"))
      }
    })

    # personTable
    output$count <- DT::renderDataTable({
      person <- Text %>% select(PERSON_ID) %>% distinct() %>% count()
      note <- Text %>% select(NOTE_ID) %>% count()

      personTable <- as.data.frame(rbind(person, note))

      rownames(personTable) <- c("Person", "Note")
      colnames(personTable) <- c("Count")

      personTable
    })

    # pie chart
    output$pie <- plotly::renderPlotly({
      pie <- Text %>% group_by(NOTE_TYPE_CONCEPT_ID) %>% select(NOTE_ID) %>% count(NOTE_TYPE_CONCEPT_ID)
      pie <- as.data.frame(pie)
      colnames(pie) <- c("NoteType", "Count")

      plotly::plot_ly(pie, labels = ~pie$NoteType, values=pie$Count, type="pie") %>% layout(title="The Proportions of Note Types of Data")
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

    # LDA tuning
    LDATuning <- eventReactive(input$Calc, {
      TopicNum <<- input$TopicNum
      By <<- input$By
      metrics <<- input$metrics

      tuning <<- ldatuning::FindTopicsNumber(dtm, topics = seq(from = input$TopicNum[1], to = input$TopicNum[2], by = input$By), metrics = input$metrics, method = "Gibbs")
      ldatuning::FindTopicsNumber_plot(tuning)
    })

    output$TuningGraph <- renderPlot({LDATuning()})

    # LDAvis
    VisSetting <- eventReactive(input$visButton,{
      fit <- topicmodels::LDA(dtm, k=input$topicNum, method='Gibbs', control=list(iter=input$learningNum, alpha=input$alphaNum))
      phi <- modeltools::posterior(fit)$terms %>% as.matrix
      theta <<- modeltools::posterior(fit)$topics %>% as.matrix
      vocab <- colnames(phi)
      doc_length <- c()

      for(i in 1:length(Text_corpus)) {
        temp <- paste(Text_corpus, collapse=" ")
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
    output$Schema <- listviewer::renderJsonedit({
      listviewer::jsonedit(jsonSchemaList <<- jsonlite::fromJSON(
        if(is.null(input$UploadSchema$datapath)){
          jsonSchemaList <- jsonlite::toJSON('{"sample":"test1"}')
        }

        else{
          jsonSchemaList <- input$UploadSchema$datapath
        }
      )
      ,onChange=htmlwidgets::JS("
                                function(){
                                  Shiny.onInputChange(
                                    'Schema_change',
                                    HTMLWidgets.getInstance(document.getElementById('Schema')).editor.getText()
                                  );
                                }"
      )
      )

    })

    observe({
      input$UploadSchema
      validationJson <<- jsonSchemaList

    })

    observe({
      input$UpdateSchema
      validationJson <<- isolate({
        if(!is.null(input$Schema_change)){
          if(input$Schema_change != jsonSchemaList){
            input$Schema_change

          }
        }
        else{
          jsonSchemaList
        }
      })
    })

    output$DownloadSchema <- downloadHandler(
      filename = function(){paste0('DownloadSchema', ".json")}
      , content = function(file){write(jsonlite::toJSON(validationJson), file)}
    )


    #JSON Structure
    output$Template <- listviewer::renderJsonedit({
      listviewer::jsonedit(jsonList <<- jsonlite::fromJSON(
        if(is.null(input$UploadTemplate$datapath)){
          jsonList <- jsonlite::toJSON('{"sample":"test2"}')
        }
        else{
          jsonList <- input$UploadTemplate$datapath
        }
      )
      ,onChange=htmlwidgets::JS("
                                function(){
                                  Shiny.onInputChange(
                                    'Template_change',
                                    HTMLWidgets.getInstance(document.getElementById('Template')).editor.getText()
                                  );
                                }"
      )

      )
    })

    observe({
      input$UploadTemplate
      validationJson2 <<- jsonList
    })

    observe({
      input$UpdateTemplate
      validationJson2 <<- isolate({
        if(!is.null(input$Template_change)){
          if(input$Template_change != jsonList){
            input$Template_change
          }
          else{
            jsonList
          }

        }
        else{
          jsonList
        }
      })
    })


    output$DownloadTemplate <- downloadHandler(
      filename = function(){paste0('DownloadTemplate', ".json")}
      , content = function(file){write(jsonlite::toJSON(validationJson2), file)}
    )

    # JSON Annotation
    output$annot <- listviewer::renderJsonedit({
      input$id
      listviewer::jsonedit(JSONannotation <<- jsonlite::fromJSON(
        if(is.null(validationJson2)){
          JSONannotation <- jsonlite::toJSON('{"sample":"test3"}')
        }
        else{
          JSONannotation <- jsonlite::toJSON(validationJson2)
        }
      )
      ,onChange=htmlwidgets::JS("
                              function(){
                                Shiny.onInputChange(
                                  'annot_change',
                                  HTMLWidgets.getInstance(document.getElementById('annot')).editor.getText()
                                );
                              }"
      )
      )
    })

    observeEvent(input$save, {
      dir.create(path = paste0(getwd(), "/JSON"))
      setwd(paste0(getwd(), "/JSON"))

      for(i in 1:length(JSON)){
        JSON[i] <- tm::stripWhitespace(JSON[i])
        JSON[i] <- jsonlite::toJSON(JSON[i])
        JSON[i] <- gsub('["{', '[{', JSON[i], fixed = T)
        JSON[i] <- gsub('}"]', '}]', JSON[i], fixed = T)
        JSON[i] <- gsub('} "]', '}]', JSON[i], fixed = T)
        JSON[i] <- gsub('\\"', '"', JSON[i], fixed = T)
        write(JSON[i], paste0("json",i,".json"))
      }
    })

    # SourceText
    SourceText <- eventReactive(input$click,{
      num <<- input$num
      Text$NOTE_TEXT[as.numeric(input$num)]
    })

    output$note <- renderText({SourceText()})

    # Validation using JSON Schema
    errorReportSetting <- eventReactive(input$button,{
      if(is.null('validationJson')){
        errorInfo <- stop('Json Schema Upload first!!')
      }
      else if(is.null(input$annot_change)){
        v <<- jsonvalidate::json_validator(validationJson)
        errorInfo <<- v(JSONannotation, verbose=TRUE, greedy=TRUE)

        if(errorInfo[1] ==TRUE)
          errorInfo[1] = 'Validate!'
        else{
          df <- attr(errorInfo,'error')
          errorInfo <- paste(errorInfo[1],paste(df[,1],df[,2],collapse ='\n'),collapse ='\n')
        }
      }
      else{
        JSON[as.numeric(input$num)] <<- input$annot_change
        v <<- jsonvalidate::json_validator(validationJson)
        errorInfo <<- v(input$annot_change, verbose=TRUE, greedy=TRUE)
        if(errorInfo[1] == TRUE){
          errorInfo[1] = 'Validate!'
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

    # Elasticsearch
    observeEvent(input$Send, {
      if(exists(input$host|input$port)==T){
        esConnection <- elastic::connect(host = input$host, errors='complete') # port = input$port
      } else{
        esConnection <- elastic::connect(errors='complete')
      }
      jsonToES(connection, jsonFolder = input$filepath, dropIfExist = T)
    })
  })
)
