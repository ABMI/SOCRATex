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
                                            fluidRow(column(6, verbatimTextOutput("noteid", placeholder = T))),
                                            fluidRow(column(6, verbatimTextOutput("note", placeholder = T)),
                                                     column(6, listviewer::jsoneditOutput("annot", height ="800px"#, width="700px"
                                                     )))
                                            , fluidRow(column(1,offset = 11, actionButton('button','Validate')))
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

    # connecting to the Database
    DBconnection <- reactive({
      databaseConnection(server=input$ip_address
                         , schema=input$database_schema
                         , user=input$user
                         , password=input$password)
    })
    observeEvent(input$connect,{
      DBcon <<- DBconnection()
      if(exists("connection")==TRUE){
        showModal(modalDialog(
          title = "Messeage", "Database connection success!!", easyClose = T, footer=modalButton("cancel"), size = "l"
        ))
      }
    })

    # Picker, showing the distinct note_type_concept_ids from the NOTE table in OMOP CDM
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

    # OMOP CDM NOTE table formatted CSV file upload and data extraction from the file
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

    # Preprocessing of the extracted text data
    observeEvent(input$process,{
      if(exists("connection")==TRUE){
        if(exists("input$resultdb")==T){
          cohortNoteExtract(num = input$ReportNum, date = input$date, result = TRUE, resultdb = input$resultdb, cohort = input$cohort, noteType = input$note_type)
          if(exists("input$dictionary_table")==T){useDictionary(useDictionary=T)}else{useDictionary(useDictionary=F)}
        } else{
          cohortNoteExtract(num = input$ReportNum, date = input$date, result = FALSE, noteType = input$note_type)
          if(exists("input$dictionary_table")==T){useDictionary(useDictionary=T)}else{useDictionary(useDictionary=F)}
        }} else{
          noteExtract(Text = Text, num = input$ReportNum, date = input$date, noteType = input$note_type)
          if(exists("input$dictionary_table")==T){useDictionary(useDictionary=T)}else{useDictionary(useDictionary=F)}
        }

      dtm <<- preProcess(text = Text_corpus,
                         english = input$english,
                         whitespace = input$whitespace,
                         stopwords = input$stopwords,
                         number = input$number,
                         punc = input$punc,
                         stem = input$stem,
                         lower = input$lower)
      rownames(dtm) <<- Text$NOTE_ID

      if(exists("dtm")==TRUE){
        showModal(modalDialog(title="Message", "Preprocessing has completed!!", easyClose = T, footer = modalButton("cancel"), size = "l"))
      }
    })

    # Drawing table with basic information of the data
    output$count <- DT::renderDataTable({
      drawPersonTable(Text = Text)
    })

    # Drawing the proportion of the note types
    output$pie <- plotly::renderPlotly({
      drawPieChart(Text = Text)
    })

    # Drawing age graph of patients in data
    output$age <- plotly::renderPlotly({
      drawAgeGraph(Text = Text)
    })

    # Draw note dates of the data
    output$date <- plotly::renderPlotly({
      drawNotedateGraph(Text = Text)
    })

    # To find the optimal number of the LDA analysis, drawing perplexity graph is necessary
    LDATuning <- eventReactive(input$Calc, {
      drawOptimalTopicNumber(dtm, TopicNum = input$TopicNum, By = input$By, metrics = input$metrics)
    })
    output$TuningGraph <- renderPlot({LDATuning()})

    # LDAvis is an interactive shiny application of the LDA analysis allowing visualozaion of the topics relationships
    VisSetting <- eventReactive(input$visButton,{
      shinyLDA(dtm = dtm, k = input$topicNum, iter = input$learningNum, alpha = input$alphaNum)
    })
    output$LDAModel <- LDAvis::renderVis({VisSetting()})

    # showing sample documents which have the highest probabilities of the LDA topics helps the users to understand their corpus
    output$SampleTopic <- DT::renderDataTable({
      topicSample(Text = Text, topic = input$topicNum, num = input$sample)
    })

    # This json editor is for editing and savinf json schema. It will be used for validation of the annotation process
    output$Schema <- listviewer::renderJsonedit({
      jsonEditor(filePath = input$UploadSchema$datapath)
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

    # This editor is for editing and saving JSON structure which will be used for annotation
    output$Template <- listviewer::renderJsonedit({
      jsonTemplateEditor(filePath = input$UploadTemplate$datapath)
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

    # This editor is the final editor and is for annotation process. It uses previously defined JSON schema and structure.
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
      ))
    })

    # save annotated Json object into the individual Json files.
    observeEvent(input$save, {
      saveJson(JSON = JSON)
    })

    # sourceText shows the original clinical reports(note_text of NOTE table) extracted before
    sourceText <- eventReactive(input$click,{
      num <<- input$num
      Text$NOTE_TEXT[as.numeric(input$num)]
    })

    output$note <- renderText({sourceText()})

    # noteId shows NOTE_ID of the NOTE table which is previously extracted
    noteId <- eventReactive(input$click,{
      num <<- input$num
      Text$NOTE_ID[as.numeric(input$num)]
    })
    output$noteid <- renderText({noteId()})

    # This is the validation prostep using previously defined JSON schema.
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

    # Sending individual Json files into the activated Elasticsearch. This process can be performed separately  from the other processes before.
    observeEvent(input$send, {
      if(exists(input$host|input$port)==T){
        esConnection <- elastic::connect(host = input$host, errors='complete') # port = input$port
      } else{
        esConnection <- elastic::connect(errors='complete')
      }
      jsonToES(esConnection, indexName = input$indexName, jsonFolder = input$filepath, dropIfExist = T)
    })
  })
)
