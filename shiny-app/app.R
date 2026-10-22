# Local and deployable interface using byte-identical package functions in core/.
library(shiny)
source(file.path('core','compare_metrics.R'),local=TRUE)
source(file.path('core','export_comparison.R'),local=TRUE)
options(shiny.maxRequestSize=20*1024^2)
ui <- fluidPage(
 tags$head(tags$style(HTML('body{font-family:Arial,sans-serif;color:#243746}.container-fluid{max-width:1400px} .well{background:#f4f7f9} .help-block{color:#435563} .btn-primary{background:#275f77;border-color:#275f77} iframe{width:100%;height:900px;border:0}'))),
 titlePanel('FD Concordance'),
 p('Compare two calculated functional diversity metrics when prior evidence suggests potential overlap.'),
 sidebarLayout(
  sidebarPanel(width=3,
   fileInput('upload','1. Load your table',accept=c('.csv','.txt','.tsv','.xlsx','.xls')),
   helpText('One row per observation. Use already calculated index values.'),
   uiOutput('sheet_control'),
   conditionalPanel("input.upload != null",selectInput('separator','Text-file separator',c('Comma'=',','Semicolon'=';','Tab'='\t'))),
   uiOutput('columns'),
   checkboxInput('grouped','Repeated observations from the same site or lake',FALSE),
   conditionalPanel('input.grouped',uiOutput('group_column')),
   checkboxInput('control','Control for taxonomic richness',TRUE),
   conditionalPanel('input.control',uiOutput('richness_column')),
   selectInput('model','Continuous model',c('Second-order polynomial'='quadratic','Additive model (GAM / GAMM)'='gam')),
   conditionalPanel("input.model == 'gam'",numericInput('k','Smooth basis size',5,min=3,step=1)),
   checkboxInput('rationale','I have a prior reason to investigate overlap',FALSE),
   actionButton('calculate','Calculate',class='btn-primary'),
   helpText('Changing the table or settings clears the previous results. No automatic redundancy verdict is assigned.')
  ),
  mainPanel(width=9,
   tabsetPanel(
    tabPanel('Data',uiOutput('data_info'),tableOutput('preview')),
    tabPanel('Results',uiOutput('status'),tableOutput('summary'),uiOutput('report')),
    tabPanel('Save',uiOutput('save_controls')),
    tabPanel('About',h3('Four complementary analytical stages'),
      tags$ol(tags$li('Regression of one index against the other.'),tags$li('Spearman rank association.'),
              tags$li('Percentile classes and class-level Jaccard agreement.'),tags$li('Optional residual concordance after richness control.')),
      p('Grouping adds a random intercept for the sampling unit to the additive models. Raw-index and residual Spearman coefficients are descriptive: no Spearman P-values or significance tests are reported. Regression P-values refer to the fitted regression models.'),
      p('Percentile classes may differ in size when values are tied. The curve band is an approximate pointwise 95% interval for the fitted mean, not a prediction interval.'),
      p('Minimum input checks are computational safeguards, not sample-size recommendations. Interpret concordance using the biological question and sampling design.'),
      p('Development interface. This app compares index values; it does not calculate diversity indices from raw trait or abundance data.'))
   )
  )
 )
)
server <- function(input,output,session){
 state <- reactiveValues(result=NULL,export=NULL,error=NULL,prefix=NULL)
 sheets <- reactive({
   req(input$upload)
   if(tolower(tools::file_ext(input$upload$name)) %in% c('xlsx','xls'))
     readxl::excel_sheets(input$upload$datapath) else character()
 })
 output$sheet_control <- renderUI({if(length(sheets())) selectInput('sheet','Worksheet',sheets())})
 values <- reactive({
   req(input$upload)
   ext <- tolower(tools::file_ext(input$upload$name))
   a <- if(ext %in% c('xlsx','xls')) {
     req(input$sheet);as.data.frame(readxl::read_excel(input$upload$datapath,sheet=input$sheet))
   } else read.table(input$upload$datapath,header=TRUE,sep=if(ext=='tsv') '\t' else input$separator,
                     check.names=FALSE,stringsAsFactors=FALSE,comment.char='',quote='"',fileEncoding='UTF-8-BOM')
   validate(need(nrow(a)>0,'The table contains no observations.'),need(!anyDuplicated(names(a)),'Column names must be unique.'))
   a
 })
 numeric_names <- reactive(names(values())[vapply(values(),is.numeric,logical(1))])
 output$columns <- renderUI({
   n<-numeric_names();validate(need(length(n)>=2,'At least two numeric index columns are needed.'))
   tagList(h4('2. Select your indices'),selectInput('predictor','Predictor (horizontal axis)',n,selected=n[1]),
           selectInput('response','Response (vertical axis)',n,selected=n[2]))
 })
 output$group_column <- renderUI(selectInput('group','Site or lake identifier',names(values())))
 output$richness_column <- renderUI({n<-numeric_names();pick<-grep('richness|^R$',n,ignore.case=TRUE,value=TRUE)
   selectInput('richness','Richness column',n,selected=if(length(pick)) pick[1] else tail(n,1))})
 observeEvent(input$grouped,{if(isTRUE(input$grouped))updateSelectInput(session,'model',selected='gam')})
 output$data_info <- renderUI({a<-values();p(paste(nrow(a),'observations;',ncol(a),'columns. Preview shows the first ten rows.'))})
 output$preview <- renderTable(head(values(),10),digits=5,rownames=FALSE)
 # Invalidate stale results immediately after input changes.
 observeEvent(list(input$upload,input$sheet,input$separator,input$predictor,input$response,input$grouped,input$group,
                   input$control,input$richness,input$model,input$k,input$rationale),{
   state$result<-NULL;state$export<-NULL;state$error<-NULL
 },ignoreInit=TRUE)
 observeEvent(input$calculate,{
  state$result<-NULL;state$export<-NULL;state$error<-NULL
  tryCatch({
   a<-values();req(input$predictor,input$response)
   if(!isTRUE(input$rationale))stop('Please confirm that there is a prior reason to investigate overlap.')
   if(isTRUE(input$grouped)&&input$model!='gam')stop('For repeated observations, select the additive model with a site or lake identifier.')
   if(isTRUE(input$grouped)&&input$group %in% c(input$predictor,input$response,if(isTRUE(input$control)) input$richness))
      stop('The grouping identifier must be distinct from the numeric analysis columns.')
   withProgress(message='Calculating and preparing figures',value=.1,{
    z<-compare_metrics(a,input$response,input$predictor,
       richness=if(isTRUE(input$control)) input$richness else NULL,
       group=if(isTRUE(input$grouped)) input$group else NULL,
       model=input$model,k=if(input$model=='gam') input$k else 5)
    incProgress(.5)
    directory<-tempfile('fdconcordance-results-');dir.create(directory)
    report<-export_comparison(z,directory,paste(input$response,'versus',input$predictor))
    write.csv(a,file.path(directory,'input_values.csv'),row.names=FALSE)
    obs<-read.csv(file.path(directory,'observations.csv'),check.names=FALSE)
    for(nm in setdiff(names(a),names(obs)))obs[[nm]]<-a[[nm]][obs$source_row]
    write.csv(obs,file.path(directory,'observations.csv'),row.names=FALSE)
    if(!is.null(state$prefix))shiny::removeResourcePath(state$prefix)
    prefix<-paste0('fdc-',gsub('[^a-zA-Z0-9]','',basename(directory)))
    shiny::addResourcePath(prefix,directory)
    state$prefix<-prefix;state$result<-z;state$export<-report
    incProgress(.4)
   })
  },error=function(e){state$error<-conditionMessage(e)})
 })
 output$status<-renderUI({if(!is.null(state$error))return(div(role='alert',class='text-danger',state$error))
  if(is.null(state$result))return(p('Load a table, choose settings, and click Calculate.'))
  p(paste(state$result$n,'observations used;',length(state$result$excluded_rows),'rows excluded. Results apply to these data.'))})
 output$summary<-renderTable({req(state$export);a<-state$export$summary
  p<-grepl('P-value',a$Statistic,fixed=TRUE)
  a$Value<-ifelse(is.na(a$Value),'Not applicable',ifelse(p & a$Value<.001,'< 0.001',format(a$Value,digits=6,trim=TRUE)))
  a},rownames=FALSE)
 output$report<-renderUI({req(state$export);tags$iframe(src=paste0(state$prefix,'/OPEN_REPORT.html'),title='Four-stage analysis report')})
 output$save_controls<-renderUI({req(state$export);tagList(
   h4('Save your results'),downloadButton('all','All results (.tar.gz)'),downloadButton('pdf','Figures PDF'),
   downloadButton('summary_csv','Summary CSV'),downloadButton('observations_csv','Observations and residuals CSV'),
   helpText('The archive contains the report, figures, tables, model results, graph data and input table. Extract it and open OPEN_REPORT.html.'))})
 output$pdf<-downloadHandler(filename=function()'FD_Concordance_figures.pdf',content=function(file){req(state$export);file.copy(file.path(state$export$directory,'figures.pdf'),file)})
 output$summary_csv<-downloadHandler(filename=function()'FD_Concordance_summary.csv',content=function(file){req(state$export);file.copy(file.path(state$export$directory,'RESULTS_SUMMARY.csv'),file)})
 output$observations_csv<-downloadHandler(filename=function()'FD_Concordance_observations.csv',content=function(file){req(state$export);file.copy(file.path(state$export$directory,'observations.csv'),file)})
 output$all<-downloadHandler(filename=function()'FD_Concordance_results.tar.gz',content=function(file){
   req(state$export);old<-setwd(state$export$directory);on.exit(setwd(old),add=TRUE)
   utils::tar(file,files=list.files('.',all.files=FALSE),compression='gzip',tar='internal')
 })
 session$onSessionEnded(function(){prefix<-shiny::isolate(state$prefix);if(!is.null(prefix))shiny::removeResourcePath(prefix)})
}
shinyApp(ui,server)
