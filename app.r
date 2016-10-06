suppressMessages (library(shiny))
suppressMessages (library(ggplot2))
suppressMessages (library(DT))
suppressMessages (library(tidyr))
suppressMessages (library(dplyr))
source("resamplestratifiedsamples.R")
options(shiny.maxRequestSize=250*1024^2) 
ui  <-  fluidPage(
  titlePanel("Hello GHAP HBGDki Member!"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Inputs", 
                 fileInput("datafile", "Choose csv file to upload", multiple = FALSE, accept = c("csv")),
                 uiOutput("dilution"),
                 uiOutput("keyvar"),
                 uiOutput("stratify"),
                 uiOutput("stratifyvar"),
                 uiOutput("samplefraction"),
                 uiOutput("ycol"),
                 uiOutput("xcol")
        ),
        hr()
      ) # tabpanel Inputs
    ), #sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel("Plot"  , 
                 plotOutput('plot',  width = "100%" ),
                 hr(),
                 plotOutput('plot2',  width = "100%" )
        ),#tabPanel Plot
        
        
        tabPanel('Uploaded Data',  dataTableOutput("mytablex") 
        ),#tabPanel Data,
        tabPanel('Trimmed Down Data',
                 h6("The download as CSV or Excel buttons below will export the data shown on the current page. If you want to save all the rows of the trimmed dataset make sure that Show all rows is selected (default)"),
                 dataTableOutput("mytablex2") 
        )
      )#tabsetPanel
    )#mainPanel
  )#sidebarLayout
)#fluidPage


server <-  function(input, output, session) {
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath,na.strings = c("NA","."))
    
    
  })
  
  samplefilterdata  <- reactive({
    if (is.null(filedata())) return(NULL)
    df <- filedata()
    if (is.null(df)) return(NULL)
    if(is.null(input$dilutionin)) {
      df <-  df 
    }
    
    if(!is.null(input$keyvarin)) {
      
      if(input$dilutionin&!input$stratifyon) {
        df <-  resample_df(df,key_cols=input$keyvarin,
                           strat_cols=NULL,
                           n=round(length(unique(df[,input$keyvarin]))*(input$samplefractionin/100),0),
                           replace = FALSE)
      }
      if(input$dilutionin&input$stratifyon) {
        df <-  resample_df(df,key_cols=input$keyvarin,
                           strat_cols=input$stratifyvarin,
                           n=round(length(unique(df[,input$keyvarin]))*(input$samplefractionin/100),0),
                           replace = FALSE)
      }
      #print(df)
    }
    
    if(is.null(input$keyvarin)) {
      df$NROW <- 1:nrow(df)
      
      if(input$dilutionin&!input$stratifyon) {
        df <-  resample_df(df,key_cols="NROW",
                           strat_cols=NULL,                                      
                           n=round(nrow(df)*(input$samplefractionin/100),0),
                           replace = FALSE)
      }
      if(input$dilutionin&input$stratifyon) {
        df <-  resample_df(df,key_cols="NROW",
                           strat_cols=input$stratifyvarin,
                           n=round(nrow(df)*(input$samplefractionin/100),0),
                           replace = FALSE)
      }
      print(df)
    }       
    
    df
  })
  
  
  output$ycol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("y", "y variable:",choices=items,selected = items[1],multiple=FALSE,selectize=TRUE)
  })
  
  output$xcol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("x", "x variable:",items,selected=items[2])
    
  })
  
  
  output$dilution <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    checkboxInput('dilutionin', 'Sub-Sample data ?', value = TRUE)
    
  })
  output$stratify <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    conditionalPanel(condition = "input.dilutionin" ,
                     checkboxInput('stratifyon', 'Stratify On ?', value = FALSE) )
    
  })
  output$samplefraction <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    conditionalPanel(condition = "input.dilutionin" ,
                     sliderInput("samplefractionin", "Percent Data to Keep ?", min=1, max=100, value=c(10),step=1)
    )                     
  })
  
  
  
  output$keyvar <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- c(names(df))  #[! MODEDF ]
    conditionalPanel(condition = "input.dilutionin" ,
                     selectizeInput(  "keyvarin",
                                      "Use this variable as a sampling unit (default to None and then by row sampling is performed):", choices = c(NAMESTOKEEP2 ),multiple=TRUE,
                                      options = list(
                                        maxItems = 1 ,
                                        placeholder = 'Please select one variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )
                     ))
  })
  
  output$stratifyvar <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    conditionalPanel(condition = "input.stratifyon" ,
                     selectizeInput(  "stratifyvarin", "Stratify on up to three categorical variables:", choices = NAMESTOKEEP2,multiple=TRUE,
                                      options = list(
                                        maxItems = 3 ,
                                        placeholder = 'Please select up to three variables',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      ))
    )
  })
  
  
  
  output$mytablex = renderDataTable({
    datatable( filedata() ,
               extensions = c('ColReorder','Buttons','FixedColumns'),
               options = list(dom = 'Bfrtip',
                              searchHighlight = TRUE,
                              pageLength=10 ,
                              lengthMenu = list(c(5, 10, 15, -1), c('5','10', '15', 'All')),
                              colReorder = list(realtime = TRUE),
                              buttons = c('colvis','pageLength'),
                              scrollX = TRUE,scrollY = 400,
                              fixedColumns = TRUE
               ), 
               filter = 'bottom',
               style = "bootstrap")
  })
  
  output$mytablex2 = renderDataTable({
    datatable( samplefilterdata() ,
               extensions = c('ColReorder','Buttons','FixedColumns'),
               options = list(dom = 'Bfrtip',
                              searchHighlight = TRUE,
                              pageLength=-1 ,
                              lengthMenu = list(c(5, 10, 15, -1), c('5','10', '15', 'All')),
                              colReorder = list(realtime = TRUE),
                              buttons = c('colvis','pageLength', 'csv','excel'),
                              scrollX = TRUE,scrollY = 400,
                              fixedColumns = TRUE
               ), 
               filter = 'bottom',
               style = "bootstrap")
  })
  
  plotObject <- reactive({
    validate(
      need(!is.null(filedata()), "Please select a data set") 
    )
    
    plotdata <- filedata()
    if(!is.null(plotdata)) {
      p <- ggplot(plotdata, aes_string(x=input$x, y=input$y)) 
      if (!is.null(input$keyvarin)  )
        p <- p + aes_string(group=input$keyvarin)
      p <- p+
        geom_point()
      if (!is.null(input$keyvarin)  ){
        p <- p+
          geom_line()           
      }
      
      if (!is.null(input$stratifyvarin)  ){
        p <- p+
          facet_wrap(input$stratifyvarin)           
      }
      
      
      p
    }
  })
  
  output$plot <- renderPlot({
    plotObject()
  })
  
  
  plotObject2 <- reactive({
    validate(
      need(!is.null(samplefilterdata()), "Please select a data set") 
    )
    plotdata <- samplefilterdata()
    if(!is.null(plotdata)) {
      p <- ggplot(plotdata, aes_string(x=input$x, y=input$y)) 
      if (!is.null(input$keyvarin)  )
        p <- p + aes_string(group=input$keyvarin)
      p <- p+
        geom_point()
      if (!is.null(input$keyvarin)  ){
        p <- p+
          geom_line()           
      }
      if (!is.null(input$stratifyvarin)  ){
        p <- p+
          facet_wrap(input$stratifyvarin)           
      }
      
      
      p
    }
  })
  
  output$plot2 <- renderPlot({
    plotObject2()
  })
  
}

shinyApp(ui = ui, server = server,  options = list(height = 2000))


