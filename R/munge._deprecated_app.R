munge_deprecated_app<-function(){
  require(shiny)
  require(bufferfactor)
  require(dplyr)
  # Define UI for application that draws a histogram
  ui <- shinyUI(fluidPage(
    titlePanel("BufferFactor"),
    br(),
    mainPanel(
      textInput('expnm',"Name of Experiment", value = "BufferCapacityAnalysis", width = NULL),
      checkboxInput("CB", label = "Export from .Asyr", value = FALSE),
      br(),
      actionButton("BB","Run Analysis"),
      br(),
      actionButton('Quit','Quit',icon=icon('remove-sign',lib='glyphicon')),
      br(),br(),
      textOutput("session"),
      tableOutput("test1")
    )))

  server <- shinyServer(function(input, output, session) {

    observeEvent(input$Quit, {
      stopApp(returnValue = invisible())
    })


    observe({
      if(input$BB > 0 ){
        DIR<-choose.dir()
        if(input$CB==TRUE){PipeFish::Outandsave(DIR);DIR<-file.path(DIR,'export')}
        DF<-list.files(DIR,full.names=T,pattern='xlsx') %>% lapply(.,munge_deprecated) %>% bind_rows()
        svpth<-file.path(DIR,paste0(input$expnm,".csv"))
        output$session <- renderText(svpth)
        output$test1 <- renderTable({DF})
        write.csv(DF,file=svpth,row.names = F)
      }
    })

  })

  # Run the application
  shinyApp(ui = ui, server = server)

}
