library(shiny)
library(shinyEventLogger)
library(readxl)
library(tidyverse)
library(qicharts2)

source("r-dist-trace.R")

options(shiny.trace = FALSE, shiny.autoreload = TRUE, port=6231)

ui <- fluidPage(
  tags$head(tags$script(src = "/newrelic.js")),
  titlePanel("newR"),
  h3("Use these buttons to send sample traces to New Relic"),
  actionButton("go", "Send a trace"),
  verbatimTextOutput("value"),
  actionButton("event", "Send error trace"),
  verbatimTextOutput("eventValue"),
  actionButton("logTrace", "Send trace and log"),
  verbatimTextOutput("logTraceValue"),
  actionButton("logErrorTrace", "Send error trace and log"),
  verbatimTextOutput("logErrorTraceValue"),
  actionButton("sendEvent", "Send an event"),
  verbatimTextOutput("sendEventValue"),
  tags$body(tags$script(src = "/nr-attrs.js")),
  h3("Upload the sample spreadsheet to plot a chart, which sends plot values to New Relic"),
  p('download sample spreadhsheet:    https://github.com/gspncr/newR/blob/master/upload_example.xlsx?raw=true'),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Upload Excel")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("spcPlot")
    )
  )
)

server <- function(input,output,session){
  observeEvent(input$go, {
    tr = tracer(userSessionId = session$token)
    print(tr)
    output$value <- renderText("trace sent to new relic")
  })
  observeEvent(input$event, {
    etr = errorTracer(name = "/error", errorName = "BadError", errorText = "oh no test failed!", userSessionId = session$token)
    print(etr)
    output$eventValue <- renderText("error trace sent to new relic")
  })
  observeEvent(input$logTrace, {
    tr = tracer(userSessionId = session$token)
    print(tr)
    log = nrLogger(traceId = tr, message="message sent from logTrace button")
    print(log)
    output$logTraceValue <- renderText("trace and log sent to new relic")
  })
  observeEvent(input$logErrorTrace, {
    etr = errorTracer(name = "/errorAndLog", errorName = "ErrorWithLog", errorText = "a bad message and a log too!", userSessionId = session$token)
    print(etr)
    log = nrLogger(traceId = etr, message="message sent from logErrorTrace button!!")
    print(log)
    output$logErrorTraceValue <- renderText("error trace and log sent to new relic")
  })
  observeEvent(input$sendEvent, {
    nEvent = newREvent()
    output$sendEventValue <- renderText("event sent to new relic")
  })
  output$spcPlot <- renderPlot({
    
    df <- read_excel(input$fileUpload$datapath)
    #print(df[[2]])
    for (value in df[[2]]){
      nrMetric = newRMetric(metricName="PlotTheDotsValue", metricValue=value)
    }
    
    names(df) <- c("Date", "Value")
    
    df %>%
      qic(Date, Value,
          data     = .)
    
  })
}

shinyApp(ui=ui, server=server)
