library(shiny)
library(shinyEventLogger)

source("r-dist-trace.R")

options(shiny.trace = FALSE, shiny.autoreload = TRUE)

ui <- fluidPage(
  tags$head(tags$script(src = "/newrelic.js")),
  titlePanel("newR"),
  actionButton("go", "Send a trace"),
  verbatimTextOutput("value"),
  actionButton("event", "Send error trace"),
  verbatimTextOutput("eventValue"),
  actionButton("logTrace", "Send trace and log"),
  verbatimTextOutput("logTraceValue"),
  actionButton("logErrorTrace", "Send error trace and log"),
  verbatimTextOutput("logErrorTraceValue"),
  tags$body(tags$script(src = "/nr-attrs.js"))
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
}

shinyApp(ui=ui, server=server)
