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
  tags$body(tags$script(src = "/nr-attrs.js"))
)

server <- function(input,output,session){
  observeEvent(input$go, {
    tr = tracer(userSessionId = session$token)
    output$value <- renderText("trace sent to new relic")
  })
  observeEvent(input$event, {
    etr = errorTracer(name = "/error", errorName = "BadError", errorText = "oh no test failed!", userSessionId = session$token)
    output$eventValue <- renderText("error trace sent to new relic")
  })
}

shinyApp(ui=ui, server=server)
