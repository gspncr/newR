library(httr)
library(uuid)
library(log4r)

NRAPIKey <-"<Insert API Key>"
RServiceName <- "Custom R Service"

logger <- create.logger(logfile = "debug.log", level = "DEBUG")
logger <- create.logger(logfile = "info.log", level = "INFO")

tracer <- function(serviceName, hostName, duration, name, UUID, spanID, desciption, userSessionId){
  if (missing(serviceName) || is.null(serviceName)){
    serviceName = RServiceName
  } 
  if (missing(hostName) || is.null(hostName)){
    hostName = Sys.info()['nodename']
  } 
  if (missing(UUID) || is.null(UUID)){
    UUID = UUIDgenerate()
  } 
  if (missing(spanID) || is.null(spanID)){
    spanID = UUIDgenerate()
  }
  if (missing(duration) || is.null(duration)){
    duration = 1.0
    #demo mode:   duration <- runif(1, 3.0, 100.5)
  }
  if (missing(name) || is.null(name)){
    name = 'Unnamed Call'
  }
  if (missing(desciption) || is.null(name)){
    desciption = "Sent using https://github.com/gspncr/newR"
  }
  if (missing(userSessionId) || is.null(name)){
    userSessionId = "Use session$token to include session Identifiers"
  }
  traceDyn = '[
           {
             "common": {
               "attributes": {
                 "service.name": "%s",
                 "host": "%s"
               }
             },
             "spans": [
               {
                 "trace.id": "%s",
                 "id": "%s",
                 "attributes": {
                   "duration.ms": %s,
                   "name": "%s",
                   "description": "%s",
                   "user.id": "%s"
                 }
               }
             ]
           }
         ]'
  readyTrace = sprintf(traceDyn, serviceName, hostName, UUID, spanID, duration, name, desciption, userSessionId)
  debug(logger, readyTrace)
  r <- POST("https://trace-api.newrelic.com/trace/v1", add_headers("Api-Key" = NRAPIKey, "Data-Format" = "newrelic", "Data-Format-Version" = 1, 
                                                                   "Content-Type" = "application/json"), body = readyTrace, encode="json")
  info(logger, r)
  return(r)
}
errorTracer <- function(serviceName, hostName, duration, name, UUID, spanID, errorName, desciption, userSessionId, errorText){
  if (missing(serviceName) || is.null(serviceName)){
    serviceName = RServiceName
  } 
  if (missing(hostName) || is.null(hostName)){
    hostName = Sys.info()['nodename']
  } 
  if (missing(UUID) || is.null(UUID)){
    UUID = UUIDgenerate()
  } 
  if (missing(spanID) || is.null(spanID)){
    spanID = UUIDgenerate()
  }
  if (missing(duration) || is.null(duration)){
    #duration = 1.0
    duration <- runif(1, 3.0, 100.5)
  }
  if (missing(name) || is.null(name)){
    name = 'Unnamed Call'
  }
  if (missing(desciption) || is.null(name)){
    desciption = "Sent using https://github.com/gspncr/newR"
  }
  if (missing(userSessionId) || is.null(name)){
    userSessionId = "Use session$token to include session Identifiers"
  }
  if (missing(errorName) || is.null(errorName)){
    errorName = "Unknown error"
  }
  if (missing(errorText) || is.null(errorText)){
    errorText = "This error is unpsecified. Include errorName and errorText details in your trace for more detail."
  }
  traceDyn = '[
           {
             "common": {
               "attributes": {
                 "service.name": "%s",
                 "host": "%s",
                 "error.name": "%s",
                 "error.text": "%s"
               }
             },
             "spans": [
               {
                 "trace.id": "%s",
                 "id": "%s",
                 "attributes": {
                   "duration.ms": %s,
                   "name": "%s",
                   "description": "%s",
                   "user.id": "%s"
                 }
               }
             ]
           }
         ]'
  readyTrace = sprintf(traceDyn, serviceName, hostName, errorName, errorText, UUID, spanID, duration, name, desciption, userSessionId)
  debug(logger, readyTrace)
  r <- POST("https://trace-api.newrelic.com/trace/v1", add_headers("Api-Key" = NRAPIKey, "Data-Format" = "newrelic", "Data-Format-Version" = 1, 
                                                                   "Content-Type" = "application/json"), body = readyTrace, encode="json")
  info(logger, r)
  return(r)
}