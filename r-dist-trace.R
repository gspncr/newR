library(httr)
library(uuid)
library(log4r)

NRAPIKey <-"insert API key"
RServiceName <- "Custom R Service"

logger <- create.logger(logfile = "debug.log", level = "DEBUG")
logger <- create.logger(logfile = "info.log", level = "INFO")

tracer <- function(serviceName, hostName, duration, name, UUID, spanID, description, userSessionId){
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
  if (missing(description) || is.null(name)){
    description = "Sent using https://github.com/gspncr/newR"
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
  readyTrace = sprintf(traceDyn, serviceName, hostName, UUID, spanID, duration, name, description, userSessionId)
  debug(logger, readyTrace)
  r <- POST("https://trace-api.newrelic.com/trace/v1", add_headers("Api-Key" = NRAPIKey, "Data-Format" = "newrelic", "Data-Format-Version" = 1, 
                                                                   "Content-Type" = "application/json"), body = readyTrace, encode="json")
  info(logger, r)
  return(UUID)
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
  return(UUID)
}
nrLogger <- function(serviceName, hostName, traceId, timestamp, message){
  if (missing(serviceName) || is.null(serviceName)){
    serviceName = RServiceName
  }
  if (missing(hostName) || is.null(hostName)){
    hostName = Sys.info()['nodename']
  }
  if (missing(traceId) || is.null(traceId)){
    traceId = UUIDgenerate()
  }
  if (missing(timestamp) || is.null(timestamp)){
    timestamp = as.numeric(as.POSIXct(Sys.time()))
  }
  if (missing(message) || is.null(message)){
    message = "Log message from your Custom R Service"
  }
  log = '[{
     "common": {
       "attributes": {
         "service-name": "%s",
         "hostname": "%s",
         "trace.id": "%s"
       }
     },
     "logs": [{
         "timestamp": %s,
         "message": "%s"
       }]
  }]'
  logReady = sprintf(log,serviceName, hostName, traceId, timestamp, message)
  #print(logReady)
  debug(logger, logReady)
  r <- POST("https://log-api.newrelic.com/log/v1", add_headers("X-Insert-Key" = NRAPIKey,  
                                                               "Content-Type" = "application/json"), body = logReady, encode="json")
  info(logger, r)
  return(traceId)
}
newRMetric <- function(metricName, metricValue, hostName, timestamp, serviceName){
  if (missing(serviceName) || is.null(serviceName)){
    serviceName = RServiceName
  }
  if (missing(hostName) || is.null(hostName)){
    hostName = Sys.info()['nodename']
  }
  if (missing(timestamp) || is.null(timestamp)){
    timestamp = as.numeric(as.POSIXct(Sys.time()))
  }
  if (missing(metricName) || is.null(metricName)){
    metricName = "Custom R Metric"
  }
  if (missing(metricValue) || is.null(metricValue)){
    metricValue = 0
  }
  metric = '
      [{ 
              "metrics":[{ 
                 "name":"%s", 
                 "type":"gauge",
                 "value":%s,
                 "attributes":{"host.name":"%s", "service.name":"%s"},
                 "timestamp": %s 
                 }] 
          }]
      '
  metricsReady = sprintf(metric, metricName, metricValue, hostName, serviceName, timestamp)
  r <- POST("https://metric-api.newrelic.com/metric/v1", add_headers("Api-Key" = NRAPIKey,  
                                                                     "Content-Type" = "application/json"), body = metricsReady, encode="json")
  debug(logger, r)
  return(r)
}
newREvent <- function(eventType, RTestName, RTestResult, RTestTimeTaken, hostName, sessionID, serviceName){
  if (missing(eventType) || is.null(eventType)){
    eventType = "RCustomEvent"
  }
  if (missing(RTestName) || is.null(RTestName)){
    RTestName = "Plot The Dots Example"
  }
  if (missing(RTestResult) || is.null(RTestResult)){
    RTestResult = "Success"
  }
  if (missing(RTestTimeTaken) || is.null(RTestTimeTaken)){
    RTestTimeTaken <- runif(1, 3.0, 100.5)
  }
  if (missing(hostName) || is.null(hostName)){
    hostName = Sys.info()['nodename']
  }
  if (missing(sessionID) || is.null(sessionID)){
    sessionID = UUIDgenerate()
  }
  if (missing(serviceName) || is.null(serviceName)){
    serviceName = RServiceName
  }
  event = '
        [
          {
            "eventType":"%s",
            "RTestName":"%s",
            "RTestResult":"%s",
            "RTestTimeTaken": %s,
            "hostName":"%s",
            "sessionID":"%s",
            "serviceName":"%s"
          }
        ]
      '
  readyEvent = sprintf(event, eventType, RTestName, RTestResult, RTestTimeTaken, hostName, sessionID, serviceName)
  r <- POST("https://insights-collector.newrelic.com/v1/accounts/1147177/events", add_headers("X-Insert-Key" = NRAPIKey,  
                                                                                              "Content-Type" = "application/json"), body = readyEvent, encode="json")
  info(logger, r)
  return(r)
}
trace.start <- function(){
  timeStart = as.numeric(as.POSIXct(Sys.time()))
  return(timeStart)
}
trace.end <- function(timeStart, userSessionId, name, description){
  if(missing(timeStart)){
    timeStart = as.numeric(as.POSIXct(Sys.time()))
  }
  if (missing(userSessionId) || is.null(userSessionId)){
    userSessionId = UUIDgenerate()
  } 
  timeEnd = as.numeric(as.POSIXct(Sys.time()))
  timeTotal = timeEnd - timeStart
  tracer(duration = timeTotal, userSessionId = userSessionId, name = name, description = description)
  return(userSessionId)
}