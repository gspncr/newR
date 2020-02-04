# Monitoring R Shiny with New Relic
### Contents:
1. [View Demo Application](https://gspncr.shinyapps.io/newR/)
2. [About](#about)
3. [Traces](#sending-traces)
4. [Browser](#r-shiny-in-the-browser)
5. [Logs in context](#logs-in-context)
6. [Metrics](#metrics)

![](https://i.imgur.com/TdpLsJO.png)
![](https://i.imgur.com/s0Qgsse.png)
![](https://i.imgur.com/Mgehb01.png)

## About

This demo application shows sending a regular distributed trace to New Relic. There are not any child spans added in the demo yet, nor is it intelligent enough to be running with timers yet. You must add your Insert API Key to `r-dist-trace.R`

The browser monitoring requires your Browser snippet to be added to `www/newrelic.js` taking care to have removed the `<script>` tags.

## Sending Traces

Use the `tracer()`and `errorTracer()`functions in your R scripts by referencing the `r-dist-trace.R`file. Do this by adding `source("r-dist-trace.R")`into every file to reference.

All arguments are optional, the more you can add the more useful your traces will become:

1. `serviceName` pass the name of the service. This will default to **Custom R Service**. You can also override this globally by setting the variable *RServiceName* in the distributed trace source file.
2. `hostName` pass this to override the name of the host. This will automatically be set with the nodename of the host running the R script. 
3. `UUID` this is used as the Trace ID. This will by default be generated. If you need to update a span later, reference the same UUID for the new span to be added to the trace.
4. `spanID` this is the Span ID. This will by default be generated.
5. `duration` this is the time, in milliseconds, the span taken. By default this is 1 millisecond, you should look to control the timer yourself for this is not yet part of this solution.
6. `name` this is the name of the operation. This will default to *Unnamed Call*
7. `description` some description of the call. This will default to *Sent using ...*
8. `userSessionId` the ID of the user running the operation. This will default to an instruction placeholder. In all of your calls, you should pass `session$token` which will pass the Session ID generated by R Shiny to the trace and can be useful in debugging.

### Error Traces

Error traces use all of the same as above, but is called using errorTracer() and includes two additional arguments - along with all of the above. Those new arguments are:

1. `errorName`  the name of the error. This will default to *Unknown error*
2. `errorText` the message of the error, or additional context. This will default to *This error is unspecified...*

## R Shiny in the Browser

**Include your Browser snippet in newrelic.js** - make sure to remove trhe HTML Script tags!

### nr-attrs.js

This script includes metadata from R Shiny, and begins to send those attributes to New Relic Browser after Shiny has completed its own work in the webpage. This is done by checking that *Shiny* exists as an object in the window.

Variables that are captured out of the box:

1. `webSocketURL` : queried in the browser from `Shiny.shinyapp.$socket.url`
2. `webSocketState` : queried in the browser from `Shiny.shinyapp.$socket.readyState`
3. `webSocketBufferedAmount` : queried in the browser from `Shiny.shinyapp.$socket.bufferedAmount`
4. `shinyErrors` : queried in the browser from `Shiny.shinyapp.$errors`
5. `shinyConditionals` : queried in the browser from `Shiny.shinyapp.$conditionals`
6. `sessionID` : queried in the browser from `Shiny.shinyapp.config.sessionId`
7. `workerID` : queried in the browser from `Shiny.shinyapp.config.workerId`

You can optionally confugure the script to configure the output variables of your script, those that are sent to the browser (already available in the `Shiny.shinyapp.$values` collection). Inside here, after `$values` append the example values, e.g. *eventValue* and *value* to the name of yours. This will look like in your code as `output$value` where you are interested in the name after `$`

#### Page Actions

You can optionally but usefully, add in New Relic Browser Page Actions. This should correspond to your R Shiny form inputs. In the example app, find these correspond to the buttons by ID *go* and *event*. This is exactly the name for the input you provided in your R code. For instance `actionButton("go", "Send a trace")` where *go* is the input name, and Shiny keeps this as the name on the client side.

This allows you to set the name of the Page Action in New Relic. You should use the detail of the operation the input triggers. The event listener also triggers `checkShiny()` again after 1 second. If you have any new values in the page, these will be fetched as a result of that. So if you need to capture variables that are present after an input is triggered, you can add them within the `checkShiny()` function.

> **FYI**
>
> If you are uploading to say, shiny apps.io then the reference to the contents in www/ must be appended with the name of the application. For instance, uploading my project to gspncr.shinyapps.io/newR, any files I am referencing in www/ must be referenced such as: /newR/newrelic.js

## Logs in context

![](https://i.imgur.com/cTuAPRq.png)

Send log messages through `nrLogger()` function. If you pass the ID returned when you call *tracer()* or *errorTracer()* then the Log message will be linked to the trace. If you are sending a Log message without an associated trace, then a new trace ID will be generated for you. You can later link these to new traces.

All arguments apart from **message** can be automatically created. Even then, a placeholder is passed for *message* but there is not much point calling this function if you have no message to send.

1. `serviceName` pass the name of the service. This will default to **Custom R Service**. You can also override this globally by setting the variable *RServiceName* in the distributed trace source file.
2. `hostName` pass this to override the name of the host. This will automatically be set with the nodename of the host running the R script. 
3. `traceId` pass this to reference an existing trace. This will automatically create a new UUID if you do not pass a *traceId* - returned by `trace()` or `errorTrace()` functions.
4. `timestamp` pass this in EPOCH MS time. This will automatically be set to the current EPOCH time, according to the timezone of the running node.
5. `message` pass this to include the log message. This will be set with placeholder text if not set.

### Logging of the NR Tracing Source

The script will write to debug.log and info.log. These can be monitored using New Relic Logs for complete visibility. The outputs into those log files, are particularly for tracing. You can however use New Relic Logs to of course monitor any other Log files you desire.

## Metrics

You can use the function `newRMetric()` to send timeseries dimensional metrics to New Relic. There is a new series of arguments that are supported for metric data:

1. `metricName`  the name of the metric. This will default to **Custom R Metric**
2. `metricValue` the integer or double value of the metric. This will default to **0**
3. `hostName` pass this to override the name of the host. This will automatically be set with the nodename of the host running the R script.
4. `serviceName` pass the name of the service. This will default to **Custom R Service**. You can also override this globally by setting the variable *RServiceName* in the distributed trace source file.
5. `timestamp` pass this in EPOCH MS time. This will automatically be set to the current EPOCH time, according to the timezone of the running node.