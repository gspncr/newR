function checkShiny() {
    if (typeof Shiny == 'object') {
        console.log("shiny loaded");
        newrelic.setCustomAttribute('websocketURL', Shiny.shinyapp.$socket.url);
        newrelic.setCustomAttribute('websocketState', Shiny.shinyapp.$socket.readyState);
        newrelic.setCustomAttribute('websocketBufferedAmount', Shiny.shinyapp.$socket.bufferedAmount);
        newrelic.setCustomAttribute('shinyErrors', Shiny.shinyapp.$errors);
        newrelic.setCustomAttribute('shinyConditionals', Shiny.shinyapp.$conditionals);
        newrelic.setCustomAttribute('sessionID', Shiny.shinyapp.config.sessionId);
        newrelic.setCustomAttribute('workerID', Shiny.shinyapp.config.workerId);

        //customise these items with your variable names
        newrelic.setCustomAttribute('shinyValues', Shiny.shinyapp.$values);
        newrelic.setCustomAttribute("value", Shiny.shinyapp.$values.value);
        newrelic.setCustomAttribute("eventValue", Shiny.shinyapp.$values.eventValue);
        newrelic.interaction().setAttribute("value", Shiny.shinyapp.$values.value);
        newrelic.interaction().setAttribute("eventValue", Shiny.shinyapp.$values.eventValue);
    }
  }
 
setTimeout(checkShiny, 500);

//customise these inputs with your form values
document.getElementById("go").addEventListener("click", function(){
    newrelic.addPageAction('tracePressed');
    setTimeout(checkShiny, 1000);
  });

document.getElementById("event").addEventListener("click", function(){
    newrelic.addPageAction('errorTracePressed');
    setTimeout(checkShiny, 1000);
  });