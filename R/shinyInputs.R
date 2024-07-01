#' @importFrom shiny runApp fluidPage observe stopApp actionButton numericInput 
#' @importFrom shiny shinyApp textInput passwordInput checkboxInput tags



shinyNumInput<-function(myLabel, value=0) {
  if (interactive()) {
    myInput<- runApp(list( #qui restituisce il valore inserito
      ui = fluidPage(
        tags$style(".form-group.shiny-input-container { width: 800px; }"),
        numericInput(inputId='n', label=myLabel, value=value),
        actionButton(inputId="pressed", label="Insert")
      ),
      server =function(input, output,session) {
        observe({
          #print(input$pressed)
          if(input$pressed > 0){
            stopApp(returnValue = input$n)
          } #close if
        })# close observe
      }#close server
    ))
  }
return(myInput)
}
#shinyApp(ui,server)


shinyTextInput<-function(myLabel, value="") {
  if (interactive()) {
    myInput<- runApp(list( #qui restituisce il valore inserito
      ui = fluidPage(
        tags$style(".form-group.shiny-input-container { width: 800px; }"),
        textInput(inputId="text", label=myLabel, value = value, width = NULL, placeholder = NULL),
        actionButton(inputId="pressed", label="Insert")
      ),
      server =function(input, output,session) {
        observe({
          #print(input$pressed)
          if(input$pressed > 0){
            stopApp(returnValue = input$text)
          } #close if
        })# close observe
      }#close server
    ))
  }
  return(myInput)
}
#shinyApp(ui,server)


shinyPwdInput<-function(myLabel) {
  if (interactive()) {
    myInput<- runApp(list( #qui restituisce il valore inserito
      ui = fluidPage(
        tags$style(".form-group.shiny-input-container { width: 800px; }"),
        passwordInput(inputId="text", label=myLabel, value = "", width = NULL, placeholder = NULL),
        actionButton(inputId="pressed", label="Insert")
      ),
      server =function(input, output,session) {
        observe({
          #print(input$pressed)
          if(input$pressed > 0){
            stopApp(returnValue = input$text)
          } #close if
        })# close observe
      }#close server
    ))
  }
  return(myInput)
}


shinyBooleanInput<-function(myLabel) {
  if (interactive()) {
    myInput<- runApp(list( #qui restituisce il valore inserito
      ui = fluidPage(
        tags$style(".form-group.shiny-input-container { width: 800px; }"),
        checkboxInput(inputId='boolean', label=myLabel, value=FALSE),
        actionButton(inputId="pressed", label="Insert")
      ),
      server =function(input, output,session) {
        observe({
          #print(input$pressed)
          if(input$pressed > 0){
            stopApp(returnValue = input$boolean)
          } #close if
        })# close observe
      }#close server
    ))
  }
  return(myInput)
}
#shinyApp(ui,server)
