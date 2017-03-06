library(shiny)

shinyServer(function(input,output){
    
    output$mychoice <- renderText(input$choice)
    
    output$normsub <- renderText({ input$normal })
    output$tumsub <- renderText({ input$tumor })
    
    #This function is repsonsible for loading in the selected file
    filedata <- reactive({
      infile <- input$file1
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      read.csv(infile$datapath)
    })
    
    
    #This previews the CSV data file
    output$filetable <- renderTable({
      filedata()
    })
    
    source("/Users/Gershkowitz/Desktop/Shiny/IteratedBank.R")
    mybank(filedata, normal, tumor)
    
    
})