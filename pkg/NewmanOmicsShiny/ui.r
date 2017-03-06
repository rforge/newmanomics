library(shiny)

shinyUI(fluidPage(
  
  titlePanel(title = "Neumann Statistic"),
  sidebarLayout(
    sidebarPanel(("Input Dataset"),
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 
                 
                radioButtons("choice","Select Option",list("Banked","Paired", "Pilot"),""),
    textInput("tumor", "Group1", ""),
    textInput("normal", "Group2", "")),
     
    mainPanel(
      h1("Results"),
      
      verbatimTextOutput("normsub"),
      verbatimTextOutput("tumsub"),
  
      textOutput("mychoice"),
      
      tableOutput("filetable")
      
      )
    
  )
  
  )
)
  
