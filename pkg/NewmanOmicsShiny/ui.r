library(shiny)

shinyUI(fluidPage(
  
  titlePanel(title = "Neumann Statistic"),
  sidebarLayout(
    sidebarPanel(("Select statistic to run"),
                 
                 radioButtons("choice","Type",list("Banked","Paired", "Pilot"),"Banked"),
                 
                 conditionalPanel(condition = "input.choice=='Banked'",
                                  fileInput("file1", "Choose CSV File",
                                            accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                                  ),
                                  fileInput("file2", "Choose CSV File",
                                            accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                                  )),
                 
                 conditionalPanel(condition = "input.choice=='Paired'",
                                  fileInput("file1", "Choose CSV File",
                                            accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                                  ),
                                  fileInput("file2", "Choose CSV File",
                                            accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                                  )),
                 conditionalPanel(condition = "input.choice=='Pilot'",
                                  fileInput("file1", "Choose CSV File",
                                            accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                                  ),
                                  sliderInput("pVal", "Select your P value: ",
                                              min = 0, max = .1, value = .05, step = .01)
                                  
                                  )
               ),
     
      mainPanel(
      h1("Results"),
      plotOutput(outputId = "pilotStat", height = "300px")
     
      
      )
    
  )
  
  )
)
  
