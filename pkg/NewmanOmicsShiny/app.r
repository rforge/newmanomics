## app.r


shinyApp(
  ui = tagList(
    # shinythemes::themeSelector(),
    navbarPage(
      theme = shinytheme("flatly"),  # <--- To use a theme, uncomment this
      "Select a statistic:",
      tabPanel("Pilot",
               sidebarPanel(
                 fileInput("file", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 sliderInput("slider", "Enter your alpha value:", 0,.1, .05, .005),
                 HTML('<script type="text/javascript">
                      $(document).ready(function() {
                      $("#file").click(function() {
                      $("#plot").text("Loading...");
                      });
                      });
                      </script>
                      ')
                 ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Significant genes/Patient",
                            h4("Heatmap"),
                            plotOutput(outputId = "pilotPlot"),
                            downloadButton(outputId = "down", label = "Download the plot")
                   )
                 )
               )
               ),
      tabPanel("Paired",
               sidebarPanel(
                 fileInput("file1", "Data1:"),
                 fileInput("file2", "Data2:")
                 
               )),
      tabPanel("Banked",
               sidebarPanel(
                 fileInput("fileB1", "Test Data:"),
                 helpText("AND"),
                 fileInput("fileB2", "Comparison Data:"),
                 helpText("OR"),
                 fileInput("fileB3", "Banked Object:")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Significant genes/Patient",
                            h4("Heatmap"),
                            tableOutput(outputId = "tester4")
                            # downloadButton(outputId = "down", label = "Download the plot")
                   ),
                   tabPanel("Gene Frequency in Patients"),
                   tabPanel("Network Graph")
                 )
               ))
  )
  ),
  server = function(input, output) {
    # PILOT
    data <- reactive({
      file1 <- input$file
      if(is.null(file1)){return()}
      read.table(file=file1$datapath, sep = ',', header = TRUE, row.names = 1)
    })
    
    output$tester1 <- renderTable({
      if(is.null(input$file)){return()}
      input$file
    })
    
    output$tester2 <- renderTable({
      if(is.null(input$file)){return()}
      mat1 = as.matrix(data())
      pvalnew = input$slider
      RN = rownames(mat1)
      
      a = pilotStat(mat1)
      
      
      n = dim(mat1)[1]
      s = dim(mat1)[2]
      
      temp.means  = unname(rowMeans(mat1))
      temp.sd = rowSds(mat1)
      
      loess.model = loess(temp.sd ~ temp.means)
      sdEst = predict(loess.model)
      
      nuValsPilot = nuValsAnova(matrix = mat1, sd = sdEst, n)
      
      
      pValsPilot = nuToPValPilot(mat1,n,s,temp.means,nuValsPilot)
      
      
      finStep(pValsPilot, pvalnew, RN, mat1)
      
    })
    
    
    output$pilotPlot <- renderPlot({
      if(is.null(input$file)){return()}
      mat1 = as.matrix(data())
      pvalnew = input$slider
      RN = rownames(mat1)
      
      a = pilotStat(mat1)
      
      n = dim(mat1)[1]
      s = dim(mat1)[2]
      
      temp.means  = unname(rowMeans(mat1))
      temp.sd = rowSds(mat1)
      
      loess.model = loess(temp.sd ~ temp.means)
      sdEst = predict(loess.model)
      
      nuValsPilot = nuValsAnova(matrix = mat1, sd = sdEst, n)
      
      pValsPilot = nuToPValPilot(mat1,n,s,temp.means,nuValsPilot)
      
      finStep(pValsPilot, pvalnew, RN, mat1)
      
    })
    
    output$down <- downloadHandler(
      filename = function(){
        paste("pilot.pdf")
      },
      content = function(file){
        pdf(file)
        mat1 = as.matrix(data())
        RN = rownames(mat1)
        pvalnew = input$slider
        
        a = pilotStat(mat1)
        
        n = dim(mat1)[1]
        s = dim(mat1)[2]
        
        temp.means  = unname(rowMeans(mat1))
        temp.sd = rowSds(mat1)
        
        loess.model = loess(temp.sd ~ temp.means)
        sdEst = predict(loess.model)
        
        nuValsPilot = nuValsAnova(matrix = mat1, sd = sdEst, n)
        
        pValsPilot = nuToPValPilot(mat1,n,s,temp.means,nuValsPilot)
        
        finStep(pValsPilot, pvalnew, RN, mat1)
      })
    # END PILOT
    # BANKED
    
    
    data <- reactive({
      fileB1 <- input$file
      if(is.null(fileB1)){return()}
      read.table(file=fileB1$datapath, sep = ',', header = TRUE, row.names = 1)
    })
    
    output$tester9 <- renderTable({
      if(is.null(input$fileB1)){return()}
      input$fileB1
    })
    
    output$tester4 <- renderTable({
      if(is.null(input$fileB1)){return()}
      mat4 = as.matrix(data())
      mat4
    })
    
  }
  )