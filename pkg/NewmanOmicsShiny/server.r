library(shiny)

shinyServer(function(input,output){
    
  output$myPlot <-renderPlot({
    
    distType <- input$choice
    #fileTwo <- input$file2
    
    if(distType == "Pilot"){
      
      fileOne <- input$file1
      
      pilotStat <- function(mat = data.matrix(input$file1)){
        n = dim(mat)[1]
        s = dim(mat)[2]
        
        temp.means  = unname(rowMeans(mat))
        temp.sd = rowSds(mat)
        
        loess.model = loess(temp.sd ~ temp.means)
        sdEst = predict(loess.model)
        
        nuValsAnova(matrix = mat, sd = sdEst)
        
        ## F measure = between group var/within group var
        ## p val = area under curve (correction needed?)
      }

      nuValsAnova = function(matrix,sd){
        nuVals = numeric(n)
        for (i in 1:n){
          nuVals[i] = (max(matrix[i, ]) - min(matrix[i, ]))/sd[i]
        }
        hist(nuVal, col="blue")
      }
    }
  })
  
})