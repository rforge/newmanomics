library(shiny)

shinyServer(function(input,output){
    
  output$myPlot <-renderPlot({
    
      
      fileOne <- input$file1
      
      output$pilotStat <- function(mat = data.matrix(input$file1)){
        n = dim(mat)[1]
        s = dim(mat)[2]
        
        temp.means  = unname(rowMeans(mat))
        temp.sd = rowSds(mat)
        
        loess.model = loess(temp.sd ~ temp.means)
        sdEst = predict(loess.model)
        
        nuValsPilot = nuValsAnova(matrix = mat, sd = sdEst, n)
        
        pValsPilot = nuToPValPilot(mat,n,s,temp.means,nuValsPilot)
        ## F measure = between group var/within group var
        ## p val = area under curve (correction needed?)
        
        #write.csv(pValsPilot,"C:/Users/anous/Documents/R/BMI Research/Output/PVals_Pilot.csv",row.names = RN)
        
      }
      
      nuValsAnova = function(matrix,sd, n){
        nuVals = numeric(n)
        for (i in 1:n){
          nuVals[i] = (max(matrix[i, ]) - min(matrix[i, ]))/sd[i]
        }
        return(nuVals)
      }
      
      nuToPValPilot = function(matrix,n,s,mean, nuVals){
        wg = numeric(n)
        for (i in 1:n){
          wg[i] = sum(abs(matrix[i,] - mean[i])^2)/(s-2)
        }
        
        fStat = wg/nuVals
        pValsPilot = pf(fStat,1,1)
        
        return(pValsPilot)
      }
  })
  
})