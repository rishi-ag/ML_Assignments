if(!require("shiny")) install.packages("shiny")
if (!require("mvtnorm")) install.packages("mvtnorm")
if (!require("ggplot2")) install.packages("ggplot2")

source("helper.R")

shinyServer(
  function(input, output) {
    
    output$plot1 <- renderPlot({
      noAnimals <- c(input$NoObs1, input$NoObs2,input$NoObs3)
      rho  <- c(input$Rho1, input$Rho2, input$Rho3)
      sdXY <- list(c(input$SD1X, input$SD1Y), c(input$SD2X, input$SD2Y), c(input$SD3X, input$SD3Y))
      muXY <- list(c(input$MU1X, input$MU1Y), c(input$MU2X, input$MU2Y), c(input$MU3X, input$MU3Y))
      animalsDF <- animals(rho, sdXY, muXY, noAnimals)
      predictionList <- getPrediction(animalsDF)
      predictLabel <- predictionList[[1]]
      
      wCat <- coef(predictionList[[2]])
      wDog <- coef(predictionList[[3]])
      wMag <- coef(predictionList[[4]])
      plotBoundary(wCat, wDog, wMag, animalsDF)
      
    })
    
    #output$table1 <- renderTable({
     # noAnimals <- c(input$NoObs1, input$NoObs2,input$NoObs3)
      #rho  <- c(input$Rho1, input$Rho2, input$Rho3)
      #sdXY <- list(c(input$SD1X, input$SD1Y), c(input$SD2X, input$SD2Y), c(input$SD3X, input$SD3Y))
      #muXY <- list(c(input$MU1X, input$MU1Y), c(input$MU2X, input$MU2Y), c(input$MU3X, input$MU3Y))
      #animalsDF <- animals(rho, sdXY, muXY, noAnimals)
      #predictionList <- getPrediction(animalsDF)
      #predictLabel <- predictionList[[1]]
      #prop.table(table(animalsDF$Animal, predictLabel), 1)
    #})
  }
)