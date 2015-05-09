    if (!require("devtools") install.packages("devtools")
    if (!require("ggplot2")) install.packages("ggplot2")
    if (!require("reshape2")) install.packages("reshape2")
    if (!require("ggthemes")) install.packages("ggthemes")
    
    source("c:/Users/Rishabh/Documents/BGSE Material/Sem2/14D005 Machine Learning/Seminar 5/HW/helper.R")
    #data parameters
    
    mu <- seq(33, 20, -0.5)
    comparison<- c()
    plots <- c()
    
    for( i in 1:length(mu)) {
      
      rhoXY <- c(0.8, 0.5)
      sd1 <- c(2, 2)
      mu1 <- c(15, mu[i])
      sd2 <- c(1, 1.5)
      mu2 <- c(15, 15)
      
      #test and train data
      data.train <- get.data(300,300, mu1, mu2, sd1, sd2, rhoXY, 2500)
      data.test <- get.data(200,200, mu1, mu2, sd1, sd2, rhoXY, 2100)
      
      
      #Calculate R
      mean.data <- colMeans(data.train[,1:2])
      R <- max(apply(data.train[,1:2], 1, function(x){sqrt(sum((x - mean.data)^2))}))
      
      
      #Perceptron Algorithm
      maxIter <- 100
      info <- perceptron(maxIter, data.train, data.test, W, b)
      errors <- info[,1:2]
      errors$iter <- 1:nrow(errors)
      
      #calculate gamma
      W <-  as.matrix(info[nrow(info), 3:4])
      b <- info[nrow(info), 5]
      data <- t(as.matrix(data.train[,1:2]))
      gamma <- min(abs((W %*% data + b)/ norm(W, type = "2")))
      
      #test vs train
      melt.error <- melt(errors, id.vars = "iter", variable.name = "Type", value.name = "Error")
      
      #plots <- rbind(plots, ggplot(melt.error, aes(x = iter, y = Error, color = Type)) + 
      #  geom_line() + scale_colour_tableau())
      
      
      ################################################################################
      comparison <- rbind(comparison, c(R, gamma, nrow(info)))
    }
    
    
    colnames(comparison) <- c("R", "gamma", "convergence")
    comparison <- as.data.frame(comparison)
    comparison <- round(comparison, 3)
    
    p1 <- ggplot(comparison, aes(x = R, y = convergence, color = gamma)) +
    geom_point(size = 5, alpha = 0.9) +
    scale_color_gradient(low = "#fd8d3c", high = "#253494")
    
    p1 <- p1 + 
    annotate("text", label = "Non Seperable", x = 10, y = 90, size = 3, colour = "black") +
    annotate("text", label = "No Convergence", x = 10, y = 80, size = 3, colour = "black")
    
    jpeg(filename = 'CovergenceTrend.jpg', units = "in", width = 9, height = 9, res = 400)
    p1
    dev.off()
    