if(!require("mvtnorm")) install.packages("mvtnorm")
if(!require("ggplot2")) install.packages("ggplot2")

#Wrapper functions

#Create variance covariance matrix
sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

#Create a BI Variate NOrmal distribution
genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

#Create a dataset for classification
animals <- function(rho, sdXY, muXY, noSim, seed = 5341) {
  catsVC     <- sigmaXY(rho[1], sdXY[[1]][1], sdXY[[1]][2])
  dogsVC     <- sigmaXY(rho[2], sdXY[[2]][1], sdXY[[2]][2])
  maggotsVC    <- sigmaXY(rho[3], sdXY[[3]][1], sdXY[[3]][2])
  
  catsBVN    <- genBVN(noSim[1], seed = 5341, muXY[[1]], catsVC)
  dogsBVN    <- genBVN(noSim[2], seed = 5342, muXY[[2]], dogsVC)
  maggotsBVN <- genBVN(noSim[3], seed = 5343, muXY[[3]], maggotsVC)
  
  animalsDf <- as.data.frame(rbind(catsBVN,dogsBVN, maggotsBVN))
  Animal <- c(rep("Cats", noSim[1]), rep("Dogs", noSim[2]), rep("Maggots", noSim[3]))
  animalsDf <- cbind(animalsDf, Animal)
  colnames(animalsDf) <- c("weight", "height", "Animal")
  
  return(animalsDf)
}


#Plot simulated values of Animals
plotAnimals <- function(animalDF) {
  p <- ggplot(data = animalDF, aes(x = height, y = weight, 
                                   colour=Animal, fill=Animal)) + 
    geom_point(alpha = 0.5) +
    xlab("Height") +
    ylab("Weight") +
    theme_bw(base_size = 14, base_family = "Helvetica") + 
    scale_colour_brewer(palette="Set1")
  p
}