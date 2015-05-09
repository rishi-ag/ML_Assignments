if (!require("mvtnorm")) install.packages("mvtnorm")

get.sigmaXY <- function(rhoXY, sdX, sdY){
  cov <- rhoXY * sdX *sdY
  matrix(c(sdX^2, cov, cov, sdY^2), nrow = 2)
}

get.data <- function(n1 = 100, n2 = 100, mu1, mu2, sd1, sd2, rhoXY, seed = 1000) {
  set.seed(seed)
  features1 <- rmvnorm(n1, mean = c(mu1[1], mu1[2]), sigma = get.sigmaXY(rhoXY[1], sd1[1], sd1[2]))
  features2 <- rmvnorm(n2, mean = c(mu2[1], mu2[2]), sigma = get.sigmaXY(rhoXY[2], sd2[1], sd2[2]))
  features <- rbind(features1, features2)
  label <- c(rep(-1, n1), rep(1, n2))
  return (data.frame(X1 = features[,1], X2 = features[,2], label))
}

train.perceptron <- function(data.train,W,b, maxIter) {
  
  test.err.cnt <- 0
  test.err <- c()
  estimates <- c()
  X <- data.train[, 1:2]
  Y <- data.train[, 3]
  
  for(iter in 1:maxIter) {
    
    #shuffle data for faster convergence
    rand.seq <- sample(seq(1, nrow(data.train)), nrow(data.train))
    data.train <- data.train[rand.seq, ]
    
    #online learning
    for(i in 1:nrow(X)) {
      x <- as.matrix(X[i,])
      if (Y[i] * (x %*% W) <= 0) {
        test.err.cnt = test.err.cnt + 1
        W <- W + t(Y[i] * x)
        b <- b + Y[i]
      }
    }
    
    test.err <- c(test.err, test.err.cnt)
    estimates <- rbind(estimates,  c(t(W), b))
    
    if(test.err.cnt == 0) {
       break()
    }
    
    test.err.cnt <- 0
  }
  
  
  
  return(list(estimates = estimates, test.err))
}

test.perceptron <- function(data.test, W, b){
  X <- as.matrix(data.test[, 1:2])
  Y <- as.matrix(data.test[, 3])
  return (sum(((X %*% W + b) *  Y < 0)))
}

perceptron <- function(maxIter, data.train, data.test, W, b) {
  
  W <- matrix(rep(0,ncol(data.test[,1:2])), nrow = ncol(data.test[,1:2]))
  rownames(W) <- c("w1", "w2")
  b <- 0
  
  
  result <- train.perceptron(data.train = data.train, W = W, b = b, maxIter = maxIter)
  
  test.err <- apply(result[[1]], 1, function(x) test.perceptron(data.test, as.matrix(x[1:2]), x[3]))
  
  return (data.frame(train.err = result[[2]], test.err = test.err, est = result[[1]]))
}