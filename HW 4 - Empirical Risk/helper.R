#Define Buckets and arrange data according to budget
createKFolds <- function(data, noBuckets = 10) {
  noObs <- nrow(data)
  indexBucket <- rep(1:noBuckets, times = ceiling(noObs / noBuckets))
  ##randomising the index of buckets
  indexBucket <- sample(indexBucket, noObs)
  ##Assign each data point to a bucket by adding indexBucket as a column
  data <- data %>% mutate(indexBucket) %>% arrange(indexBucket)
  names(data)[1] <- "Y"
  data
}

#perform validation on given k and given bucketno
knn.validation <- function(data, bucketNo, k) {
  
  xtrain <- data %>% filter(indexBucket != bucketNo) %>% select(-Y, - indexBucket) 
  ytrain <- data %>% filter(indexBucket != bucketNo) %>% select(Y)
  xtest <- data %>% filter(indexBucket == bucketNo) %>% select(-Y, - indexBucket)
  ytest <- data %>% filter(indexBucket == bucketNo) %>% select(Y)
  
  pred <- knn(train = xtrain, test = xtest, cl = ytrain$Y, k = k)
  error <- mean(pred != ytest$Y)
  return(c(k = k , bucketNo = bucketNo, test.error = error))
}

#resubstitutions
resubstitution <- function(data, k) {
  pred <- knn(train = data[,-1], test = data[,-1], cl = data[,1], k = k)
  return(c(k =k, test.error = mean(pred != data[,1])))
}


#monte carlo simulation
monte.carlo <- function(data, k) {
  noObs <- nrow(data)
  ind1 <- rep(1, ceiling(noObs * 0.90))
  ind2 <- rep(2, ceiling(noObs * 0.10))
  indexBucket <- c(ind1, ind2)
  indexBucket <- sample(indexBucket, noObs)
  data <- data %>% mutate(indexBucket)
  names(data)[1] <- "Y"
  knn.validation(data, 2, k)[c(1, 3)]
}

