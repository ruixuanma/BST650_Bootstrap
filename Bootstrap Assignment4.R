wcgs_df <- read.csv("/Users/maruixuan/Documents/Documents/GitHub/BST650_Bootstrap/wcgs.csv", header = TRUE, stringsAsFactors = FALSE)


testFun <- function(data, statistic, n){
  
  data <- na.omit(data) # will work for matrices or atomic vectors
  
  if (is.matrix(data) || is.data.frame(data)){
    # if data is matrix or data frame 
    if (n <= nrow(data)){
      sample_idx <- sample(1:nrow(data), size = n, replace = TRUE)
      sampleOfData <- data[sample_idx, ]
    } else {
      stop("n is too big")
    }

  } else if (is.atomic(data)){
    # atomic helps us to find basic vectors
    if(n <= length(data)){
      sampleOfData <- sample(data, size = n, replace = TRUE)
    } else {
      stop("n is too big")
    }
  } else {
    # not matrix, not data frame, not atomic. It would be an error
    stop("not matrix, not data frame, not atomic")
  }
  

  statistic(sampleOfData)
}

# Test
testFun(wcgs_df, summary, 10)

testFun(wcgs_df$dbp, mean, 10)

testFun(as.list(wcgs_df), summary, 10)

testFun(wcgs_df$dbp, mean, 4000)


