## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
## NOTE: Do not round the result!

corr <- function(directory, threshold = 0) {
  ## Get the observations where the number of complete cases is above the threshold
  completes <- complete(directory)
  completes <- subset(completes, nobs > threshold)
  
  ## Initialize a vector
  correlations <- c()
  
  ## Loop through ids
  for (i in completes$id) {
    ## Create a file name using the current id
    fileName <- sprintf("%03d.csv", i)
    
    ## Put together the file path
    filePath <- paste(directory, fileName, sep = "/")
    
    ## Get the data out of the file
    data <- read.csv(filePath)
    
    ## Find the number of complete cases
    completeCases <- data[complete.cases(data),]
    count <- nrow(completeCases)
    
    ## If the number of complete cases meets the threshold
    if (count >= threshold) {
      ## Determine the correlations
      correlations <- c(correlations, cor(completeCases$nitrate, completeCases$sulfate))
    }
  }
  
  ## Return the vector of correlations
  correlations
}