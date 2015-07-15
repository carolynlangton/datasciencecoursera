## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases

complete <- function(directory, id = 1:332) {
  ## Get the file names from the directory
  fileNames <- list.files(directory)
  
  ## Initialize vectors
  ids <- c()
  completeCasesCount <- c()
  
  ## Loop through the id values
  for (i in id) {
    ## Create a file name using the current id
    fileName <- sprintf("%03d.csv", i)
    
    ## Put together the file path
    filePath <- paste(directory, fileName, sep = "/")
    
    ## Get the data out of the file
    data <- read.csv(filePath)
    
    ## Add the id to the ids vector
    ids <- c(ids, i)
    
    ## Find the number of complete cases and add that number to the vector
    completeCases <- data[complete.cases(data),]
    completeCasesCount <- c(completeCasesCount, nrow(completeCases))
  }
  
  ## Create the data frame and return it
  df <- data.frame(id = ids, nobs = completeCasesCount)
  df
}