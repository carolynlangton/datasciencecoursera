## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
## NOTE: Do not round the result!

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## Get the file names from the directory
  fileNames <- list.files(directory)
  
  ## Initialize a vector to hold the values
  values <- c()
  
  ## Loop through the id values
  for (i in id) {
    ## Create a file name using the current id
    fileName <- sprintf("%03d.csv", i)
    
    ## Put together the file path
    filePath <- paste(directory, fileName, sep = "/")
    
    ## Get the data out of the file
    data <- read.csv(filePath)
    
    ## Get the column we want
    pollutantData <- data[,pollutant]
    
    ## Remove NA values
    pollutantData <- pollutantData[!is.na(pollutantData)]
    
    ## Add values to the previously created vector
    values <- c(values, pollutantData)
  }
  
  ## Get the mean
  mean(values)
}