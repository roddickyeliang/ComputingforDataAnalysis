corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  numOfFiles <- length(list.files(directory))
  vec <- c()
  
  monsToCorr <- subset(complete(directory,1:numOfFiles), nobs > threshold)[1][,1]
  
  formatMon <- function(i,directory) {
    subset(getmonitor(i,directory), sulfate!="NA" & nitrate!="NA", select = c(sulfate,nitrate))
  } 
   
  for(i in monsToCorr) {
    monitor <- formatMon(i,directory)
    vec <- c(vec,cor(monitor)[2])
  }
  
  return(vec)
}