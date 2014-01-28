rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- data$State
  
  if(!state %in% states) {
    stop("invalid state")
  }
  
  if(!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(outcome == "heart attack") i <- 11
  if(outcome == "heart failure") i <- 17
  if(outcome == "pneumonia") i <- 23
  
  stateData <- subset(data, data$State == state)
  good <- !is.na(stateData[,i])
  stateData <- stateData[good,]
  
  stateData[,i] <- as.numeric(stateData[,i])
  stateData <- stateData[with(stateData,order(stateData[,i],stateData$Hospital.Name)),]
  
  rowNums <- as.integer(nrow(stateData))
  
  if(num == "best") rank <- which.min(stateData[,i])
  if(num == "worst") rank <- which.max(stateData[,i])
  if(!(num == "best" || num == "worst")) rank <- as.integer(num)
  
  if(rank > rowNums) return(NA)
  
  return(stateData[rank,]$Hospital.Name)
}