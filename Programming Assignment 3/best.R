best <- function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if(outcome == "heart attack") i <- 11
  if(outcome == "heart failure") i <- 17
  if(outcome == "pneumonia") i <- 23
  
  stateData <- subset(data, data$State == state)
  good <- !is.na(stateData[,i])
  stateData <- stateData[good,]
  
  stateData[,i] <- as.numeric(stateData[,i])
  minIndex <- which.min(stateData[,i])
  
  return(stateData[minIndex,]$Hospital.Name)
}

