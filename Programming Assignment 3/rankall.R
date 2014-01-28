rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid  
  if(!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name  
  state <- data$State
  state <- sort(unique(state))
  hospital <- rep("", length(state))
  if(outcome == "heart attack") i <- 11
  if(outcome == "heart failure") i <- 17
  if(outcome == "pneumonia") i <- 23
  
  for(index in 1:length(state)) {
    stateData <- subset(data, data$State == state[index])
    good <- !is.na(stateData[,i])
    stateData <- stateData[good,]
    
    stateData[,i] <- as.numeric(stateData[,i])
    stateData <- stateData[with(stateData,order(stateData[,i],stateData$Hospital.Name)),]
    
    rowNums <- as.integer(nrow(stateData))
    
    if(num == "best") rank <- which.min(stateData[,i])
    if(num == "worst") rank <- which.max(stateData[,i])
    if(!(num == "best" || num == "worst")) rank <- as.integer(num)
    
    if(rank > rowNums) {
      hospital[index] <- NA
    } else {
      hospital[index] <- stateData[rank,]$Hospital.Name
    } 
  }
  
  return(data.frame(hospital, state))
}