best <- function(state, outcome) {
  ## read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!(state %in% outcomes$State)) { stop("invalid state")}
  if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){ stop("invalid outcome")}
    
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  statedata <- subset(outcomes,State == state)
  stateoutcomes <- cbind(statedata[,2],statedata[,11],statedata[,17],statedata[,23])
  
}