rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!(state %in% outcomes$State)) { stop("invalid state")}
  if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){ stop("invalid outcome")}
  # define our numbers here for use later
  if(num == "best") { num <- 1 }

  if(outcome == "heart attack") {cause <- 11} # the columns
  if(outcome == "heart failure") {cause <- 17} # in our dataset
  if(outcome == "pneumonia") {cause <- 23}      # 
  
  ## cut our big data table down to size with only the essential bits  
  statedata <- subset(outcomes, State == state)
  statetab <- cbind(statedata[,2],statedata[,cause])
  suppressWarnings(statetab <- statetab[!is.na(as.numeric(statetab[,2])),])
  if(num == "worst") {num <- nrow(statetab)}  

  # rank by alpha
  statetab <- statetab[order(statetab[,1]),]

  # rank by outcome
  rankd <- statetab[order(as.numeric(statetab[,2]), decreasing = FALSE),]

  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  rankd[num]
}