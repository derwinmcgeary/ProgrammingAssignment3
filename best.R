best <- function(state, outcome, datafile="outcome-of-care-measures.csv") {
  ## read outcome data
  outcomes <- read.csv(datafile, colClasses = "character")
  
  ## Check that state and outcome are OK
  if(!(state %in% outcomes$State)) { stop("invalid state")}
  if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){ stop("invalid outcome")}

  ## variable "cause" is a column number in a table we construct later...
  ## ...there must be a better way...
  if(outcome == "heart attack") {cause <- 2}
  if(outcome == "heart failure") {cause <- 3}
  if(outcome == "pneumonia") {cause <- 4}
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  statedata <- subset(outcomes,State == state)
  stateoutcomes <- cbind(statedata[,2],statedata[,11],statedata[,17],statedata[,23])

  colnames(stateoutcomes) <- c("Hospital","heart attack", "heart failure", "pneumonia")

  
  
  # Now we have a nice matrix with the data we care about
  # We need to find the minimum value, discarding NAs ...
  suppressWarnings(minharm <- range(as.numeric(stateoutcomes[,cause]), na.rm=TRUE)[1])

  # ... and put all rows with that value into bests
  suppressWarnings(bests <- as.numeric(stateoutcomes[,cause]) == minharm)
  bests <- which(as.logical(bests))

  # ... and return stateoutcomes[bests,1]
  stateoutcomes[[bests,1]]
}