rankall <- function(outcome, num = "best", datafile="outcome-of-care-measures.csv") {
  ## Read outcome data
  outcomes <- read.csv(datafile, colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){ stop("invalid outcome")}
  if(outcome == "heart attack") {cause <- 11} # the columns
  if(outcome == "heart failure") {cause <- 17} # in our dataset
  if(outcome == "pneumonia") {cause <- 23}      # 
  if(num == "best") { num <- 1 }
  
  ## Subset to the columns we care about
  allhosps <- cbind(cbind(outcomes[,2], outcomes[,7],outcomes[,cause]))
  colnames(allhosps) <- c("hospital","state","deaths")
  suppressWarnings(allhosps <- allhosps[!is.na(as.numeric(allhosps[,3])),])
  
  # list them by state
  x <- split(as.data.frame(allhosps),allhosps[,2])
  ## For each state, find the hospital of the given rank

  output <- NULL

  for(st in x) {
    
    st <- as.matrix(st)
    if(num == "worst") {num <- nrow(st)}
    if(num > nrow(st)) {output <- c(output,c(NA,st[1]))
    print(c(NA,st[2]))
    next()
    }

    # rank by alpha
    st <- st[order(st[,1]),]
    
    # rank by outcome
    rankd <- st[order(as.numeric(st[,3]), decreasing = FALSE),]
    print(rankd[num])
    output <- c(output,c(rankd[num],st[2]))
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  as.data.frame(output)
}