rankall <- function(outcome, num = "best", datafile="outcome-of-care-measures.csv") {
  ## Read outcome data
  outcomes <- read.csv(datafile, colClasses = "character", stringsAsFactors = FALSE)
  
  ## Check that state and outcome are valid
  if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){ stop("invalid outcome")}
######################## Set up some variables #############################
  if(outcome == "heart attack") {cause <- 11} # the columns
  if(outcome == "heart failure") {cause <- 17} # in our dataset
  if(outcome == "pneumonia") {cause <- 23}      # 
  if(num == "best") { num <- 1 } # best is rank number one, of course
  worst <- FALSE
  if(num == "worst") {            # We don't know what rank is worst...
    worst <- TRUE                 # ... it depends on how many hospitals
    num <- 1                      # So we reverse the sort order and return number 1
  }

  ## Subset to the columns we care about
  allhosps <- cbind(outcomes[,2], outcomes[,7],outcomes[,cause])
  colnames(allhosps) <- c("hospital","state","deaths")
  suppressWarnings(allhosps[,3] <- as.numeric(as.character(allhosps[,3])))
  allhosps <- allhosps[!is.na(allhosps[,3]),]
  
  # list them by state
  statelst <- unique(allhosps[,2]) # vector of state codes in the file
  statelist <- statelst[order(statelst)] # in alphabetic order
  x <- split(as.data.frame(allhosps),allhosps[,2])

## For each state, find the hospital of the given rank

  output <- data.frame(hospital = rep(as.character(NA),length(statelist)), state = statelist, row.names = statelist, stringsAsFactors = FALSE)

############################# The main loop ############################
  for(sta in statelist) {

    st <- x[[sta]] ## table corresponding to the State
    st$deaths <- as.numeric(as.character(st$deaths)) # We need numeric, not alphabetic sort
    
    if(nrow(st)==0) {next()}      # nothing there? move on!

    if(num > nrow(st)) { # there aren't enough hospitals with data in this state
      output[sta,] <- c(NA,sta) # to give us an answer
      next()                            # so mark it as NA and move on
    } 

    # rank by alpha
    st <- st[order(st[,1]),]

    # rank by outcome
    if(!worst) {
    rankd <- st[order(st$deaths),] } else {
    rankd <- st[order(st$deaths, decreasing = TRUE),]
    }

    result <- rankd[num,1:2]
    joy <- as.vector(result[[1]])
    output[sta,1] <- joy
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  output
}