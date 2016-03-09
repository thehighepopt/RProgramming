rankall <- function(outcome, num = "best") {
      outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
      outsub <- outcomes[, c(7,2,11,17,23)]
      colnames(outsub) <- c("state","hospital","attack", "failure", "pneumonia")
      
      ## Check that state and outcome are valid
      
      #if(!is.element(state, unique(outcomes$State))) stop("invalid state")
      if(!is.element(outcome, c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
      
      
      ## For each state, find the hospital of the given rank
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
      if (outcome == "heart attack") {
            a <- 3
      } else if (outcome == "heart failure") { 
            a <- 4
      } else {a <- 5}      
      
      nonas <- (outsub[complete.cases(as.numeric(outsub[,a])),])
            
      ordered <- nonas[order(nonas[,1],as.numeric(nonas[,a]),nonas[,2]),]
      
      byState <- split(ordered, ordered$state)
      
      if (num == "best") {
            b <- do.call("rbind", lapply(byState, function(x) x[1,2:1]))
      }     else if (num == "worst") {
                b <- do.call("rbind", lapply(byState, function(x) x[nrow(x),2:1]))
          }     else {b <- do.call("rbind", lapply(byState, function(x) x[num,2:1]))
               }
      
      b
      
}
