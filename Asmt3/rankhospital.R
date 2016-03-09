rankhospital <- function(state, outcome, num)  {
      outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
      outsub <- outcomes[, c(7,2,11,17,23)]
      colnames(outsub) <- c("State","Hospital","attack", "failure", "pneumonia")
      
      ## Check that state and outcome are valid
      
      if(!is.element(state, unique(outcomes$State))) stop("invalid state")
      if(!is.element(outcome, c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
      
      
      ## Return hospital name in that state with the given rank 30-day death rate
      if (outcome == "heart attack") {
            a <- 3
            } else if (outcome == "heart failure") { 
                  a <- 4
            } else {a <- 5}      
      
      nonas <- (outsub[complete.cases(as.numeric(outsub[,a])),])
      outstate <- subset(nonas, State == state)
                  
      ordered <- outstate[order(as.numeric(outstate[,a]),outstate[,2]),]
      
      if (num == "best") {
            b <- 1
      }     else if (num == "worst") {
            q <- which(ordered == max(as.numeric(ordered[,a])), arr.ind=TRUE)
            b <- q[1,1]
      }     else {b <- num} 
      
      hospital <- ordered[b,2]
      hospital
      
}