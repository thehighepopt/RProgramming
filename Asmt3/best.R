best <- function (state, outcome) {
      ## Read outcome data
      outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
      outsub <- outcomes[, c(7,2,11,17,23)]
      colnames(outsub) <- c("State","Hospital","attack", "failure", "pneumonia")
            
      ## Check that state and outcome are valid
      
      if(!is.element(state, unique(outcomes$State))) stop("invalid state")
      if(!is.element(outcome, c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
      
      
      ## Return hospital name in that state with lowest 30-day death rate
      if (outcome == "heart attack") {
            a <- 3
      } else if (outcome == "heart failure") { 
            a <- 4
      }      else {a <- 5}
      
      nonas <- (outsub[complete.cases(as.numeric(outsub[,a])),])
      outstate <- subset(nonas, State == state)
      outmin <- subset(outstate, as.numeric(outstate[,a]) == min(as.numeric(outstate[,a])))
      ordered <- outmin[order(outmin[,a]),]
            
      hospital <- ordered[1,2]
      hospital
     
}