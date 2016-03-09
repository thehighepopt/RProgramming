complete <- function(directory, id = 1:332) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      d <- list.files(directory, full.names=TRUE)
      poldat <- data.frame()
      for (i in 1:332) {
            poldat <- rbind(poldat, read.csv(d[i]))
            
      }
      
      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used
                  
      ## where 'id' is the monitor ID number and 'nobs' is the
      ## number of complete cases
      
      outputdf <- data.frame()
      for (i in id) {
           tmp <- subset(poldat, ID == i)
           nobs <- sum(complete.cases(tmp))
           outputdf <- rbind(outputdf, data.frame(i,nobs)) 
           
      }
      colnames(outputdf) <- c("id", "nobs")       
      outputdf
      
}