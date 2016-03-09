corr <- function(directory, threshold = 0) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     d <- list.files(directory, full.names=TRUE)
     poldat <- data.frame()
     for (i in 1:332) {
          poldat <- rbind(poldat, read.csv(d[i]))
              
     }
     
     dfover <- subset(complete(directory, 1:332) , nobs >= threshold, select = id)
     over_thresh <- dfover[,1]
     corrdat <- subset(poldat[complete.cases(poldat),], ID %in% over_thresh)
     correl <- tapply(rownames(corrdat), corrdat$ID, function(r) with(corrdat[r, ], cor(sulfate, nitrate))) 
     corr <- as.vector(correl)
     }