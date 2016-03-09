pollutantmean <- function(directory, pollutant, id = 1:332) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     d <- list.files(directory, full.names=TRUE)
     poldat <- data.frame()
     for (i in 1:332) {
          poldat <- rbind(poldat, read.csv(d[i]))
     }
     
     p <- pollutant
     
     IDsub <- poldat[which(poldat[, "ID"] %in% id),]
     
     mean(IDsub[, p], na.rm=TRUE)
     
}