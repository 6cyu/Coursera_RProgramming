corr <- function(directory, threshold = 0){
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        source("complete.R")
        obs = complete(directory,1:332)
        
        # finding the index of data whose number of complete observation
        # is greater than the given threshold
        id_threshold <- obs$id[obs$nobs>threshold]

        # computing the correlation of the thresholded data
        polCor = rep(0,length(id_threshold))
        count <- 1
        for(i in id_threshold){
                # getting polution data
                # formatting id (adding "0" or "00" in front of id)
                id_format <- formatC(i,width=3,flag = "0")
                fileName <- paste(directory,id_format,sep = "/")
                fullName <- paste(fileName,"csv", sep = ".")
                fullPolData <- read.csv(fullName)
                
                # removing NA values
                good <- complete.cases(fullPolData)
                polData <- fullPolData[good,]
                
                polCor[count] <- cor(polData$sulfate,polData$nitrate)
                count <- count + 1
                }        
        polCor
}