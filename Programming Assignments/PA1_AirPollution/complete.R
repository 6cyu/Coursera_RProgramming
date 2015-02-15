complete <-function(directory, idx=1:332){
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        polData2 = NULL
        nobs_val = rep(0,length(idx))
        count = 1
        for(i in idx){
                # getting polution data
                # formatting id (adding "0" or "00" in front of id)
                id_format <- formatC(i,width=3,flag = "0")
                fileName <- paste(directory,id_format,sep = "/")
                fullName <- paste(fileName,"csv", sep = ".")
                fullPolData <- read.csv(fullName)

                # removing NA values
                good <- complete.cases(fullPolData)
                polData <- fullPolData[good,]
                nobs_val[count] <- length(polData$ID)
                count <- count + 1
        }
        
        data.frame(id = idx, nobs = nobs_val )        
}