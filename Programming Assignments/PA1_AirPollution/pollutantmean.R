pollutantmean <- function(directory, pollutant, id = 1:332){
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        polData2 = NULL
        for(i in id){
                # getting polution data
                # formatting id (adding "0" or "00" in front of id)
                id_format <- formatC(i,width=3,flag = "0")
                fileName <- paste(directory,id_format,sep = "/")
                fullName <- paste(fileName,"csv", sep = ".")
                fullPolData <- read.csv(fullName)
                
                # subsetting data based on pollutant
                polData <- subset(fullPolData,select = pollutant)
                polData2 <- rbind(polData2,polData)
        }
        
        # removing NA values
        good <- complete.cases(polData2)
        polData3 <- polData2[good,]
        mean(polData3)
    
}