pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    df <- data.frame()
    pollutant_vect <- vector()
    for (file in dir(directory)[id]) {
        df <- read.csv(paste(directory, file, sep="/"))
        pollutant_vect <- c (pollutant_vect, df[,pollutant])
    }
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
        
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    mean(pollutant_vect, na.rm=TRUE)
}