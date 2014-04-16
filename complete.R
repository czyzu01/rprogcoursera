complete <- function(directory, id = 1:332) {
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
    
    id_vect <- vector()    
    nobs_vect <- numeric()
    files <- dir(directory)[id]
    for (monitor in 1:length(id)) {
        df <- read.csv(paste(directory, files[monitor], sep="/"))
        nobs_vect <- c(nobs_vect, nrow(df[complete.cases(df),]))
        id_vect <- c(id_vect, id[monitor])
    }
    df <- data.frame(id_vect, nobs_vect)
    names(df)[1]<-"id"
    names(df)[2]<-"nobs"
    df
}