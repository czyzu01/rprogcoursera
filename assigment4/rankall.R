rankPerState <- function(hospitals, num="best") {
    ## assumes order of  Hospital.Name, State, data
    ## sort by data 
    hospitals_sorted<-hospitals[with(hospitals, order(hospitals[3], hospitals$Hospital.Name)), ]
    if (num=="best") {
        num=1
    } else if (num=="worst") {
        num=nrow(hospitals_sorted)
    } else if (!is.numeric(num)) {
        stop("invalid num")
    }
    num<-as.numeric(num)
       
    
    if (num<=nrow(hospitals_sorted)){
        c(hospital=hospitals_sorted[num,1],state=hospitals_sorted[num,2])
##        hospitals_sorted[num,1]
    } else {
        c(hospital=NA, state=hospitals_sorted[1,2])
##        NA
}
}


rankall <- function(outcome, num="best") {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check conditions
    if (outcome == "heart attack") {
        col_index <- 11
    } else if (outcome == "heart failure") {
        col_index <- 17
    } else if (outcome == "pneumonia") {
        col_index <- 23
    } else {
        stop("invalid outcome")
    }    
    
    ## Subselect only interesting data.
    hospitals_all<-data[, c(2, 7, col_index)]
    
    ## get only complete cases for selected column
    hospitals_all[,3]<-as.numeric(hospitals_all[, 3])
    hospitals_all<-hospitals_all[complete.cases(hospitals_all[,3]),]

    ## split data over states.
    hospitals_list<-split(hospitals_all, factor(hospitals_all[,2]))
    ## apply ranking
    ranked_hospitals<-lapply(hospitals_list, rankPerState, num)
    ## convert back to data.frame
    ## by first combining to matrix
    matrix_for_data_frame<-matrix(unlist(ranked_hospitals), ncol=2, byrow=T)
    ## and converting matrix to data.frame
    result<-data.frame(matrix_for_data_frame, row.names=matrix_for_data_frame[,2])
    colnames(result) <- c("hospital", "state")
    result
}