best <- function(state, outcome) {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    ### Check for valid state
    valid_states<-levels(factor(data[,7]))
    if (! state %in% valid_states) {
        stop("invalid state")
}
    ## Return hospital name in that state with lowest 30-day death
    hospitals<-subset(data, data$State==state, 
                      select=c(Hospital.Name, 
                             Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                             Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
                             Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
                             )
                      )
    
    if (outcome == "heart attack") {
        hospitals[,2]<-as.numeric(hospitals[, 2])
        column<-hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    } else if (outcome == "heart failure") {
        hospitals[,3]<-as.numeric(hospitals[, 3])
        column<-hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    } else if (outcome == "pneumonia") {
        hospitals[,4]<-as.numeric(hospitals[, 4])
        column<-hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    } else {
        stop("invalid outcome")
    }    
    hospitals_sorted<-hospitals[with(hospitals, order(column, Hospital.Name)), ]
    hospitals_sorted[1,1]
    ## rate
    
}