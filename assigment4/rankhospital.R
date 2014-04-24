rankhospital <- function(state, outcome, num="best") {
    
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
        col_index <- 2
    } else if (outcome == "heart failure") {
        col_index <- 3
    } else if (outcome == "pneumonia") {
        col_index <- 4
    } else {
        stop("invalid outcome")
    }    
    hospitals[,col_index]<-as.numeric(hospitals[, col_index])
    hospitals<-hospitals[complete.cases(hospitals[,col_index]),]
    
    column<-hospitals[,col_index]
    hospitals_sorted<-hospitals[with(hospitals, order(column, hospitals$Hospital.Name)), ]
    if (num=="best") {
        num=1
    } else if (num=="worst") {
        num=nrow(hospitals_sorted)
    } else if (!is.numeric(num)) {
        stop("invalid num")
    }
    num<-as.numeric(num)
    
    hospitals_sorted[num,1]
    ## rate
    
}