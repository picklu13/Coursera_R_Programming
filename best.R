best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    frameSupplied <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeCol <- list("heart attack" = 13, "heart failure"=19,"pneumonia" = 25)
    cols <- c(1,2,7)
    
    validOutcomeList <- c("heart attack","heart failure","pneumonia")
    outcomeColumnNum <- outcomeCol[[outcome]]
    cols <- c(cols,outcomeColumnNum)
    filteredData <- frameSupplied[,cols]
    validStateList <- levels(factor(filteredData$State))
    if (!(state %in% validStateList))
    {
        stop("invalid state")
    }
    if (!(outcome %in% validOutcomeList)){
        stop("invalid outcome")
    }
    
    fcs <- (factor(filteredData$State))
    grouped <- split(filteredData,filteredData$State)
    stateWiseGrouped<-(grouped[state])
    df <- (stateWiseGrouped[[state]])
    print (df[,4])
    df <- data.frame(df, ot = as.numeric(df[,4]))
    order.outcome <- order(df[,5])
    print(min(df[,5], na.rm = TRUE )   )
    print (df)
    orderedData <- df[order.outcome,]
    print(orderedData)
    lowest <- orderedData[1,]
    bestHospital <- lowest$Hospital.Name
    print(bestHospital)
        

    
}
