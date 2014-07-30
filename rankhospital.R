rankhospital <- function(state, outcome,num="best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    
    frameSupplied <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeCol <- list("heart attack" = 11, "heart failure"=17,"pneumonia" = 23)
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
    #print (df[,4])
    df <- data.frame(df, ot = as.numeric(df[,4]))
    
    rankedRow <- numeric()
    if(num=="best"){
        orderedData <- df[order(df$ot, df$Hospital.Name), ]
        rankedRow <- orderedData[1,]
        'order.outcome <- order(df[,5])
        orderedData <- df[order.outcome,]'
        
    } else if (num=="worst") {
        orderedData <- df[order(-df$ot, df$Hospital.Name), ]
        rankedRow <- orderedData[1,]
    } else if (num > nrow(df)) {
        return (NA)
    } else{
        orderedData <- df[order(df$ot, df$Hospital.Name), ]
        rankedRow <- orderedData[num,]
       
                
    }
        
   
    return (rankedRow$Hospital.Name)
    
'
    order.outcome <- order(df[,5])
    
    orderedData <- df[order.outcome,]
    print(orderedData)
    
    lowest <- orderedData[1,]
    bestHospital <- lowest$Hospital.Name
    print(bestHospital)
    '
    
    
}
