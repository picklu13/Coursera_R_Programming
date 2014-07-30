source("complete.R")
corr <- function(directory, threshold = 0) {
    
    completeDataFrame <- complete(directory)
    thresholdFiltered <- subset(completeDataFrame,  completeDataFrame[,"nobs"] > threshold)
    corVec <- numeric()
    
    if(nrow(thresholdFiltered) > 0){
        csvList <- character()
        idList <- thresholdFiltered[,"id"] 
        for (i in idList) {
            if (i <10 )
            {
                fileName <- paste("00",i,".csv",sep = "")
                csvList <- append(csvList,fileName)
            }else if(i>=10 & i<100)
            {
                fileName <- paste(c("0"),i,".csv",sep = "")
                csvList <- append(csvList,fileName)
            }
            else{ 
                fileName <- paste(i,".csv",sep = "")
                csvList <- append(csvList,fileName) 
                
            }
        }
        
        fullPathList <- paste(directory,"/",csvList,sep="")
        
        for ( file in fullPathList) {
            df <- read.csv(file)
            
            good <- complete.cases(df)
            goodData <- df[good,]
            sulfates <- goodData[,"sulfate"]
            nitrates <- goodData[,"nitrate"]
            coRelation <- cor(sulfates,nitrates)
            
            corVec<-append(corVec,coRelation)
            
        }
    }
    
    return(corVec)
    
}