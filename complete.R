complete <- function(directory, id = 1:332) {

    csvList <- character()
    for (i in id  ) {
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
    df<-data.frame()
    for (filename in fullPathList ){
        dat <- read.csv(filename)
        good <- complete.cases(dat)
        goodData <- dat[good,]
       
        numRows <-  (nrow(goodData))
        specID <- goodData[1,4]
        
        
        df<-rbind(df,c(specID , numRows))
        
    
    }
    
    
    colnames(df) <- c("id","nobs")
   
    'print(df)'
    return(df)
    
    
    
}