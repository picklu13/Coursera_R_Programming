pollutantmean <- function (directory, pollutant, id = 1:332) {
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
  
  
  meanVec <- numeric()
  dat <- data.frame()
  for (filename in fullPathList ){
      dat <- rbind(dat, read.csv(filename))
  }
 
  polldata <- dat[pollutant]
  good <- complete.cases(polldata)
  goodData <- polldata[good,]
 
  meanOfPoll <- mean(goodData)
  id<- meanOfPoll
  
  return (id)

}

