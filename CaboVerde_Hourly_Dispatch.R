## load libraries
load_install<-function(lib){
  if(! require(lib, character.only=TRUE)) install.packages(lib, character.only=TRUE)
  library(lib, character.only=TRUE)
}

## the required libraries (e.g. packages)
Thelib<-c("knitr", "xlsx", "plyr", "ggplot2", "scales", "gdata", "chron", "reshape2", "grid", "hydroTSM")

## apply the function
lapply(Thelib, load_install)

## read Cape Verde Hourly demand data
for(i in 1:12){
  hourly<-read.xlsx(file="~/github/ECREEE/CaboVerde_Santiago_Hourly_Dispatch.xlsx",
                    sheetIndex=i,
                    as.data.frame=TRUE,
                    header=FALSE,
                    check.names=TRUE,
                    rowIndex=7:30,
                    colIndex=2:32
  )

  # remove columns containing "NA"
  for(i in 1:dim(hourly)[2]){
    if(all(is.na(hourly[,i])))
      if(i==1){drop<-i} else
        drop<-c(drop,i)
  }

  keep<-hourly[,-c(drop)]
  colnames(keep)<-1:dim(keep)[2]
  keep$hour<-1:24

  # flatten the array
  test<-melt(keep, na.rm=FALSE, id.vars="hour")
  test$month<-i
  if(i==1){dat<-test} else
    dat<-rbind(dat,test)
}