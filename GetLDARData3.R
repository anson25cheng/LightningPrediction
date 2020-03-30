
library(data.table)
library(plyr)
library(foreach)
#clears all the variables being used
DateData<-data.table(Year = numeric(),Month = numeric(),Day = numeric(),Hour = numeric(),Strike= numeric())
tempDate<-data.table(Year = numeric(),Month = numeric(),Day = numeric(),Hour = numeric(),Strike= numeric())
StrikeDateData = NULL
#change your root to what you are working in.  
root = "/home/dom/Desktop/ChengThesis/ThesisData/LDAR"
dirs_lvl1 <- list.dirs(root, recursive = F)
test=NULL

#for to go through each year of data
for (j in 1:length(dirs_lvl1)){
  #gets the list of all txt files in each year
  data.files <- list.files(dirs_lvl1[j], 
                           pattern = '(.txt|.RAW|.raw)',
                           full.names = T)
  foreach(l = 5:9,.export = 'fread') %do% {
    
    if(file.info(data.files[l])$size == 0){
      test = NULL
    } 
    else {
      #read in the data
      test<- try(fread(data.files[l],fill=TRUE),silent = TRUE)
      test<-na.omit(test)
      
      #get the month, day, year, and hour for the data point. 
      Month<-as.numeric(substring(test[1,1],1,2))
      Day<-as.numeric(substring(test[1,1],4,5))
      Year<-as.numeric(substring(test[1,1],7,10))
      Hour<-as.numeric(substring(test[1,1],12,13))
      tempDate<-data.table(Year = Year, Month = Month, Day = Day, Hour = Hour, Strike=1)
      StrikeDateData<-rbind(StrikeDateData,test)
      test = NULL
      return(tempDate)
    }
    
  }
  #stores the data. Make sure to check and save in increments
  #StrikeDateData<-rbind(StrikeDateData, DateData)
  DateDate = NULL
  
}
 

save(StrikeDateData, file="LDAR.RData")


