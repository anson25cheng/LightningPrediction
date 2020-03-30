library(data.table)
library(plyr)
library(tidyverse)
root = "/home/dom/Desktop/ChengThesis/ThesisData/Unprocessed EFM Data"
dirs_lvl1 <- list.dirs(root, recursive = F)
save.EFM_Data<-data.table(day = character(), time = character(), voltage = integer(), location=character(),timestamp=character())

for(i in dirs_lvl1) {
  
  dirs_lvl2 <- list.dirs(i, recursive = F)
  
for(j in dirs_lvl2) {
  
  dirs_lvl3 <- list.dirs(j, recursive = F)
  
for(k in dirs_lvl3) {
  EFM_Data<-data.table(day = character(), time = character(), voltage = integer(), location=character(),timestamp=character())
  data.files <- list.files(k, 
                           pattern = '(.txt|.raw)',
                           full.names = T)
  if(file.info(data.files[1])$size != 0){
  EFM_Data[,c(1:3)] <- data.table::fread(data.files[1])
  EFM_Data$location <- substr(basename(data.files[1]), start = 1, stop = 5)
  EFM_Data$timestamp <- basename(dirname(data.files[1]))
  }
  #This will pull in the data from one timestamp
  #will create a data table with date,time,voltage,location,and timestamp (in 30min increments)
  for(l in 2:length(data.files)) {
   
   if(file.info(data.files[l])$size == 0) next
  this.data <- try(fread(data.files[l]), silent = T)
   this.data$location <- substr(basename(data.files[l]), start = 1, stop = 5)
   this.data$timestamp <-EFM_Data$timestamp
    
    EFM_Data <- rbind(EFM_Data,this.data)
     
   }
  
  setkeyv(EFM_Data, c('location', 'timestamp'))
  temp.data <- ddply(EFM_Data,.(location,timestamp))
  names(temp.data)[1] <- "day"
  names(temp.data)[2] <- "time"
  names(temp.data)[3] <- "voltage"
  save.EFM_Data<-rbind(save.EFM_Data,temp.data)
  
  ##################################
  #move onto next timestamp in the day
}
  #move onto next day in series of 5 days
}
  #Move onto next group of dates.
  
}

