#converting meters from KSCS to lat long
StrikeDateData[,2] <- sapply(StrikeDateData[,2],as.integer)
StrikeDateData2 <-  StrikeDateData

 latitude.conv <- function(x){28.538486 + ((x/1000)/6378) *(180/pi)
 }
  longitude.conv <- function(y){-80.642633 + ((y/1000)/6378) *(180/pi)/cos(28.538486*pi/180)
  }

StrikeDateData2[,2] <- lapply(StrikeDateData[,3],latitude.conv)
StrikeDateData2[,3] <- lapply(StrikeDateData[,2],longitude.conv)

StrikeDateData2 <- data.frame(cbind(StrikeDateData2[,2:3],StrikeDateData[,5]))
save(StrikeDateData2, file = "LDAR_Final.RData")

###Read in data

rm(list = ls(all.names = TRUE))
gc()
library(data.table)
library(plyr)
library(tidyverse)
root = "/home/dom/Desktop/ChengThesis/ThesisData/EFMData"
dirs_lvl1 <- list.dirs(root, recursive = F)
save.EFM_Data<-data.table(day = character(), time = character(), voltage = integer(), location=character(),timestamp=character())

for(i in dirs_lvl1[84]) {
  dirs_lvl2 <- list.dirs(i, recursive = F)
  for(j in dirs_lvl2) {
    dirs_lvl3 <- list.dirs(j, recursive = F)
    for(k in dirs_lvl3) {
      EFM_Data<-data.table(day = character(), time = character(), voltage = integer(), location=character(),timestamp=character())
      data.files <- list.files(k, 
                               pattern = '(.txt|.RAW)',
                               full.names = T)
      
      for(l in 1:length(data.files)){
        if(file.info(data.files[1])$size != 0){
          EFM_Data[,c(1:3)] <- data.table::fread(data.files[1])
          EFM_Data$location <- substr(basename(data.files[1]), start = 1, stop = 5)
          EFM_Data$timestamp <- basename(dirname(data.files[1]))
        }
      }
      
      #This will pull in the data from one timestamp
      #will create a data table with date,time,voltage,location,and timestamp (in 30min increments)
      for(l in 1:length(data.files)) {
        
        if(file.info(data.files[l])$size == 0) next
        this.data <- try(fread(data.files[l]), silent = T)
        names(this.data)[1] <- "day"
        names(this.data)[2] <- "time"
        names(this.data)[3] <- "voltage"
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
save(save.EFM_Data, file = "efmtest84.RData")


#converting day number to a date
rm(list=ls())
gc()
load("~/efmtest85.RData")
library(lubridate)
year <- as.numeric(substring(save.EFM_Data[1:156729712,1],1,4))
day <- as.numeric(substring(save.EFM_Data[1:156729712,1],5,7))
df <- data.frame(cbind(year,day))
df$origin <-as.Date(paste0(df$year, "-01-01"),tz="UTC")-days(1)
df_new <- as.Date(df$day,origin = df$origin, tz ="UTC")
epoch_time1<-as.data.frame(difftime(as.POSIXct(paste(df_new,save.EFM_Data[1:156729712,2]),format = "%Y-%m-%d %H:%M:%S"), as.Date("2013-01-01"), units= "mins"),stringsAsFactors=FALSE)

year <- as.numeric(substring(save.EFM_Data[156729713:313459425,1],1,4))
day <- as.numeric(substring(save.EFM_Data[156729713:313459425,1],5,7))
df <- data.frame(cbind(year,day))
df$origin <-as.Date(paste0(df$year, "-01-01"),tz="UTC")-days(1)
df_new <- as.Date(df$day,origin = df$origin, tz ="UTC")
epoch_time2<-as.data.frame(difftime(as.POSIXct(paste(df_new,save.EFM_Data[156729713:313459425,2]),format = "%Y-%m-%d %H:%M:%S"), as.Date("2013-01-01"), units= "mins"),stringsAsFactors=FALSE)

#################
rows <- nrow(save.EFM_Data)
year <- as.numeric(substring(save.EFM_Data[313459426:rows,1],1,4))
day <- as.numeric(substring(save.EFM_Data[313459426:rows,1],5,7))
df <- data.frame(cbind(year,day))
df$origin <-as.Date(paste0(df$year, "-01-01"),tz="UTC")-days(1)
df_new <- as.Date(df$day,origin = df$origin, tz ="UTC")
epoch_time3<-as.data.frame(difftime(as.POSIXct(paste(df_new,save.EFM_Data[313459426:rows,2]),format = "%Y-%m-%d %H:%M:%S"), as.Date("2013-01-01"), units= "mins"),stringsAsFactors=FALSE)

rows <- nrow(epoch_time3)
final_epoch<-data.frame(a=c(epoch_time1[1:156729712,],epoch_time2[1:156729713,],epoch_time3[1:rows,]))

#################
year <- as.numeric(substring(save.EFM_Data[313459426:470189138,1],1,4))
day <- as.numeric(substring(save.EFM_Data[313459426:470189138,1],5,7))
df <- data.frame(cbind(year,day))
df$origin <-as.Date(paste0(df$year, "-01-01"),tz="UTC")-days(1)
df_new <- as.Date(df$day,origin = df$origin, tz ="UTC")
epoch_time3<-as.data.frame(difftime(as.POSIXct(paste(df_new,save.EFM_Data[313459426:470189138,2]),format = "%Y-%m-%d %H:%M:%S"), as.Date("2013-01-01"), units= "mins"),stringsAsFactors=FALSE)

rows <- nrow(save.EFM_Data)
year <- as.numeric(substring(save.EFM_Data[470189139:rows,1],1,4))
day <- as.numeric(substring(save.EFM_Data[470189139:rows,1],5,7))
df <- data.frame(cbind(year,day))
df$origin <-as.Date(paste0(df$year, "-01-01"),tz="UTC")-days(1)
df_new <- as.Date(df$day,origin = df$origin, tz ="UTC")
epoch_time4<-as.data.frame(difftime(as.POSIXct(paste(df_new,save.EFM_Data[470189139:rows,2]),format = "%Y-%m-%d %H:%M:%S"), as.Date("2013-01-01"), units= "mins"),stringsAsFactors=FALSE)

rows <- nrow(epoch_time4)
final_epoch<-data.frame(a=c(epoch_time1[1:156729712,],epoch_time2[1:156729713,],epoch_time3[1:156729713,],epoch_time4[1:rows,]))


final_efm<-data.frame(cbind(save.EFM_Data[,3],save.EFM_Data[,4]))
final_efm$epochtime <- as.data.frame(final_epoch)
final_efm$X1 = as.numeric(as.character(final_efm$X1))
final_efm$epochtime = as.numeric(floor(unlist(final_efm$epochtime)))


library(dplyr)
efm_avg = final_efm %>%
  group_by(X2,epochtime) %>%
  summarise(avg = mean(X1))
save(efm_avg, file = "efm_avg.RData")

rm(list=ls())
gc()

load("~/efm_avg.RData")
load("~/LDAR_Final.RData")
############################################KSC01
library(data.table)
library(gmt)
#calculate distance between KSC01 and all lightning strikes in nautical miles
KSC01_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.7036,279.3314,units="nm"))
#add a second column that is the epoch time of all strikes and their respective nautical miles from KSC01
KSC01_geodist[,2] <- StrikeDateData2[,3]
KSC01_geodist[,2] <- (KSC01_geodist[,2]/60)-22616640
KSC01_geodist[,2] <- as.numeric(floor(KSC01_geodist[,2]))
#KSC01_geodist[,2] <- KSC01_geodist[order(KSC01_geodist[,2]),]
#subset data with only nautical miles less than or equal to 5
test<- subset(KSC01_geodist, KSC01_geodist[,1] <= 5)
#order the epoch times from beginning to end
# test <- test[order(test[,2]),]
# test[,2] <- test[,2]/60
# test[,2] <- test[,2]-22616640
# test[,2] <- as.numeric(floor(test$V2))

#take the differences between each lightning strike
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
#subset if the difference in lightning strikes is greater than or equal to 900 seconds (storms)
storms <- test[(test[,3]>=15),]
#create start stop times
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
#remove duplicates
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
#add the 30 minute window forecast
test<-test[,1:2]
data <- test[,2]
#loop through checking which interval of lightning strikes a reading occured in
startstop = as.matrix(startstop)


data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
library(dplyr)
data = data %>%
  group_by(V1) %>%
    summarise(V2 = mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC01_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC01_clean <-as.data.frame(KSC01_clean)
KSC01_cleanavg = KSC01_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))

seq_ksc = seq(min(KSC01_geodist$V2),max(KSC01_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC01_cleanavg$epochtime_strike = as.numeric(KSC01_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC01_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
#KSC01_cleanavg = KSC01_cleanavg %>% full_join(seq_ksc, by = c('epochtime_strike' = "seq_ksc"))
#seq_ksc[which(!is.na(seq_ksc$avg_time_end)),]
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)
KSC01 <- subset(efm_avg, efm_avg[,1]=='KSC01')
if (dim(KSC01)[1]==0){
  KSC01<-subset(efm_avg,efm_avg[,1]=='KSC02')
}


df = KSC01 %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp)
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}

finaldata = df[,2:3]
names(finaldata) = c("epochtime","KSC01_voltage")


##CHANGE
#+60
w = 145*60*5*24

seq_ksc = seq_ksc[-1:-w,]
finaldata$KSC01_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata <- as.data.frame(finaldata)


############################################KSC02
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.6875,279.2803,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC02')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC04')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC02_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC02_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)


############################################KSC04
#calculate distance between KSC01 and all lightning strikes in nautical miles
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.66433,279.3608,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC04')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC05')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC04_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC04_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)


############################################KSC05
#calculate distance between KSC01 and all lightning strikes in nautical miles
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.65818,279.3006,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC05')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC06')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC05_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC05_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC06
#calculate distance between KSC01 and all lightning strikes in nautical miles
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.64397,279.3336,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC06')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC07')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC06_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC06_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC07
#calculate distance between KSC01 and all lightning strikes in nautical miles
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.64238,279.25222,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC07')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC08')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC07_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC07_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC08
#calculate distance between KSC01 and all lightning strikes in nautical miles
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.6389,279.3775,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC08')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC09')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC08_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC08_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)


############################################KSC09
#calculate distance between KSC01 and all lightning strikes in nautical miles
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.6214,279.3919,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC09')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC10')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC09_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC09_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)



############################################KSC10
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.6241,279.29861,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC10')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC11')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC10_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC10_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC11
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.6058,279.3261,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC11')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC12')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC11_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC11_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)


############################################KSC12
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.6023,279.3592,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC12')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC13')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC12_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC12_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)


############################################KSC13
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.602,279.411,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC13')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC15')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC13_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC13_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC15
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.577,279.3569,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC15')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC16')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC15_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC15_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)


############################################KSC16
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.5753,279.427,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC16')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC17')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC16_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC16_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC17
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.562,279.33,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC17')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC18')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC17_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC17_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC18
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.555,279.298,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC18')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC19')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC18_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC18_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC19
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.549,279.37972,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC19')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC20')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC19_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC19_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC20
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.5417,279.35556,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC20')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC21')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC20_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC20_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC21
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.5258,279.37778,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC21')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC22')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC21_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC21_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC22
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.507,279.306,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC22')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC24')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC22_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC22_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC24
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.47677,279.3253,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC24')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC25')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC24_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC24_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC25
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.42474,279.3361,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC25')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC26')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC25_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC25_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC26
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.5494,279.4331,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC26')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC27')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

names(finaldata2) = c("KSC26_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC26_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)
############################################KSC27
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.5031,279.44194,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC27')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC28')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]


names(finaldata2) = c("KSC27_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC27_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC28
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.46024,279.4728,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC28')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC29')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC28_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC28_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC29
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.4626,279.416389,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC29')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC30')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC29_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC29_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC30
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.4751117,279.44167,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC30')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC32')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC30_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC30_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC31
#KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.42033,279.408889,units="nm"))

############################################KSC32
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.4369,279.42,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC32')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC34')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]

###
names(finaldata2) = c("KSC32_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC32_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

############################################KSC34
KSC_geodist <- as.data.frame(geodist(StrikeDateData2[,1],StrikeDateData2[,2],28.4498,279.436389,units="nm"))
KSC_geodist[,2] <- StrikeDateData2[,3]
KSC_geodist[,2] <- (KSC_geodist[,2]/60) - 22616640
KSC_geodist[,2] <- as.numeric(floor(KSC_geodist[,2]))
test<- subset(KSC_geodist, KSC_geodist[,1] <= 5)
test[,3] <-c(NA, diff(test$V2,lag=1,differences = 1))
storms <- test[(test[,3]>=15),]
endtime <- as.data.frame(storms[,2]-storms[,3])
endtime <- as.data.frame(endtime[-1,])
endtime <- rbind(endtime,tail(test,1)[,2])
starttime<-as.data.frame(storms[,2])
starttime[1,] <- test[1,2]
startstop <- cbind(starttime,endtime)
startstop <- startstop[!(startstop[,1]==startstop[,2]),]
test<-test[,1:2]
data <- test[,2]
startstop = as.matrix(startstop)
data = as.matrix(data)
data = cbind(data,0)
data <- as.data.frame(data)
data = data %>%
  group_by(V1) %>%
  summarise(V2=mean(V2))
data = as.matrix(data)
for (i in 1:nrow(data)){
  check = F
  for (j in 1:nrow(startstop)){
    if(data[i]+30>=startstop[j,1] && data[i]+30<=startstop[j,2] && check == FALSE){
      check = TRUE
      data[i,2] = startstop[j,2]-data[i]
    }
  }
}
#create dataframe of 30 minute forecast
library(tidyverse)
library(dplyr)
data <- data.frame(data)
test <- distinct(test,V2,.keep_all=TRUE)
nm <- test[,1]
epochtime_strike<-test[,2]
time_to_end <- data[,2]
KSC_clean <- as.data.frame(cbind(epochtime_strike,time_to_end))
KSC_clean <-as.data.frame(KSC_clean)
KSC_cleanavg = KSC_clean %>% 
  group_by(epochtime_strike) %>% 
  summarise(avg_time_end = mean(time_to_end))
seq_ksc = seq(min(KSC_geodist$V2),max(KSC_geodist$V2),1)
seq_ksc = tibble(seq_ksc)
KSC_cleanavg$epochtime_strike = as.numeric(KSC_cleanavg$epochtime_strike)
seq_ksc = seq_ksc %>% left_join(KSC_cleanavg, by = c( "seq_ksc" = 'epochtime_strike' ))
library(xts)
seq_ksc = seq_ksc %>% mutate(n=avg_time_end) %>% fill(n)
seq_ksc = as.matrix(seq_ksc)
for(i in 2:nrow(seq_ksc)){
  t=F
  if(is.na(seq_ksc[i,2])){
    seq_ksc[i,3] = seq_ksc[i-1,3]-1
  }
}
seq_ksc[,3][seq_ksc[,3]<0]=0
seq_ksc <- as.data.frame(seq_ksc)
library(imputeTS)
seq_ksc[,3] <- na_replace(seq_ksc[,3],0)
seq_ksc <- cbind(seq_ksc$seq_ksc,seq_ksc$n)
seq_ksc <- as.data.frame(seq_ksc)

KSC <- subset(efm_avg, efm_avg[,1]=='KSC34')
if (dim(KSC)[1]==0){
  KSC<-subset(efm_avg,efm_avg[,1]=='KSC32')
}
df = KSC %>%ungroup()
gaps = df$epochtime-lag(df$epochtime)
gaps = which(gaps!=1)
df1 = c()
for(i in gaps){
  df_temp = data.frame(
    df$X2[1],
    seq(df$epochtime[i-1]+1,df$epochtime[i]-1, by  = 1),
    mean(df$avg[i-1],df$avg[i])
  )
  colnames(df_temp) = colnames(df)
  
  df1 = rbind(df1,df_temp )
}
df = df %>% bind_rows(df1) %>% arrange(epochtime)

library(dplyr)
library(tibble)
df  = df %>% mutate(rowid = row_number())
check = 24*60*5
seq_check = seq(1,check,1)
if(nrow(df)<check){
  seq_to_add = which(!seq_check %in% df$rowid)
  seq_epoch_add = seq(1, length(which(!seq_check %in% df$rowid)),1)
  seq_epoch_add = seq_epoch_add + df$epochtime[length(df$epochtime)]
  df_add = data.frame(seq_to_add)
  df_add$X2 = df$X2[1]
  df_add$epochtime = seq_epoch_add
  df_add$avg = df$avg[length(df$avg)]
  colnames(df_add)[1] = 'rowid'
  df_add = df_add %>% select(X2,epochtime,avg,rowid)
  df = df %>% bind_rows(df_add)
}
finaldata2 = df[,3]


names(finaldata2) = c("KSC34_voltage")
seq_ksc = seq_ksc[-1:-w,]
seq_ksc<-as.data.frame(seq_ksc)

finaldata2$KSC34_onset <- seq_ksc[1:length(df$epochtime),2]
finaldata2 <- as.data.frame(finaldata2)
finaldata <- cbind(finaldata,finaldata2)

######change
finaldata_85 <- finaldata
save(finaldata_85, file = "finaldata_85.RData")
