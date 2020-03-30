###build dataset
rm(list=ls())
gc()
load("~/finaldata_1.RData")
load("~/finaldata_2.RData")
load("~/finaldata_3.RData")
load("~/finaldata_4.RData")
load("~/finaldata_5.RData")
load("~/finaldata_6.RData")
load("~/finaldata_7.RData")
load("~/finaldata_8.RData")
load("~/finaldata_9.RData")
load("~/finaldata_10.RData")
load("~/finaldata_11.RData")
load("~/finaldata_12.RData")
load("~/finaldata_13.RData")
load("~/finaldata_14.RData")
load("~/finaldata_15.RData")
load("~/finaldata_16.RData")
load("~/finaldata_17.RData")
load("~/finaldata_18.RData")
load("~/finaldata_19.RData")
load("~/finaldata_20.RData")
load("~/finaldata_21.RData")
load("~/finaldata_22.RData")
load("~/finaldata_23.RData")
load("~/finaldata_24.RData")
load("~/finaldata_25.RData")
load("~/finaldata_26.RData")
load("~/finaldata_27.RData")
load("~/finaldata_28.RData")
load("~/finaldata_29.RData")
load("~/finaldata_30.RData")
load("~/finaldata_31.RData")
load("~/finaldata_32.RData")
load("~/finaldata_33.RData")
load("~/finaldata_34.RData")
load("~/finaldata_35.RData")
load("~/finaldata_36.RData")
load("~/finaldata_37.RData")
load("~/finaldata_38.RData")
load("~/finaldata_39.RData")
load("~/finaldata_40.RData")
load("~/finaldata_41.RData")
load("~/finaldata_42.RData")
load("~/finaldata_43.RData")
load("~/finaldata_44.RData")
load("~/finaldata_45.RData")
load("~/finaldata_46.RData")
load("~/finaldata_47.RData")
load("~/finaldata_48.RData")
load("~/finaldata_49.RData")
load("~/finaldata_50.RData")
load("~/finaldata_51.RData")
load("~/finaldata_52.RData")
load("~/finaldata_53.RData")
load("~/finaldata_54.RData")
load("~/finaldata_55.RData")
load("~/finaldata_56.RData")
load("~/finaldata_57.RData")
load("~/finaldata_58.RData")
load("~/finaldata_59.RData")
load("~/finaldata_60.RData")
load("~/finaldata_61.RData")
load("~/finaldata_62.RData")
load("~/finaldata_63.RData")
load("~/finaldata_64.RData")
load("~/finaldata_65.RData")
load("~/finaldata_66.RData")
load("~/finaldata_67.RData")
load("~/finaldata_68.RData")
load("~/finaldata_70.RData")
load("~/finaldata_71.RData")
load("~/finaldata_72.RData")
load("~/finaldata_73.RData")
load("~/finaldata_74.RData")
load("~/finaldata_75.RData")
load("~/finaldata_76.RData")
load("~/finaldata_77.RData")
load("~/finaldata_78.RData")
load("~/finaldata_79.RData")
load("~/finaldata_80.RData")
load("~/finaldata_81.RData")
load("~/finaldata_82.RData")
load("~/finaldata_83.RData")
load("~/finaldata_84.RData")
load("~/finaldata_85.RData")


list_files = list(finaldata_1,finaldata_2,finaldata_3,finaldata_4,finaldata_5,finaldata_6,finaldata_7,
                  finaldata_8,finaldata_9,finaldata_10,finaldata_11,finaldata_12,finaldata_13,finaldata_14,
                  finaldata_15,finaldata_16,finaldata_17,finaldata_18,finaldata_19,finaldata_20,finaldata_21,finaldata_22,
                  finaldata_23,finaldata_24,finaldata_25,finaldata_26,finaldata_27,finaldata_28,finaldata_29,finaldata_30,
                  finaldata_31,finaldata_32, finaldata_33,finaldata_34,finaldata_35,finaldata_36,finaldata_37,finaldata_38,finaldata_39,
                  finaldata_40,finaldata_41,finaldata_42,finaldata_43,finaldata_44,finaldata_45,finaldata_46,finaldata_47,
                  finaldata_48,finaldata_49,finaldata_50,finaldata_51,finaldata_52,finaldata_53,finaldata_54,finaldata_55,
                  finaldata_56,finaldata_57,finaldata_58,finaldata_59,finaldata_60,finaldata_61,finaldata_62,finaldata_63,
                  finaldata_64,finaldata_65,finaldata_66)

list_files = list(finaldata_34,finaldata_35,finaldata_36,finaldata_37,finaldata_38,finaldata_39,
                  finaldata_40,finaldata_41,finaldata_42,finaldata_43,finaldata_44,finaldata_45,finaldata_46,finaldata_47,
                  finaldata_48,finaldata_49,finaldata_50,finaldata_51,finaldata_52,finaldata_53,finaldata_54,finaldata_55,
                  finaldata_56,finaldata_57,finaldata_58,finaldata_59,finaldata_60,finaldata_61,finaldata_62,finaldata_63,
                  finaldata_64,finaldata_65,finaldata_66,finaldata_67,finaldata_68,finaldata_70,finaldata_71,finaldata_72,finaldata_73,
                  finaldata_74,finaldata_75,finaldata_76,finaldata_77,finaldata_78,finaldata_79,finaldata_80,finaldata_81, finaldata_82,
                  finaldata_83, finaldata_84, finaldata_85)


names_main = colnames(data_all)


for(i in 1:length(list_files)){
  print('-------------')
  print(colnames(list_files[[i]]))
  names_i = colnames(list_files[[i]])
  numbers_i =  as.numeric(substring(names_i, 4,5)[-1])
  missing_names = setdiff(names_main,names_i)
  print(missing_names)
  # part where columns are missing
  if(length(missing_names)!=0){
    missing_numbers = as.numeric(substring(missing_names, 4,5))
    print(missing_numbers)
    if(length( which(numbers_i > missing_numbers))>3){
      replacement  = which(numbers_i > missing_numbers)[c(1,4)] + 1
      for (x in 1:length(missing_names) ){
        list_files[[i]] = cbind( list_files[[i]], list_files[[i]][,replacement[x]])
        colnames(list_files[[i]])[length(list_files[[i]])] = missing_names[x]
      }
      list_files[[i]] = list_files[[i]][,names_main]
    }else{
      replacement  = which(numbers_i < missing_numbers)[c(length( which(numbers_i < missing_numbers))-3,length( which(numbers_i < missing_numbers)))] + 1
      for (x in 1:length(missing_names) ){
        list_files[[i]] = cbind( list_files[[i]], list_files[[i]][,replacement[x]])
        colnames(list_files[[i]])[length(list_files[[i]])] = missing_names[x]
      }
      list_files[[i]] = list_files[[i]][,names_main]
    }
    ## part with all columns included
  }else{
    list_files[[i]] = list_files[[i]][,names_main]
  }
}

## join the files
for(i in 1:length(list_files)){
  data_all = rbind(data_all,list_files[[i]])
}




#####
final_names = c()
for(i in 1:length(list_files)){
  print(colnames(list_files[[i]]))
  if(i == 1){
    final_names = colnames(list_files[[i]])
  }else{
    final_names = intersect(final_names, colnames(list_files[[i]]))
  }
}
print(final_names)
data_all = c()
for(i in 1:length(list_files)){
  data_all = rbind(data_all,list_files[[i]][,final_names] )
}
save(data_all, file = "final_all.RData")

########
hour_of_day <- c()
hours <- c()
for (i in 1:21){
  for (j in 1:5){
    n<-60
    hours <- rep(0:23, each = n)
    hours <- as.data.frame(hours)
    hour_of_day <- rbind(hour_of_day,hours)
  }
}
data_all <- cbind(data_all,hour_of_day)
############
rm(list=ls())


epoch_time <- data_all[,1]
data_all <- data_all[,-1]
column = data_all$KSC01_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,2] <-column2

column = data_all$KSC02_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,4] <-column2

column = data_all$KSC04_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,6] <-column2

column = data_all$KSC05_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,8] <-column2

column = data_all$KSC06_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,10] <-column2

column = data_all$KSC07_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,12] <-column2

column = data_all$KSC08_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,14] <-column2

column = data_all$KSC09_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,16] <-column2

column = data_all$KSC11_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,18] <-column2

column = data_all$KSC12_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,20] <-column2

column = data_all$KSC13_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,22] <-column2

column = data_all$KSC15_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,24] <-column2

column = data_all$KSC16_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,26] <-column2

column = data_all$KSC17_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,28] <-column2

column = data_all$KSC18_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,30] <-column2

column = data_all$KSC19_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,32] <-column2

column = data_all$KSC20_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,34] <-column2

column = data_all$KSC21_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,36] <-column2

column = data_all$KSC22_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,38] <-column2

column = data_all$KSC24_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,40] <-column2

column = data_all$KSC27_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,42] <-column2

column = data_all$KSC28_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,44] <-column2

column = data_all$KSC29_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,46] <-column2

column = data_all$KSC30_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,48] <-column2

column = data_all$KSC32_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,50] <-column2

column = data_all$KSC34_onset
column2 = as.data.frame(column)

for(i in  1:(nrow(column2)-1)){
  if(  column2$column[i]>0  ){
    column2$column[i+1] = column2$column[i] -1
  }
}
data_all[,52] <-column2

hold_data <- data_all

library(dplyr)
column = data_all$KSC01_onset
column2=as.data.frame(column)
column2 = column2 %>% ## name of the dataframe
  mutate( newc = ## name of the new column to be added
            case_when(
              column == 0 ~ "A",
              column >0 & column <=15 ~ "B",
              column >15 & column <=30 ~ "C",
              column >30  ~ "D"
            )
  )

column2[,2] <- as.factor(column2[,2])
newc <- column2$newc
newc <- as.data.frame(newc)
epoch_time <- as.data.frame(epoch_time)
data_all <-cbind(data_all[,1:52],newc,epoch_time)

table(data_all$newc)
data_all<-data_all[,1:53]

library(UBL)
set.seed(123)
smoteif_data <- SmoteClassif(newc~.,data_all, C.perc="balance",k=15,repl=FALSE,dist="Euclidean",p=1)
table(smoteif_data$newc)
save(smoteif_data,file="smote_final.RData")

smoteif_data <- smoteif_data[order(smoteif_data[,54]),]
smoteif_data$newc <- NULL


