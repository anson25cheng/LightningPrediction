#combine two datasets based on year, month, day, hour.
library(data.table)
setwd("D:\\Hill Thesis\\Thesis Data\\RCode\\CSVData")

#read in the three datasets

FieldMill<-fread("FieldMillWide.csv")
FieldMillMissing<-fread("FieldMillWideMissing.csv")
ObsData<-fread("ObsVar_99percent.csv")
LightningStrike<-fread("LightningStrikeDataFull.csv")
LightningStrike201307<-readRDS("LightningStrike201307.rds")

#merge the three datasets
tempCombined1<-merge(FieldMill,ObsData,all=TRUE)
tempCombined2<-merge(tempCombined1,FieldMillMissing,all = TRUE)
CombinedDataSet<-merge(tempCombined2,LightningStrike,all = TRUE)
CombinedDataSet<-CombinedDataSet[!duplicated(CombinedDataSet),]
CombinedDataSet<-subset(CombinedDataSet,CombinedDataSet$Hour!=24)

Data<-CombinedDataSet
count =1
for (i in 1489:2232){
  if (Data$Day[i] == LightningStrike201307$Day[count]){
    if(Data$Hour[i] == LightningStrike201307$Hour[count]){
      Data$Strike[i]<-LightningStrike201307$Strike[count]
      count = count+1
    }
  }
  
}


saveRDS(Data,"CombinedDataSet.rds")
write.csv(Data,"CombinedDataSet.csv")

#this was a relatively easy way to combine different data sets with similar time steps. 
#make sure to sanity check things
