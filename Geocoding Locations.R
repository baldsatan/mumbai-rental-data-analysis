path<-"D:/Pratik Das/Machine Learning/Mumbai Rental Data"
setwd(path)
library(readr)
dataset <- read_csv("16-Jul-99acres-mumbai-rental-data.csv")

locations<-paste(dataset$Society,dataset$Location,sep = " ,")
geo_data<-as.data.frame(locations)

library(ggmap)
for (i in 1:length(locations)) {
  latlon = geocode(as.character(locations[i]),override_limit = T)
  geo_data$Lon[i] = as.numeric(latlon[1])
  geo_data$Lat[i] = as.numeric(latlon[2])
}

First_List<-geo_data[is.na(geo_data$Lon)==F,]
Sec_Iter<-geo_data[is.na(geo_data$Lon),]

for (i in 1:length(Sec_Iter)) {
  latlon = geocode(as.character(Sec_Iter$locations[i]),override_limit = T)
  Sec_Iter$Lon[i] = as.numeric(latlon[1])
  Sec_Iter$Lat[i] = as.numeric(latlon[2])
}

Second_List<-Sec_Iter[is.na(Sec_Iter$Lon)==F,]
Third_Iter<-Sec_Iter[is.na(Sec_Iter$Lon),]

for (i in 1:length(Third_Iter)) {
  latlon = geocode(as.character(Third_Iter$locations[i]),override_limit = T)
  Third_Iter$Lon[i] = as.numeric(latlon[1])
  Third_Iter$Lat[i] = as.numeric(latlon[2])
}

Third_List<-Third_Iter[is.na(Third_Iter$Lon)==F,]
Fourth_Iter<-Third_Iter[is.na(Third_Iter$Lon),]

for (i in 1:length(Fourth_Iter)) {
  latlon = geocode(as.character(Fourth_Iter$locations[i]),override_limit = T)
  Fourth_Iter$Lon[i] = as.numeric(latlon[1])
  Fourth_Iter$Lat[i] = as.numeric(latlon[2])
}

Fourth_List<-Fourth_Iter

locations<-rbind(rbind(rbind(First_List,Second_List),Third_List),Fourth_List)

Abnormal_Values<-locations[locations$Lon>=74|locations$Lon<=70|is.na(locations$Lon),]

for (i in 1:length(Abnormal_Values)) {
  latlon = geocode(as.character(Abnormal_Values[i,1]),override_limit = T)
  Abnormal_Values$Lon[i] = as.numeric(latlon[1])
  Abnormal_Values$Lat[i] = as.numeric(latlon[2])
}

locations<-locations[!(locations$locations %in% Abnormal_Values$locations),]
Retreived_Values<-Abnormal_Values[Abnormal_Values$Lon<=74&Abnormal_Values$Lon>=70&is.na(Abnormal_Values$Lon)==F,]
Abnormal_Values<-Abnormal_Values[Abnormal_Values$Lon>=74|Abnormal_Values$Lon<=70|is.na(Abnormal_Values$Lon)==T,]
locations<-rbind(locations,Retreived_Values)

for (i in 1:length(Abnormal_Values)) {
  latlon = geocode(as.character(Abnormal_Values[i,1]),override_limit = T)
  Abnormal_Values$Lon[i] = as.numeric(latlon[1])
  Abnormal_Values$Lat[i] = as.numeric(latlon[2])
}
