setwd('C:/Users/Anant Sharma/Desktop/DATA SCIENCE/Practice/Big mart sales')

##Loading the data

train<-read.csv("Train_UWu5bXk.txt")
test<-read.csv("Test_u94Q5KV.txt")

##Combining both train and test data

test$Item_Outlet_Sales<-NA
total<-rbind(train,test)

##Imputing NA values
total$Outlet_Size[total$Outlet_Size==""]<-NA

## 1) Imputing NA values in item_weight with mean with respect to item_type.
means<-tapply(total$Item_Weight,total$Item_Type,mean,na.rm=TRUE)
means.dataframe<-data.frame(item_type=row.names(means),mean_item_weight=means)
row.names(means.dataframe)<-NULL

for(i in 1:nrow(means.dataframe))
{
  total$Item_Weight[total$Item_Type==means.dataframe[i,1] & is.na(total$Item_Weight)]<-means.dataframe[i,2]
}

## 2) Imputing NA values in Outlet_size with mode with respect to outlet_type

##Creating a mode function
getmode <- function(v) {
  v<-v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_outlet_size<-tapply(total$Outlet_Size,total$Outlet_Type,getmode)

##we found that every Grocery Store, Supermarket Type1 will be imputed with "small" and  Supermarket Type2, Supermarket Type3 with "medium"

total$Outlet_Size[is.na(total$Outlet_Size) & total$Outlet_Type=="Grocery Store"]<-"Small"
total$Outlet_Size[is.na(total$Outlet_Size) & total$Outlet_Type=="Supermarket Type1"]<-"Small"
total$Outlet_Size[is.na(total$Outlet_Size) & total$Outlet_Type=="Supermarket Type2"]<-"Medium"
total$Outlet_Size[is.na(total$Outlet_Size) & total$Outlet_Type=="Supermarket Type3"]<-"Medium"


##Feature Engineering

##1) Modifying item_visibility

##some products had item_visibility as zero which is not possible hence we need treat them as NA values and substitute them with mean item_visibilty of that preoduct

total$Item_Visibility[total$Item_Visibility==0]<-NA

means_visibility<-tapply(total$Item_Visibility,total$Item_Identifier,mean,na.rm=TRUE)
means_dataframe<-data.frame(Item_Identifier=row.names(means_visibility),means_visibility=means_visibility)
row.names(means_dataframe)<-NULL

for(i in 1:nrow(means_dataframe))
{
  total$Item_Visibility[total$Item_Identifier==means_dataframe[i,1] & is.na(total$Item_Visibility)]<-means_dataframe[i,2]
}

##2) Creating a new feature item_type_combined

##Item_identifiers starting with "FD" are food
##Item_identifiers starting with "DR" are drinks
##Item_identifiers starting with "NC" are Non consumable

total$item_type_combined[grepl("FD",total$Item_Identifier)]<-"Food"
total$item_type_combined[grepl("DR",total$Item_Identifier)]<-"Drinks"
total$item_type_combined[grepl("NC",total$Item_Identifier)]<-"Non-Consumable"
total$item_type_combined<-factor(total$item_type_combined,levels=c("Food","Drinks","Non-Consumable"))

##3) A new feature specifying years of operation of a store

total$Outlet_Years<-2013-total$Outlet_Establishment_Year


##4) Fat content features were mismatched

total$Item_Fat_Content[total$Item_Fat_Content=="LF"]<-"Low Fat"
total$Item_Fat_Content[total$Item_Fat_Content=="low fat"]<-"Low Fat"
total$Item_Fat_Content[total$Item_Fat_Content=="reg"]<-"Regular"

##fat content of non consumable products is meaningless so we define a new category "Non-Edible"

total$Item_Fat_Content<-factor(total$Item_Fat_Content,levels=c("Low Fat","Regular","Non-Edible"))
total$Item_Fat_Content[total$item_type_combined=="Non-Consumable"]<-"Non-Edible"

##making outlet_size levels correct

total$Outlet_Size<-factor(total$Outlet_Size,levels=c("High","Medium","Small"))


##making a new feature Item_Visibility_MeanRatio

for(i in 1:nrow(means_dataframe))
{
  total$Item_Visibility_MeanRatio[total$Item_Identifier==means_dataframe[i,1]]<-total$Item_Visibility/means_dataframe[i,2]
}

##Splitting data into train and test

process_train<-total[1:8523,]
process_test<-total[8524:14204,]

##Applying model

library(caret)
fit<-train(Item_Outlet_Sales~Outlet_Years+Outlet_Size+Item_MRP,data=process_train,method="gbm",verbose=FALSE)
pre<-predict(fit,process_test)
finalsub<-data.frame(Item_Identifier=process_test$Item_Identifier,Outlet_Identifier=process_test$Outlet_Identifier,Item_Outlet_Sales=pre)


write.csv(finalsub,file="finalsub.csv",row.names=FALSE)