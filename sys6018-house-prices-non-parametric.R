################################################################################
# sys6018-house-prices-non-parametric.R
# House Prices Kaggle Competition
# September 18, 2018
# Team C1-3
################################################################################
library(readr)  
library(dplyr)
library(sjPlot)
library(beepr)

#######Import data & Setup
train = read_csv("train.csv")
test = read_csv("test.csv")
str(train)
str(test)
test$SalePrice = NA
housing <- rbind(train, test)

#######Data exploration
plot(housing$SalePrice)
plot(housing$OverallQual, housing$SalePrice)
plot(housing$MiscVal, housing$SalePrice)

#######Data cleaning (missing values, outliers, etc.)
###Missing Values
colSums(is.na(housing))
housing[is.na(housing$Alley), "Alley"]<-"No Alley"
housing[is.na(housing$BsmtQual), "BsmtQual"]<-"No Basement"
housing[is.na(housing$BsmtCond), "BsmtCond"]<-"No Basement"
housing[is.na(housing$BsmtExposure), "BsmtExposure"]<-"No Basement"
housing[is.na(housing$BsmtFinType2), "BsmtFinType2"]<-"No Basement"
housing[is.na(housing$BsmtFullBath ), "BsmtFullBath"]<-0
housing[is.na(housing$BsmtHalfBath ), "BsmtHalfBath"]<-0
housing[is.na(housing$BsmtFinType1), "BsmtFinType1"]<-"No Basement"
housing[is.na(housing$Functional), "Functional"]<-"Typical Functionality"
housing[is.na(housing$FireplaceQu), "FireplaceQu"]<-"No fireplace"
housing[is.na(housing$GarageType), "GarageType"]<-"No garage"
housing[is.na(housing$GarageYrBlt & housing$GarageType == "No garage"), "GarageYrBlt"]<-"No garage"
housing[is.na(housing$GarageFinish), "GarageFinish"]<-"No garage"
housing[is.na(housing$GarageQual), "GarageQual"]<-"No garage"
housing[is.na(housing$GarageCond), "GarageCond"]<-"No garage"
housing[is.na(housing$PoolQC), "PoolQC"]<-"No pool"
housing[is.na(housing$Fence), "Fence"]<-"No Fence"
housing[is.na(housing$MiscFeature), "MiscFeature"]<-"None"
colSums(is.na(housing))

###Outliers
##Did nothing with outliers, will control with this during selection of k

###Other
##Converted all non numeric variables to variables, then filled in the rest of 
##the NAs with the median, also to control for outliers
housing<- data.frame(lapply(housing, function(x) { if(is.character(x)) as.integer(as.factor(x)) else x }))
str(housing)
namean=function(x){
  x[is.na(x)] =median(x, na.rm=TRUE) 
  x
}
housing = data.frame(apply(housing,2,namean))
colSums(is.na(housing))

###Feature Engineering (total number of baths and total living area)
housing$Totlivingarea <- housing$X1stFlrSF+ housing$X2ndFlrSF+ housing$GrLivArea
housing$Bath <- housing$BsmtHalfBath*0.5 + housing$BsmtFullBath +housing$FullBath + housing$HalfBath*0.5

#######Rationale for the selected statistical modeling methods
###Correlation
round(cor(housing),2)
##Chose important variables and those representing a correlation group
# Keeping only OverallQual, YearBuilt, YearRemodAdd, ExterQual, BsmtQual, Bath, TotalBsmtSF ,  KitchenQual, 
#GarageArea, Totlivingarea 
housing <- housing %>% 
              dplyr::select(Id,OverallQual, YearBuilt, YearRemodAdd, ExterQual, BsmtQual, TotalBsmtSF , Bath, KitchenQual, GarageArea, Totlivingarea, SalePrice)

#######Implementation
###Getting Data Ready for the Model
##Split back into training and testing
train= housing[1:1460,]
test= housing[1461:2919,]

##Removing ID Column, saving testing's for later submission
train <- train[-1]
testID <- test[1]
test <- test[-1]
test$SalePrice <- NULL

#Scaling Each Data Set
test <- data.frame(scale(test))
SalePrice = train[ncol(train)]
train <- data.frame(scale(train[,-ncol(train)]))
train <- as.data.frame(train)
train[,"SalePrice"] <- SalePrice

summary(train)
summary(test)

#Euclidean Distance
edistance <- function(testobj, trainobj)
{
  difference = 0
  for(i in c(1:(length(testobj)-1) ))
  {
    difference = difference + (testobj[[i]]-trainobj[[i]])^2
  }
  difference = sqrt(difference)
  return(difference)
}

#kNN function, runs through each test object, calculates the distance for each of its predictors, 
#then averages the sale price for the top 5 training objects, ranked by shortest distance
kNN <- function(test, train, k){
  pred <- c()
  counter = 0 
  for(i in c(1:nrow(test)))
  {   
    distance =c()        
    saleprices = c()
    for(j in c(1:nrow(train)))
    {
      distance <- c(distance, edistance(test[i,], train[j,]))
      saleprices <- c(saleprices, train[j,ncol(train)])
    }
    neighbors <- data.frame(saleprices, distance)
    neighbors <- neighbors[order(neighbors$distance),]       
    neighbors <- neighbors[1:k,]
    pred <- c(pred, weighted.mean(neighbors[,"saleprices"], neighbors[,"distance"]))
    counter = counter + 1
    print(counter)
  }
  return(pred)
}

#######Model Selection Approach
###Testing Sum of Squares Elbow Criterion
df = train[-ncol(train)]
sjc.elbow(df)
##From graph, 6 is a good number, will also reduce outlier consideration
##We didnt do cross validation (because we had to use kNN) but tried a bunch of different numbers 
#for K and saw that 6 was the best
predictions <- kNN(test, train, 6) 
knnpredictions <- data.frame("Id" = testID, "SalePrice" = predictions)
write.table(knnpredictions, file = "C1:3-Non parametric.csv", row.names=FALSE, col.names=c("Id", "SalePrice"), sep=",")
##Code makes a sound when its over because it takes a very long time (-___-)
beep()
