################################################################################
# sys6018-house-prices-parametric.R
# House Prices Kaggle Competition
# September 18, 2018
# Team C1-3
################################################################################

library(tidyverse)

#######Import data 
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

#######Adding an null column to test dataset to facilitate row-bind
test$SalePrice = NA
housing <- rbind(train, test)

#######Data cleaning (missing values, outliers, etc.)
###Missing Values
housing[is.na(housing$MSZoning), "MSZoning"]<-housing %>% group_by(MSZoning) %>% summarise(n = n()) %>% filter(n == max(n)) %>% pull(MSZoning)
housing[is.na(housing$LotFrontage), "LotFrontage"]<-median(housing$LotFrontage, na.rm = T)
housing[is.na(housing$Alley), "Alley"]<-"No Alley"
housing[is.na(housing$Utilities), "Utilities"]<-housing %>% group_by(Utilities) %>% summarise(n = n()) %>% filter(n == max(n)) %>% pull(Utilities)
housing[is.na(housing$Exterior1st), "Exterior1st"]<-housing %>% group_by(Exterior1st) %>% summarise(n = n()) %>% filter(n == max(n)) %>% pull(Exterior1st)
housing[is.na(housing$Exterior2nd), "Exterior2nd"]<-housing %>% group_by(Exterior2nd) %>% summarise(n = n()) %>% filter(n == max(n)) %>% pull(Exterior2nd)
housing[is.na(housing$MasVnrArea), "MasVnrArea"]<-housing %>% group_by(MasVnrArea) %>% summarise(n = n()) %>% filter(n == max(n)) %>% pull(MasVnrArea)
housing[is.na(housing$BsmtQual), "BsmtQual"]<-"No Basement"
housing[is.na(housing$BsmtCond), "BsmtCond"]<-"No Basement"
housing[is.na(housing$BsmtExposure), "BsmtExposure"]<-"No Basement"
housing[is.na(housing$BsmtFinType1), "BsmtFinType1"]<-"No Basement"
housing[is.na(housing$BsmtFinSF1), "BsmtFinSF1"]<-median(housing$BsmtFinSF1, na.rm = T)
housing[is.na(housing$BsmtFinType2), "BsmtFinType2"]<-"No Basement"
housing[is.na(housing$BsmtFinSF2), "BsmtFinSF2"]<-median(housing$BsmtFinSF2, na.rm = T)
housing[is.na(housing$BsmtUnfSF), "BsmtUnfSF"]<-median(housing$BsmtUnfSF, na.rm = T)
housing[is.na(housing$TotalBsmtSF), "TotalBsmtSF"]<-median(housing$TotalBsmtSF, na.rm = T)
housing[is.na(housing$Electrical), "Electrical"]<-housing %>% group_by(Electrical) %>% summarise(n = n()) %>% filter(n == max(n)) %>% pull(Electrical)
housing[is.na(housing$BsmtFullBath ), "BsmtFullBath"]<-0
housing[is.na(housing$BsmtHalfBath ), "BsmtHalfBath"]<-0
housing[is.na(housing$KitchenQual), "KitchenQual"]<-housing %>% group_by(KitchenQual) %>% summarise(n = n()) %>% filter(n == max(n)) %>% pull(KitchenQual)
housing[is.na(housing$Functional), "Functional"]<-"Typical Functionality"
housing[is.na(housing$FireplaceQu), "FireplaceQu"]<-"No fireplace"
housing[is.na(housing$GarageType), "GarageType"]<-"No garage"
housing[is.na(housing$GarageYrBlt & housing$GarageType == "No garage"), "GarageYrBlt"]<-"No garage"
housing[is.na(housing$GarageFinish), "GarageFinish"]<-"No garage"
housing[is.na(housing$GarageCars), "GarageCars"]<-median(housing$GarageCars, na.rm = T)
housing[is.na(housing$GarageArea), "GarageArea"]<-median(housing$GarageArea, na.rm = T)
housing[is.na(housing$GarageQual), "GarageQual"]<-"No garage"
housing[is.na(housing$GarageCond), "GarageCond"]<-"No garage"
housing[is.na(housing$PoolQC), "PoolQC"]<-"No pool"
housing[is.na(housing$Fence), "Fence"]<-"No Fence"
housing[is.na(housing$MiscFeature), "MiscFeature"]<-"None"
housing[is.na(housing$SaleType), "SaleType"]<-housing %>% group_by(SaleType) %>% summarise(n = n()) %>% filter(n == max(n)) %>% pull(SaleType)

####### Converting variables to factors
housing <- housing %>% mutate_if(is.character,as.factor)
housing$MSSubClass= factor(housing$MSSubClass)
housing$OverallCond = factor(housing$OverallCond)
housing$OverallQual = factor(housing$OverallQual)

##### Splitting into train and test
train= housing[1:1460,]
test= housing[1461:2919,]

##### Subseting train set for cross-validation
sub <- sample(1:nrow(train),size=nrow(train)/2)
sub.train <- train[sub,]     
sub.valid <- train[-sub,]

###### Linear Model 4
sub.train.lm4 <- lm(SalePrice~LotArea+Neighborhood+OverallQual+YearBuilt+YearRemodAdd
                    +RoofMatl+X1stFlrSF+X2ndFlrSF+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF
                    +KitchenQual+PoolArea+SaleCondition+ScreenPorch,
                    data=train)
summary(sub.train.lm4)
# Adj.R-squared: 0.8874, p-value:< 2.2e-16

###### Cross-validation
predict.lm4 <- predict(sub.train.lm4, newdata=sub.valid)

# rmse between logs
rmse4 <- sqrt(mean(log(predict.lm4)-log(sub.valid$SalePrice))^2)
rmse4 #0.007251649 - chosen for the lowest rmse value

###### Predictions
price.predict4<-predict(sub.train.lm4, newdata=test)

house.predict.merged4 <- as.data.frame(cbind(test$Id, price.predict4))
names(house.predict.merged4) <- c("Id","SalePrice")

write.table(house.predict.merged4, file = "parametric4.csv", 
            row.names=F, col.names=T, sep=",")

