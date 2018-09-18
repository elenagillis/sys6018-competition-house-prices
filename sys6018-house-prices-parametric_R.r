################################################################################
# sys6018-house-prices-parametric.R
# House Prices Kaggle Competition
# September 18, 2018
################################################################################

# Import data
housetrain = read.csv('train.csv')
housetest = read.csv('test.csv')


# Identifying factor variables
factor_var <- c('MSSubClass','MSZoning','Street','Alley','LotShape','LandContour',
             'Utilities','LotConfig','LandSlope','Neighborhood','Condition1',
             'Condition2','BldgType','HouseStyle','OverallQual','OverallCond',
             'RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType',
             'ExterQual','ExterCond','Foundation','BsmtQual','BsmtCond',
             'BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC',
             'CentralAir','Electrical','KitchenQual','Functional','FireplaceQu',
             'GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive',
             'PoolQC','Fence','MiscFeature','SaleType','SaleCondition')

# Setting factor variables as factors
housetrain[factor_var] <- lapply(housetrain[factor_var],factor)
housetest[factor_var] <- lapply(housetest[factor_var],factor)

# Subseting train set for cross-validation
sub <- sample(1:nrow(housetrain),size=nrow(housetrain)/2)
sub.train <- housetrain[sub,]     
sub.valid <- housetrain[-sub,]


# Simple linear Model
sub.train.lm2 <- lm(SalePrice~LotConfig+Neighborhood+OverallQual+YearRemodAdd
                    +RoofMatl+Exterior1st+Exterior2nd+X1stFlrSF+X2ndFlrSF
                    +KitchenQual+TotRmsAbvGrd+PoolArea+SaleType+SaleCondition,
                    data=housetrain)
summary(sub.train.lm2)
# Adj.R-squared: 0.9013, p-value:< 2.2e-16

# Cross-validation
predict.lm2 <- predict(sub.train.lm2, newdata=sub.valid)

# rmse between logs
rmse2 <- sqrt(mean(log(predict.lm2)-log(sub.valid$SalePrice))^2)
rmse2 #0.007414275

# Predictions
price.predict<-predict(sub.train.lm2, newdata=housetest)

house.predict.merged <- as.data.frame(cbind(housetest$Id, price.predict))
names(house.predict.merged) <- c("Id","SalePrice")

# Replacing NA with median price
house.predict.merged$SalePrice[is.na(house.predict.merged$SalePrice)]<- 0
house.predict.merged$SalePrice[house.predict.merged$SalePrice==0] <- median(house.predict.merged$SalePrice)

write.table(house.predict.merged, file = "house_predict.csv", 
            row.names=F, col.names=T, sep=",")



# Linear Model 4
sub.train.lm4 <- lm(SalePrice~LotArea+Neighborhood+OverallQual+YearBuilt+YearRemodAdd
                    +RoofMatl+X1stFlrSF+X2ndFlrSF+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF
                    +KitchenQual+PoolArea+SaleCondition+ScreenPorch,
                    data=housetrain)
summary(sub.train.lm4)
# Adj.R-squared: 0.8828, p-value:< 2.2e-16

# cross-validation
predict.lm4 <- predict(sub.train.lm4, newdata=sub.valid)

# rmse between logs
rmse4 <- sqrt(mean(log(predict.lm4)-log(sub.valid$SalePrice))^2)
rmse4 #0.00426434

# Predictions
price.predict4<-predict(sub.train.lm4, newdata=housetest)

house.predict.merged4 <- as.data.frame(cbind(housetest$Id, price.predict4))
names(house.predict.merged4) <- c("Id","SalePrice")

# Replacing NA with median price
house.predict.merged4$SalePrice[is.na(house.predict.merged4$SalePrice)]<- 0
house.predict.merged4$SalePrice[house.predict.merged4$SalePrice==0] <- median(house.predict.merged4$SalePrice)

write.table(house.predict.merged4, file = "house_predict4.csv", 
            row.names=F, col.names=T, sep=",")

