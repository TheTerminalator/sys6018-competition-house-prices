library(readr)
library(dplyr)
library(plyr)
library(DAAG)

train <- read_csv("train.csv") #Loading in data set

# replace NA's with None or 0

train$Alley[is.na(train$Alley)] <- 'None'
train$BsmtQual[is.na(train$BsmtQual)] <- 'None'
train[7][is.na(train[, 7])] <- 'None'
train[31:34][is.na(train[, 31:34])] <- 'None'
train[35][is.na(train[, 35])] <- 0
train[36][is.na(train[, 36])] <- 'None'
train[58:59][is.na(train[, 58:59])] <- 'None'
train[61][is.na(train[, 61])] <- 'None'
train[62:63][is.na(train[, 62:63])] <- 0
train[64:65][is.na(train[, 64:65])] <- 'None'
train[73:75][is.na(train[, 73:75])] <- 'None'
train$LotFrontage[is.na(train$LotFrontage)] = mean(train$LotFrontage[!is.na(train$LotFrontage)])
train$LotArea[is.na(train$LotArea)] = mean(train$LotArea[!is.na(train$LotArea)])


#Cleaning years into ages
#We are going to take a look at our year variables and turn them into something more meaninful
#like age.
#Utilizing age instead of year will give us more insight into our data set and to see 
#if there are potential correlations to price for those houses that are younger in terms of age
#common sense would have it that the younger the age -> the higher the price sold

summary(train$YearBuilt)
#Min is 1872 - oldest house built
train$AgeBuilt = (train$YearBuilt - 1872)

summary(train$YearRemodAdd)
#min 1950 ... max 2010
train$AgeSinceRemod = (train$YearRemodAdd - 1950)

#see if there's a correlation
age.lm <- lm(SalePrice ~ AgeBuilt + AgeSinceRemod, data = train)
summary(age.lm)

# MIGHT NEED TO MOVE

#factorizing columns

cols <- c(3, 6:17, 22:26, 28:34, 36, 40:43, 54, 56, 58, 59, 61, 64:66, 73:75, 79:80)

train[cols] <- lapply(train[cols], factor)

sapply(train, class)

train_corrected <- subset(train, !is.na(train$MasVnrType))

train_corrected2 <- subset(train_corrected, !is.na(train_corrected$Electrical))

train_corrected2 <- train_corrected2[ -c(10, 20:21, 60) ]

apply(train_corrected2, 2, function(col) sum(is.na(col)))


# we are dropping several columns to make our analysis clean and easier to follow
# We are dropping the Utilities column as the only available values are "AllPub" which does not 
#give us any better understanding of the data
# We are also dropping YearBuilt and YearRemodAd as well because we have calculated Ages
# Finally, we are dropping GarageYrBuilt as there are 81 cases where there is no garage





# We wish to clean test data set in a similar fashion

test <- read_csv("test.csv") #Loading in data set

# replace NA's with None or 0

test$Alley[is.na(test$Alley)] <- 'None'
test$BsmtQual[is.na(test$BsmtQual)] <- 'None'
test[7][is.na(test[, 7])] <- 'None'
train[31:34][is.na(train[, 31:34])] <- 'None'
train[35][is.na(train[, 35])] <- 0
train[36][is.na(train[, 36])] <- 'None'
test[58:59][is.na(test[, 58:59])] <- 'None'
test[61][is.na(test[, 61])] <- 'None'
test[62:63][is.na(test[, 62:63])] <- 0
test[64:65][is.na(test[, 64:65])] <- 'None'
test[73:75][is.na(test[, 73:75])] <- 'None'
test$LotFrontage[is.na(test$LotFrontage)] = mean(test$LotFrontage[!is.na(test$LotFrontage)])
test$LotArea[is.na(test$LotArea)] = mean(test$LotArea[!is.na(test$LotArea)])

# Data cleaning specific to test set. There are certain columns which contain NA values in test but not in train. We understand that we cannot simply delete rows in test set. We want to make a decision after looking at each of the columns

test$MSZoning[is.na(test$MSZoning)] = 'RL' # The most frequent value
test[24][is.na(test[, 24])] <- 'MetalSd' 
# There is one missing value. Metal Sd was the most frequent value in the exterior 1st column.
test[25][is.na(test[, 25])] <- 'VinylSd' 
# There is one missing value. Vinyl Sd was the most frequent value in the exterior 1st column.
test[26][is.na(test[, 26])] <- 'None' 
test[27][is.na(test[, 27])] <- 0
test[37][is.na(test[, 37])] <- 0 # 1 Missing value. Most of this column is 0 anyway
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] = mean(test$BsmtUnfSF[!is.na(test$BsmtUnfSF)])
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] = mean(test$TotalBsmtSF[!is.na(test$TotalBsmtSF)])
# There's just one missing in both BsmtUnfSF and TotalBsmtSF. So, I just them to average
test$BsmtFullBath[is.na(test$BsmtFullBath)] = 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] = 0
# There's just two missing in both BsmtUnfSF and TotalBsmtSF. I noticed these values are only 1's and 0's in the data set. So, I just set both to 0
test$KitchenQual[is.na(test$KitchenQual)] = 'TA'
# There is one missing value. I just set to most frequent value
test$Functional[is.na(test$Functional)] = 'Typ'
# 2 missing values. Setting it to most frequent value
test$SaleType[is.na(test$SaleType)] = 'WD'
# 1 missing value. Setting it to the most frequent value.

# seems like most of these columns whch have missing values in test are missing just 1 or 2 values. So, we don't think it really matters how we deal with them



#Cleaning years into ages
#We are going to take a look at our year variables and turn them into something more meaninful
#like age.
#Utilizing age instead of year will give us more insight into our data set and to see 
#if there are potential correlations to price for those houses that are younger in terms of age
#common sense would have it that the younger the age -> the higher the price sold

summary(test$YearBuilt)
#Min is 1879 - oldest house built
test$AgeBuilt = (test$YearBuilt - 1879)

summary(test$YearRemodAdd)
#min 1950 ... max 2010
test$AgeSinceRemod = (test$YearRemodAdd - 1950)

#see if there's a correlation
age.lm <- lm(SalePrice ~ AgeBuilt + AgeSinceRemod, data = test)
summary(age.lm)

# MIGHT NEED TO MOVE

#factorizing columns

cols <- c(3, 6:17, 22:26, 28:34, 36, 40:43, 54, 56, 58, 59, 61, 64:66, 73:75, 79:80)

test[cols] <- lapply(test[cols], factor)

test_corrected <- subset(test, !is.na(test$MasVnrType))

test_corrected2 <- subset(test_corrected, !is.na(test_corrected$Electrical))

test_corrected2 <- test_corrected2[ -c(10, 20:21, 60) ]

apply(test_corrected2, 2, function(col) sum(is.na(col)))

# Modelling

# Simple Linear Regression

model <- lm(SalePrice ~ ., data = train_corrected2)
summary(model)

test_predictions = predict(model, newdata = test, type = "response")
test_predictions[test_predictions<0] <- 0
test_predictions[is.na(test_predictions)] = mean(test_predictions[!is.na(test_predictions)])
kaggle_submission = cbind(test$Id, test_predictions)
colnames(kaggle_submission) = c("Id", "SalePrice")
write.csv(kaggle_submission, file = "basic_linear_model.csv", row.names = FALSE)
      

#MODEL 3 is a model that takes into account relative p values of each independent variable
model3 <- lm(SalePrice ~ LotArea + LandSlope + Neighborhood + Condition1
             + Condition2 + OverallQual + OverallCond + RoofMatl +
               Exterior1st + Exterior2nd + MasVnrArea + ExterQual +
               ExterCond + BsmtQual + BsmtCond + BsmtExposure +
               BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF +
               KitchenQual, data = train_corrected2)

test_predictions = predict(model3, newdata = test, type = "response")
test_predictions[test_predictions<0] <- 0
test_predictions[is.na(test_predictions)] = mean(test_predictions[!is.na(test_predictions)])
kaggle_submission = cbind(test$Id, test_predictions)
colnames(kaggle_submission) = c("Id", "SalePrice")
write.csv(kaggle_submission, file = "basic_linear_model3.csv", row.names = FALSE)
