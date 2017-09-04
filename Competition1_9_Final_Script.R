#Tyler Lewris
#tal3fj
#Sai Prasanth
#
#SYS 6018 Competition 1-9 House Prices
#September 5th 2017

#loading necessary libraries
library(readr)
library(dplyr)
library(plyr)
library(DAAG)
library(class)
library(caret)

train <- read_csv("train.csv") #Loading in data set

#-------------------------------------DATA CLEANING----------------------------------
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
#We are going to take a look at our year variables and turn them
#into something more meaninful like age.
#Utilizing age instead of year will give us more insight into our data set and to see 
#if there are potential correlations to price for those houses that are younger in terms of age
#common sense would have it that the younger the age -> the higher the price sold

summary(train$YearBuilt)
#Min is 1872 - oldest house built
train$AgeBuilt = (train$YearBuilt - 1872)

summary(train$YearRemodAdd)
#min 1950 ... max 2010
train$AgeSinceRemod = (train$YearRemodAdd - 1950)

#brief data exploration
#see if there's a correlation
age.lm <- lm(SalePrice ~ AgeBuilt + AgeSinceRemod, data = train)
summary(age.lm)


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

#FURTHER DATA CLEANING ON TEST SET
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


#factorizing columns

cols <- c(3, 6:17, 22:26, 28:34, 36, 40:43, 54, 56, 58, 59, 61, 64:66, 73:75, 79:80)

test[cols] <- lapply(test[cols], factor)

test_corrected <- subset(test, !is.na(test$MasVnrType))

test_corrected2 <- subset(test_corrected, !is.na(test_corrected$Electrical))

test_corrected2 <- test_corrected2[ -c(10, 20:21, 60) ]

apply(test_corrected2, 2, function(col) sum(is.na(col)))




#-------------------------------DATA EXPLORATION/MODELING----------------------------
# Modelling

# Regression

model <- lm(SalePrice ~ ., data = train_corrected2)
summary(model)

# Applying the model on test set

test_predictions = predict(model, newdata = test_corrected2, type = "response")
test_predictions[test_predictions<0] <- 0
test_predictions[is.na(test_predictions)] = mean(test_predictions[!is.na(test_predictions)])
kaggle_submission = cbind(test$Id, test_predictions)
colnames(kaggle_submission) = c("Id", "SalePrice")
write.csv(kaggle_submission, file = "basic_linear_model_correct.csv", row.names = FALSE)


# However, we would like to explore each of the signficant variables in more depth, plot 2 dimensional graphs and observe if the variables have any meaningful relationship with sale price with plots

plot(train_corrected2$MSZoning, train_corrected2$SalePrice)

# Yes. Meaningful relationship

wer <- subset(train_corrected2, train_corrected2$LotArea < 20000)

plot(wer$LotArea, wer$SalePrice)

# No. Kind of surprising. We had to subset the idea and got rid of the outliers

plot(train_corrected2$LotConfig, train_corrected2$SalePrice)

# Not really

plot(train_corrected2$LandSlope, train_corrected2$SalePrice)

# No

plot(train_corrected2$Neighborhood, train_corrected2$SalePrice)

# clear relationship

plot(train_corrected2$Condition1, train_corrected2$SalePrice)

plot(train_corrected2$Condition2, train_corrected2$SalePrice)

# Just from the graph, it seems like there are differences across categories. But then we noticed that more than 95% of the data points are filled with one value 'Norm'. So we are ignoring these two as well

plot(train_corrected2$OverallQual, train_corrected2$SalePrice)

# totally significant

plot(train_corrected2$OverallCond, train_corrected2$SalePrice)

# I expected it to be significant. But turns out bulk of the homes are rated 5 and 6

# Actually its pretty much the same in test set as well. I am not including thsi

plot(train_corrected2$RoofStyle, train_corrected2$SalePrice)

# Nothing

plot(train_corrected2$RoofMatl, train_corrected2$SalePrice)

summary(train_corrected2$RoofMatl)

summary(test_corrected2$RoofMatl)

# OK. I am including it. But there are barely 95% of the values are just 1 value

plot(train_corrected2$MasVnrArea, train_corrected2$SalePrice)

checking <- subset(train_corrected2, train_corrected2$MasVnrArea < 400)

plot(checking$MasVnrArea, checking$SalePrice)

# No. nothing really going on

plot(train_corrected2$ExterQual, train_corrected2$SalePrice)

# Yes. it matters

plot(train_corrected2$BsmtQual, train_corrected2$SalePrice)

# It matters

plot(train_corrected2$BsmtExposure, train_corrected2$SalePrice)

summary(train_corrected2$BsmtExposure)

summary(test_corrected2$BsmtExposure)

# It seems like there is significant difference only if there is no basement at all. But that aspect is captured is other variables. Hence, not including this variable

plot(train_corrected2$BsmtFinSF1, train_corrected2$SalePrice) # we need to subset to understand relationship

subset_checkfg <- subset(train_corrected2, train_corrected2$BsmtFinSF1 < 1500)

plot(subset_checkfg$BsmtFinSF1, subset_checkfg$SalePrice) 

# The curve is slightly upward. But there is not a lot going on honestly. Not including it

plot(train_corrected2$BsmtFinSF2, train_corrected2$SalePrice) 

# Don't even need to bother subsetting. It is clear with basic plot. No real relationship

plot(train_corrected2$BsmtUnfSF, train_corrected2$SalePrice) 

subset_checkjs <- subset(train_corrected2, train_corrected2$BsmtUnfSF < 1500)

plot(subset_checkjs$BsmtUnfSF, subset_checkjs$SalePrice) 

# Absolutely no relationship

plot(train_corrected2$`1stFlrSF`, train_corrected2$SalePrice) 

# There is definitely a relationship. More like parabolic rather than linear actually

plot(train_corrected2$`2ndFlrSF`, train_corrected2$SalePrice) 

# yes. important

plot(train_corrected2$BedroomAbvGr, train_corrected2$SalePrice) 

# There isn't much of a difference across levels really

plot(train_corrected2$KitchenAbvGr, train_corrected2$SalePrice) 

summary(train_corrected2$KitchenAbvGr)

# Seems like most of the data is at one level. So, ignoring this variable

plot(train_corrected2$KitchenQual, train_corrected2$SalePrice) 

# Yes. There is significant difference

plot(train_corrected2$Functional, train_corrected2$SalePrice) 

# Not really important

plot(train_corrected2$Fireplaces, train_corrected2$SalePrice) 

# Kind of matters. I am just leaving it in

# We conclude that the ones that truly matter to the analysis are MSZoning, 
#Neighborhood, overallqual, roofmatl, externalqual, bsmtqual, '1stflrsf', 
#'2ndflrsf', kitchenqual, Fireplaces

plot(train_corrected2$AgeBuilt, train_corrected2$SalePrice) 


#-------------------------------------LINEAR MODEL------------------------------------

#-----------------------------------PARAMETRIC APPROACH-------------------------------

# Building a linear model with only the ones that are found to be significant


model_selected <- lm(SalePrice ~ MSZoning + Neighborhood + OverallQual + RoofMatl + ExterQual + BsmtQual + `1stFlrSF` + `2ndFlrSF` + KitchenQual + Fireplaces, data = train_corrected2)
summary(model_selected)

# Every variable showed up as significant. Most of them were 3 stars

#lets reduce the number of variables and see if we get a different result
model_adjusted1 <- lm(SalePrice ~ MSZoning + Neighborhood + OverallQual + ExterQual + BsmtQual + `1stFlrSF` + `2ndFlrSF` + KitchenQual, data = train_corrected2)
summary(model_adjusted1)

model_adjusted2 <- lm(SalePrice ~ MSZoning + Neighborhood + OverallQual + ExterQual + BsmtQual + `1stFlrSF` + `2ndFlrSF` + KitchenQual + AgeBuilt, data = train_corrected2)
summary(model_adjusted2)


#--------------------RATIONALE FOR THE SELECTED STATISTICAL MODELING METHODS-----------

# We focussed on Linear Regression among parametric approaches because we believe that it is a great modelling technique and one that we are very comfortable with. Also, based on the plots we generated, it was apparent to us that most of the variables, the ones that are important had a linear relationship 

# We choose K-NN as our parametric approach since we don't have a lot of important numeric variables. Based on the plots we generated, it seemed like only 4 numeric parameters were important and K-NN, an algrorithm convinient to implement could potentially give us a good answer

#-----------------------------------REGRESSION CROSS VALIDATION----------------------------------

#The Validation Set Approach
nrow(train_corrected2)
#1451 total observations, split into two groups

train_sample_CV <- train_corrected2[(sample(1:nrow(train_corrected2),725,F)),]
train_test_CV <- train_corrected2[(-sample(1:nrow(train_corrected2),726,F)),]

#Lets run our best linear model from above on the train_sample 
CV_model_train <- lm(SalePrice ~ MSZoning + Neighborhood + OverallQual +
                       ExterQual + BsmtQual + `1stFlrSF` + `2ndFlrSF` +
                       KitchenQual + AgeBuilt, data = train_sample_CV)
summary(CV_model_train)
train_sample_CV_MSE <- mean(CV_model_train$residuals^2)
#MSE 963,034,761
#MSE is high but lets use our model to predict the output of the second 
#half of our subsetted data and compare

#now we need to run our model on the second half of our data and get our predictions
CV_predictions = predict(CV_model_train, newdata = train_test_CV, type = "response")
summary(CV_predictions)
TestMSE <- mean((CV_predictions - train_test_CV$SalePrice)^2)
sqrt(TestMSE)
#Cross validation shows on average we are missing the predicted output by $36,424

#Second Cross Validation approach
#Cross validate the model with three folds to get an average MSE
#Mean Square = 1.33e+09
cv.lm(train_corrected2, model_adjusted2, m=3, dots =
        FALSE, seed=29, plotit=TRUE, printit=TRUE)


#-----------------------------------NON PARAMETRIC APPROACH--------------------------

#This creates subsets of the data and here we are only using numerical predictors. 

group <- 1:725
group2 <- 1:725
train_num = as.data.frame(cbind(train_corrected2$`1stFlrSF`, train_corrected2$`2ndFlrSF`, train_corrected2$AgeBuilt, train_corrected2$OverallQual, train_corrected2$SalePrice)[group,])
test_num = as.data.frame(cbind(train_corrected2$`1stFlrSF`, train_corrected2$`2ndFlrSF`, train_corrected2$AgeBuilt, train_corrected2$OverallQual, train_corrected2$SalePrice)[-group2,])

colnames(train_num) <- c("floor1","floor2","age","qual","price")
colnames(test_num) <- c("floor1","floor2","age","qual","price")

model_knn <- train(train_num[, 1:4], train_num[, 5], method='knn', preProcess=c("center", "scale"))

predictions<-predict(object=model_knn,test_num[,1:4], type = "raw")

predictions <- as.data.frame(predictions)

mean((test_num$price - predictions$predictions)^2)

MSe <- mean((predictions$predictions - test_num$price)^2)
sqrt(MSe)

# 34,000. Not a bad value actually. Building a final K-NN model on the actual test set

# Applying the K-NN model built above on the actual test data set

train_num = as.data.frame(cbind(train_corrected2$`1stFlrSF`, train_corrected2$`2ndFlrSF`, train_corrected2$AgeBuilt, train_corrected2$OverallQual, train_corrected2$SalePrice))

test_num = as.data.frame(cbind(test_corrected2$`1stFlrSF`, test_corrected2$`2ndFlrSF`, test_corrected2$AgeBuilt, test_corrected2$OverallQual))


colnames(train_num) <- c("floor1","floor2","age","qual","price")
colnames(test_num) <- c("floor1","floor2","age","qual")

model_knn <- train(train_num[, 1:4], train_num[, 5], method='knn', preProcess=c("center", "scale"))

predictions<-predict(object=model_knn,test_num[,1:4], type = "raw")

predictions <- as.data.frame(predictions)

kaggle_submission2 = cbind(test$Id, predictions$predictions)
colnames(kaggle_submission) = c("Id", "SalePrice")
write.csv(kaggle_submission, file = "K-NN.csv", row.names = FALSE)


#-------------------------------------WRITE TO CSV------------------------------------

test_predictions = predict(model_adjusted2, newdata = test_corrected2, type = "response")
test_predictions[test_predictions<0] <- 0
kaggle_submission = cbind(test$Id, test_predictions)
colnames(kaggle_submission) = c("Id", "SalePrice")
write.csv(kaggle_submission, file = "linear_model_adjusted2.csv", row.names = FALSE)

# We think its kind of ironic that we could have used the test data set without 
# any sort of cleaning directly and still get the excat same output file. 
# The columns that actually matter don't have any missing values and R is smart enough to understand which factors need to be factorized.

kaggle_submission2 = cbind(test$Id, predictions$predictions)
colnames(kaggle_submission) = c("Id", "SalePrice")
write.csv(kaggle_submission, file = "K-NN.csv", row.names = FALSE)
