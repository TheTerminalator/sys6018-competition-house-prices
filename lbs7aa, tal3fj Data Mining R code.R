library(readr)
library(dplyr)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

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

cols <- c(3, 6:17, 22:26, 28:34, 36, 40:43, 54, 56, 58, 59, 61, 64:66, 73:75, 79:80)

train[cols] <- lapply(train[cols], factor)

sapply(train, class)

apply(train, 2, function(col) sum(is.na(col)))
train$LotFrontage[is.na(train$LotFrontage)] = mean(train$LotFrontage[!is.na(train$LotFrontage)])

train_corrected <- subset(train, !is.na(train$MasVnrType))

apply(train_corrected, 2, function(col) sum(is.na(col)))

train_corrected2 <- subset(train_corrected, !is.na(train_corrected$Electrical))

apply(train_corrected2, 2, function(col) sum(is.na(col)))

train_corrected2[0, 7]

#train_corrected2 <- addNA(factor)

#train_corrected2$Alley[is.na(train_corrected2$Alley)] = "None"

model <- lm(SalePrice ~ OverallQual + YearBuilt, data = train)
summary(model)




# create kaggle submission file
test_predictions = predict(model, newdata = test, type = "response")
test_predictions[test_predictions<0] <- 0
kaggle_submission = cbind(test$Id, test_predictions)
colnames(kaggle_submission) = c("Id", "SalePrice")
write.csv(kaggle_submission, file = "basic_linear_model.csv", row.names = FALSE)
