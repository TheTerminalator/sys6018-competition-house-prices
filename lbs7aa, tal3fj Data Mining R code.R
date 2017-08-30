library(readr)
library(dplyr)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

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
