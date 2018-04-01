


library("readxl", lib.loc="~/R/win-library/3.4")
fraud = read_excel("FraudRaw.xls")

#inspect the fraud dataset
summary(fraud)
str(fraud)
#there are no missing values

#remove original Age, claims, tickets and atty fields
#keeping their transformed version
fraud_T = fraud[-1]
fraud_T = fraud_T[-3]
fraud_T = fraud_T[-4]
fraud_T = fraud_T[-6]
#all remaining and transformed fields are numeric


#segment into training (first 1000 rows) and test (remaining 1000 rows) set
fraud_T_train = fraud_T[1:1000,]
fraud_T_test = fraud_T[1001:2000,]


#fit model
log_model = glm(outcome ~ ., family = binomial, data = fraud_T_train)

#get probabilities
train_prob = predict(log_model, fraud_T_train, type = "response")

#display ROC curve
ROCRpred = prediction(train_prob, fraud_T_train$outcome)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE)
#ROC curve shows that .2 may be the best cutoff point


#set threshold to trigger a yes for fraud (1)
train_pred = rep("0", nrow(fraud_T_train))
train_pred[train_prob > .2] = "1"

#display confusion matrix
table(fraud_T_train$outcome, train_pred)



#apply model to test set
test_prob = predict(log_model, fraud_T_test, type = "response")

#display the ROC curve
ROCRpred = prediction(test_prob, fraud_T_test$outcome)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE)
#ROC curve shows an extremely poor model fit
#the most efficient cutoff is approximately .2

#set threshold to trigger a yes for fraud (1)
test_pred = rep("0", nrow(fraud_T_test))
test_pred[test_prob > .2] = "1"


#display confusion matrix
table(fraud_T_test$outcome, test_pred)

#Model does not perform very well
#At first appears that there is only a few typeII errors
#But the actual number of fraud is only 15 observations
#fraudulent observations is 100% incorrect
#Non-fraud is 100% correct

#Therefore the model is predicting no fraud at the .5 threshold
#It still only predicts 1/15 fraud at the .1 threshold with little movement
#in the non-fraudulent observations (969/985)

#At the .01 threshold, it only accurately predicts 7/15 fraud but
#suffers severely on correctly classifying Non-fraudulent policies (504/985)
#This is clearly illustrated in the ROC curve display






