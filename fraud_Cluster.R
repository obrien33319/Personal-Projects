

#import libraries
library("dplyr", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("stringr", lib.loc="~/R/win-library/3.4")
library("flexclust", lib.loc="~/R/win-library/3.4")

#import full dataset (5000 rows) and store in 'fraud'
fraud = read_excel('FraudRaw.xls')
#inspect the structure of the data
str(fraud)


#convert string into categorical data
#Gold = 1, Jones = 2, None = 3, Smith = 4
fraud$atty = str_replace(fraud$atty, "Gold", "1")
fraud$atty = str_replace(fraud$atty, "Jones", "2")
fraud$atty = str_replace(fraud$atty, "none", "3")
fraud$atty = str_replace(fraud$atty, "Smith", "4")

#gender, atty and outcome are not numerical values and should be changed to factor
fraud$Gender = as.numeric(fraud$Gender)
fraud$outcome = as.numeric(fraud$outcome)
fraud$atty = as.numeric(fraud$atty)

#reinspect fraud data
str(fraud)

#create training (first 1000 rows) and test (following 1000 rows) sets
fraudtrain = fraud[1:1000,]
fraudtest = fraud[1001:2000,]


#inspect training and testing datasets
str(fraudtrain)
str(fraudtest)

#scale claims field using log
#fraudtrain$Claim = log(fraudtrain$Claim)
#fraudtest$Claim = log(fraudtest$Claim)


#remove outcome field from data
fraudtrain_model = subset(fraudtrain, select= -outcome)
fraudtest_model = subset(fraudtest, select= -outcome)


#train model
model_base = kmeans(fraudtrain_model, centers = 2)
model = kcca(fraudtrain_model, k = 2, family = kccaFamily("kmeans"), simple = TRUE)

#confusion/coincidence matrix
#table(model$cluster, fraudtrain$outcome)
table(model_base$cluster, fraudtrain$outcome)
#53.2% accuracy

#predict
model_pred = predict(model, fraudtest_model)

#test data confusion matrix (model performance)
table(model_pred, fraudtest$outcome)
#49.5% accuracy


