

#import libraries
library("dplyr", lib.loc="~/R/win-library/3.4")
library("readxl", lib.loc="~/R/win-library/3.4")
library("stringr", lib.loc="~/R/win-library/3.4")
library("flexclust", lib.loc="~/R/win-library/3.4")

#import full dataset (650 rows) and store in 'loan'
loan = read_excel('LoanRaw.xls')
#inspect the structure of the data
str(loan)

#create training (first 600 rows) and test (following 50 rows) sets
loantrain = loan[1:600,]
loantest = loan[601:650,]

#inspect training and testing datasets
str(loantrain)
str(loantest)
boxplot(loantrain)
boxplot(loantest)

#possibly may need to scale fields income, assets and debts
#loantrain$Income = log(loantrain$Income)
#loantrain$Assets = log(loantrain$Assets)
#loantrain$Debts = log(loantrain$Debts)

#loantest$Income = log(loantest$Income)
#loantest$Assets = log(loantest$Assets)
#loantest$Debts = log(loantest$Debts)

loantrain_model = subset(loantrain, select = -Ontime)
loantest_model = subset(loantest, select = -Ontime)

#train model
model_base = kmeans(loantrain_model, centers = 2)
model = kcca(loantrain_model, k = 2, family = kccaFamily("kmeans"), simple = TRUE)

#confusion/coincidence matrix
table(model_base$cluster, loantrain$Ontime)
#71.5% accuracy

#predict
model_pred = predict(model, loantest_model)

#test data confusion matrix (model performance)
table(model_pred, loantest$Ontime)
#84% accuracy



