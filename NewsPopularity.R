
#Import packages

library("readr", lib.loc="~/R/win-library/3.4")
library("caret", lib.loc="~/R/win-library/3.4")
library("corrplot", lib.loc="~/R/win-library/3.4")
library("mlbench", lib.loc="~/R/win-library/3.4")
library("ROCR", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")

#Import data and store in variable named "News"
News = read_csv("OnlineNewsPopularity.csv")

#View and observe the data
str(News)
summary(News)

#Remove the first 2 non-predictive columns
News2 = News[-1]
News2 = News2[-1]

#add new binary shares column called shares2
News_binary = News2
News_binary$shares2[News_binary$shares>1400] <- 1
News_binary$shares2[News_binary$shares<=1400] <- 0

#remove original shares column
News3 = News_binary[-59]
#Reinspect data
str(News3)

#Get correlation matrix of News2
correlations = cor(News3)
corrplot(correlations, method = "circle")


#get a random sample of 5000 rows
set.seed(48)
Trainset = sample(nrow(News3), 5000)
News3Train = News3[Trainset, ]
NewsTest = News3[-Trainset, ]

#build training model
model3 = glm(shares2 ~ ., data = News3Train, family = binomial)
model3Prob = predict(model3, News3Train, type = "response")
model3Pred = rep("0", nrow(News3Train))
model3Pred[model3Prob > .5] = "1"
table(News3Train$shares2, model3Pred)
#66% accuracy, not bad

#take a look at the ROC curve to ge the most ideal threshold
ROCRpred = prediction(model3Prob, News3Train$shares2)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE)

#.42 seems to be the optimal cutoff in order to be conservative
# and reduce the number of times we call a non popular article popular
model3 = glm(shares2 ~ ., data = News3Train, family = binomial)
model3Prob = predict(model3, News3Train, type = "response")
model3Pred = rep("0", nrow(News3Train))
model3Pred[model3Prob > .42] = "1"
table(News3Train$shares2, model3Pred)
#65% Accuracy with type one error down to 11%


#run on test set
model3TestProb = predict(model3, NewsTest, type = "response")
model3TestPred = rep("0", nrow(NewsTest))
model3TestPred[model3TestProb > .42] = "1"
table(NewsTest$shares2, model3TestPred)
#62.7% accuracy on the test sample (34,644 rows) with this model
#type 1 error remained at 11%





