setwd("C:/Users/Sandeep/Downloads")

install.packages('randomForest')
library(randomForest)

df.train <- read.csv('train_u6lujuX.csv')
df.train$Credit_History <- as.factor(df.train$Credit_History)
str(df.train)

df.model.rf = randomForest(Loan_Status ~ LoanAmount + Loan_Amount_Term + Loan_Amount_Term+Credit_History, data=df.train, ntree=500,nodesize=50,na.action=na.roughfix )

PredictForest = predict(df.model.rf, newdata = df.train)


#df.test <- cbind(df.test,PredictForest)
##Calculating misclassification error using confusion matrix.
cm<-table(df.train$Loan_Status,PredictForest)
cm
1-sum(diag(cm))/sum(cm)