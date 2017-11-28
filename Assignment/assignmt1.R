# Defining the path
path<-'C:/Users/guptasa/Downloads'
setwd(path)


#Importing the dataset
library(readxl)
employee <- read_excel("C:/Users/guptasa/Downloads/Employeeattrition.xlsx")
view(employee)
attach(employee)

#Summary and structure of the dataset
summary(employee)
str(employee)

# we can see the format of categorical variables are not in Factors and as they enter into statistical models 
# differently than continuous variables, storing data as factors insures that 
# the modeling functions will treat such data correctly.

#Converting variables into factors
columns_factor<- c(2,3,5,7,8,10:12,14,15:18,22,23,25,26,28,31)
library(magrittr)
library(dplyr)

employee %<>%
  mutate_each_(funs(factor(.)),columns_factor)
str(employee)

#Check on Missing Values
library(VIM)
e1 <-aggr(employee)
summary(e1)

#Check on NearZero Variance
library(caret)
x <-nearZeroVar(employee,saveMetrics=TRUE)
x

#Splitting the dataset into Training Set and Test Set
install.packages('caTools')
library(caTools)
set.seed(111)
split=sample.split(employee$Attrition, SplitRatio = 0.75)

training_set = subset(employee, split == TRUE)
test_set = subset(employee, split == FALSE)


#Fitting a Tree using Rpart
library(rpart)
fit <- rpart(Attrition ~ .,data=training_set[,c(-9,-10,-27)])


#Summarizing the parameters of the fit
summary(fit)

# Getting the best CP Value
printcp(fit)
plotcp(fit)


# we see that the best Cp value is at 0.016854
# Root node error: 178/1103 = 0.16138

# n= 1103 
# 
# CP nsplit rel error  xerror     xstd
# 1 0.053371      0   1.00000 1.00000 0.068639
# 2 0.030899      2   0.89326 0.98315 0.068169
# 3 0.025281      4   0.83146 0.96629 0.067691
# 4 0.019663      6   0.78090 0.93258 0.066714
# 5 0.016854      8   0.74157 0.91573 0.066214
# 6 0.010000     10   0.70787 0.92135 0.066381

#Plotting the tree

library(rattle)
drawTreeNodes(fit,cex=.8,pch=11,size=4*.8, col=NULL,nodeinfo=TRUE,
              units="",cases="obs",digits=getOption("digits"),decimals=2,print.levels=TRUE,
              new=TRUE)



#Predicting the probabilities of the fit
predvalues<-predict(fit)


#Predicting the probabilities of the test_set
test_set$test_pred_prob <- predict(fit,test_set)
test_pred <- predict(fit,test_set,type='class')
summary(test_set$test_pred_prob)


#Confusion Matrix
ConfusionMatrix <- confusionMatrix(test_pred, test_set$Attrition, positive = "Yes")
ConfusionMatrix



#ROC Curve Analysis
library(ROCR)
demo(ROCR)
predictn <-prediction(test_set$test_pred_prob[,2],test_set$Attrition)
str(predictn)

predictn@fp
predictn@fn
predictn@n.neg


perf<-performance(predictn, "tpr","fpr")
plot(perf)
auc<-performance(predictn,"auc")@y.values
auc






