
# Defining the path
path<-'C:/Users/Sankalp/Documents/R/Files/'
setwd(path)


#Importing the dataset
library(readxl)
employee <- read_excel("~/R/Files/Employeeattrition.xlsx")
view(employee)
attach(employee)

#Summary and structure of the dataset
summary(employee)
str(employee)

# we can see the format of categorical variables are not in Factors and as they enter into statistical models 
# differently than continuous variables, storing data as factors insures that 
# the modeling functions will treat such data correctly.

#Converting variables into factors
columns_factor<- c(2,3,5,7,8,10:12,14:18,22,23,25,26,31)
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


#Fitting a Tree using Rpart
library(rpart)
fit <- rpart(Attrition ~ .,data=employee[,c(-9,-10)])


#Summarizing the parameters of the fit
summary(fit)

# Getting the best CP Value
printcp(fit)
plotcp(fit)


#Predicting the probabilities of the fit
predvalues<-predict(fit)


