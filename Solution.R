# ***********************************************************Libraries**********************************************************
rm(list = ls())
library(ggplot2)
library(dplyr)
library(gridExtra)
library(DMwR)
library(caTools)
library(pROC)
library(Hmisc)

source("UserFunctions.R")
# *********************************************************Exploratory Data Analysis********************************************************
# Read Data
sdata = read.csv("Logit-Simmons.csv", header=TRUE, stringsAsFactors = FALSE)

# Structure of Data frame
str(sdata) 
summary(sdata)


by(data = sdata$Spending, FUN = summary, INDICES = as.factor(sdata$Purchase))
# histConNames = "Spending"
# histCatNames = names(sdata[,c(3,4)])
# sdata.hist.cat = lapply(histCatNames, FUN = fn_Hist_Plot_Cat, histdata = sdata)
# sdata.hist.con = lapply(histConNames, FUN = fn_Hist_Plot_Con, histdata = sdata)

sdata.hist.Spend_purchase = fn_Hist_Plot_ConGroup(xnames = "Spending", histdata = sdata, hgroup = "Purchase")
sdata.box.con.Spend_purchase = fn_BoxPlotConGroup(y_names = "Spending", bdata = sdata, x_var = "Purchase")
sdata.hist.Card_purchase = fn_Hist_Plot_CatGroup(xnames = "Card", histdata = sdata, hgroup = "Purchase")
plot(sdata.hist.Spend_purchase)

plot(sdata.box.con.Spend_purchase)

plot(sdata.hist.Card_purchase)
# sdata.box.con.Spend_purchase
# plot = list(sdata.hist.Spend_purchase, sdata.box.con.Spend_purchase, sdata.hist.Card_purchase)
# do.call("grid.arrange", c(plot, ncol=3))
 
# After some EDA, we'll go to modelling.
# We'll convert categorical variables as factors.
sdata$Purchase = as.factor(sdata$Purchase)

glm.model = glm(Purchase ~ Card + Spending, data = sdata, family = binomial)

# Summary of model
summary(glm.model)

# We can see both variables are significant. 

confint(glm.model)
exp(coef(glm.model))
exp(confint(glm.model))

# Performance of Logistic Regression Model
# AIC : This is used as a metric when comparing multiple models. Model with lowest AIC is preferred. 
AIC(glm.model)


predictTest = predict(glm.model, newdata = sdata, type = "response")
# sdata$Predicted_Purchase = predictTest
 
predictDecision = predictTest > 0.5

confusion_matrix = table(sdata$Purchase, predictDecision)
confusion_matrix

# Accuracy of Model
Accuracy = (20+52)/100
# Model accuracy is 0.72

# Misclassification Rate
MisClassficationRate = (20+8)/100
# Model MisClassfication Rate is 0.28

# True Positive Rate
TPR = 20/40
# Model True Postivie Rate is 0.5

# False Positive Rate
FPR = 8/60
FPR
# Model False Positive Rate is 0.133

# Specificity
SPC = 52/60
# Model Specificity is 0.867

# Precision
PRSN = 20/28
# Model Precision is 0.714

# Roc Curve
theprobs=fitted(glm.model)
gg=floor(theprobs+0.5)

model.roc<-roc(Purchase~gg, data=sdata)
plot(model.roc)

 