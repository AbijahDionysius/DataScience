getwd();

Loands=read.csv("Loan Processing.csv",header = TRUE);
names(Loands);
attach(Loands);
Status=factor(Status,levels=c(0,1),labels=c("Reject Loan","Accept Loan"));
Status;
Model=glm(Status~Credit.Score,data=Loands,family=binomial)
library(lmtest)
lrtest(Model)
library(pscl)
# Compute various Pseudo R squares 
pR2(Model)
summary(Model)
confint(Model)
exp(coef(Model))
#exp(coef(Model))/(1-exp(coef(Model)))
#exp(confint(Model))
predict=predict(Model,type="response")
library(Deducer)
rocplot(Model)

pred = fitted(Model)
pred
data.frame(Status,pred)
# Cut off points 0.50
gg1 = floor(pred)
table(Actual=Status,Prediction=gg1)

