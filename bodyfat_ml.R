getwd()
setwd("set work dir")
bodyfat_raw <- read.csv("BODYFAT.csv")
dim(bodyfat_raw)
str(bodyfat_raw)
bodyfat_ds <- bodyfat_raw[,3:19]
dim(bodyfat_ds)
str(bodyfat_ds)

bodyfat_ds <- bodyfat_ds[,-2]

###############################

### Initialize libraries------------------------------------

library(e1071)  # for skewness

library(PerformanceAnalytics)  # for correlogram (chart.Correlation)

library(Hmisc)  # for missing value treatement (impute)

library(corrplot)  # for correlogram (corrplot)

library(party)  # selecting best variables (cforest)

library(Boruta)  # deciding if a variable is important (Boruta)

library(caret)  # for boxcox transformation (BoxCoxTrans)

library(car)  # for vif

library(DMwR)  # for knnImputation

library(DAAG)  # for cross validation

library(olsrr) # ols regression diagnositcs and multicollinearity

#library(relaimpo)  # for finding relative importance of predictors in lm mod

################### Split dataset ####################
set.seed(90) # Set Seed to reproduced sample in future
# Selecting 75% of data as sample from total 'n' rows
sample <- sample.int(n = nrow(bodyfat_ds), size = floor(.75*nrow(bodyfat_ds)), replace = F)
bodyfat_ds_train <- bodyfat_ds[sample, ]
bodyfat_ds_test  <- bodyfat_ds[-sample, ]
str(bodyfat_ds_train)

train.obscount <- nrow(bodyfat_ds_train)
train.obscount

test.obscount  <- nrow(bodyfat_ds_test)
test.obscount



######################################################


### Exploratory analysis -----------------------------------------------

# Generate plots: Density, Scatter, Box plots

# Set up your working directory here, to a location where you want to store plots.

for (k in names(bodyfat_ds_train)){
  
  png(file=paste(k,"_dens_scatter_box" ,".png", sep=""), width=900, height=550)
  
  x <- as.numeric (bodyfat_ds_train[, k])
  
  Skewness <- round(skewness(x), 2)  # calc skewness
  kurtosis <- round(kurtosis(x),2) # Calc Kurtosis
  
  dens <- density(x, na.rm=T)  # density func
  
  par(mfrow=c(1, 3))  # setup plot-area in 3 columns
  
  # Density plot
  
  plot(dens, type="l", col="red", ylab="Frequency", xlab = k, main = paste(k, ": Density Plot"), sub=paste("Skewness: ", Skewness,"  kurtosis: "   ,kurtosis))
  
  
  
  curve(dnorm(x, mean=mean(x,na.rm = TRUE), sd=sd(x,na.rm = TRUE)), 
        col="darkblue", lwd=2, add=TRUE, yaxt="n")
  lines(dens)
  
  #polygon(dens, col="white")
  
  # scatterplot
  
  plot(x, bodyfat_ds_train$PctBodyFat2, col="blue", ylab="PctBodyFat2", xlab = k, main = paste(k, ": Scatterplot"), pch=20)
  
  abline(response ~ x)
  
  # boxplot
  
  boxplot(x, main=paste(k, ": Box Plot"), sub=paste("Outliers: ", paste(boxplot.stats(x)$out, collapse=" ")))
  
  dev.off()
  
}

# Multiple Linear Regression
attach(bodyfat_ds_train)
fit <- lm(PctBodyFat2 ~ ., data=bodyfat_ds_train)
par(mfrow=c(1,2))
plot(fit)
names(fit)
summary(fit) # show results

fit <- lm(PctBodyFat2 ~ . -Weight, data=bodyfat_ds_train)
summary(fit) # show results
ols_vif_tol(fit)

fit <- lm(PctBodyFat2 ~ . -Adioposity -Abdomen -Weight, data=bodyfat_ds_train)
summary(fit) # show results
ols_vif_tol(fit)

#fit <- lm(PctBodyFat2 ~ . -Adioposity -Abdomen -Weight, data=bodyfat_ds_train)
#summary(fit) # show results
#ols_vif_tol(fit)


fit <- lm(PctBodyFat2 ~ . -Age -Adioposity -Abdomen -Weight -Biceps -Ankle  -Forearm -Wrist, data=bodyfat_ds_train)
summary(fit) # show results
ols_vif_tol(fit)


fit <- lm(PctBodyFat2 ~ . -Abdomen -Weight, data=bodyfat_ds_train)
summary(fit) # show results
ols_vif_tol(fit)

fit <- lm(PctBodyFat2 ~ . -FatFreeWt -Abdomen -Weight, data=bodyfat_ds_train)
summary(fit) # show results
ols_vif_tol(fit)

fit <- lm(PctBodyFat2 ~ . -Adioposity -FatFreeWt -Abdomen -Weight, data=bodyfat_ds_train)
summary(fit) # show results
ols_vif_tol(fit)

fit <- lm(PctBodyFat2 ~ Density , data=bodyfat_ds_train)
summary(fit) # show results


coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 

influence(fit) # regression diagnostics
par(mfrow=c(5,2))
ols_diagnostic_panel(fit)

#Influential Observation Analysis

#The leverage measures the amount by which the predicted value would change 
#if the observation was shifted one unit in the ydirection.The leverage always 
#takes values between 0 and 1. A point with zero leverage has no effect on the
#regression model. If a point has leverage equal to 1 the line must follow the
#point perfectly. 

lev = hat(model.matrix(fit))
par(mfrow=c(1,1))
plot(lev)
bodyfat_ds[lev >.5,]

# diagnostic plots 

# Assumptions

#1 The regression function is linear.
#2 The error terms have constant variance (homoscedastic).
#3 The error terms are independent.
#4 The error terms are normally distributed. : 

# Diagnostics

#2 Plot the residuals against both the predicted values and the explanatory variables
# The residuals should be randomly scattered about 0 and the width should be equal
# throughout for the constant variance assumption to hold. 
# ols_ovsp_plot
par(mfrow=c(5,3))
ols_ovsp_plot(fit)
par(mfrow=c(5,3))
for (k in names(bodyfat_ds_train)){
  
  png(file=paste(k,"_assumptions" ,".png", sep=""), width=900, height=550)
  
  x <- as.numeric (bodyfat_ds_train[, k])
  
  plot(x, fit$res)
   dev.off()
  
}



# plot(Density, fit$res)
# plot(Age , fit$res)
plot(fit$fitted, fit$res)

#3 The error terms are independent. 
# no patterns 

#4 A Normal probability plot of the residuals can be used to check the normality 
par(mfrow=c(1,3))
qqnorm(fit$res)
qqline(fit$res)
hist(fit$res)

# Assessing Collinearity (Variance inflation factors - VIF)

ols_vif_tol(fit) 

# or 

ols_coll_diag(fit)

# Fit the test data set 
##################################### Fiting the model ########################
Train.rmse = sqrt(sum(fit$residuals**2)/train.obscount)
train.obscount
Train.rmse
names(fit)

str(bodyfat_raw)

pred=predict(fit,newdata=bodyfat_ds_test)
test.obs=bodyfat_ds_test$PctBodyFat2
str(pred)
res=test.obs-pred
Test.rmse=sqrt(sum(res**2)/test.obscount)
test.obscount
Test.rmse

Test.rmse
Train.rmse
