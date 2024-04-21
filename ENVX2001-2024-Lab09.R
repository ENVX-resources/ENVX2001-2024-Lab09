## ----setup, include=FALSE-------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)


## ----readingData----------------------------------------------------------------------------------------------
library(readxl)
Dippers <- read_xlsx("mlr.xlsx" , sheet = "Dippers")
glimpse(Dippers)


## -------------------------------------------------------------------------------------------------------------
#| eval: false
## FullMod <- lm(Br_Dens ~ ., data=Dippers)
## RedMod <- step(FullMod, direction = "backward")
## summary(FullMod)
## summary(RedMod)
## AIC(FullMod, RedMod)


## ----VarSelectSolution, echo = T, eval = T--------------------------------------------------------------------

FullMod <- lm(Br_Dens ~ ., data=Dippers)
RedMod <- step(FullMod, direction = "backward", trace = 0) # backward selection

# Plot as table (a good trick for reports!)
compare <- AIC(FullMod, RedMod) %>% # get AIC data frame
  # then, add adjusted r-squared values in a new column using `mutate()`
  mutate(adj_r2 = c(summary(FullMod)$adj.r.squared, summary(RedMod)$adj.r.squared)) %>%
  # then, round all numeric values to 2 decimal places
  mutate(across(where(is.numeric), \(x) round(x, 2)))

knitr::kable(compare)


## -------------------------------------------------------------------------------------------------------------
summary(FullMod)
summary(RedMod)
AIC(FullMod, RedMod)


## ----random_number--------------------------------------------------------------------------------------------
stream <- read_xlsx("mlr.xlsx", "California_streamflow")


## -------------------------------------------------------------------------------------------------------------
fit <- lm(L10BSAAM ~ L10OPRC + L10OBPC, data = stream)


## -------------------------------------------------------------------------------------------------------------
set.seed(100) # to make sure everybody gets the same results

# this generates the random number into the dataset
stream$random_no <- rnorm(n = nrow(stream), mean = 3, sd = 2) 


## ----randno_solution, eval = T, echo = T----------------------------------------------------------------------
mod_rand1 <- lm(L10BSAAM ~ L10OPRC + L10OBPC,data = stream)
summary(mod_rand1)

mod_rand2 <- lm(L10BSAAM ~ L10OPRC + L10OBPC + random_no ,data = stream)
summary(mod_rand2)


## ----readDataStream-------------------------------------------------------------------------------------------
# read in the data
require(readxl)
stream <- read_xlsx("mlr.xlsx", "California_streamflow")
# best model
ML_Mod2 <- lm(L10BSAAM ~ L10OPRC + L10OBPC, data = stream)
summary(ML_Mod2)


## ----lake_predict---------------------------------------------------------------------------------------------
# create a new data frame with the variables to predict at
# Note that it does not matter what you put in for L10BSAAM
new.df <- data.frame(L10BSAAM = 0, L10OPRC = 2, L10OBPC = 3)

# now use predict() and specify se.fit=T
predLake <- predict(ML_Mod2, newdata = new.df, se.fit = TRUE)

# the output now has two elements:
# the fit
predLake$fit
# the se of the fit
predLake$se.fit


## The se.fit is within the range of the response variable
## ----Lake_predict_fit, echo = T, eval = T---------------------------------------------------------------------
predLake$fit
range(stream$L10BSAAM)


## ----newdf----------------------------------------------------------------------------------------------------
new.df <- data.frame(L10OPRC = seq(3.0, 4.0, length = 5),
                     L10OBPC = seq(3.0, 4.0, length = 5))


## ----Lake_predict_PI, echo = T, eval = T----------------------------------------------------------------------
Lake_predict <- predict(ML_Mod2, newdata=new.df, interval="prediction")
Lake_predict


## ----sampledata-----------------------------------------------------------------------------------------------
#Only use so we are all get same random numbers - 
# otherwise R uses computer time to get random number
set.seed(10)

#Sample 20% of the rows, find row numbers
index <- sample(1:nrow(stream), size = 0.20*nrow(stream))
#Split data
valid <- stream[index,]
dim(valid)
calib <- stream[-index,]
dim(calib)


## ----DefineModels---------------------------------------------------------------------------------------------
# use model 2 and model 3 from topic 7/8 practical (last week)
# and test which one is the best model, but use calib data
ML_Mod2 <- lm(L10BSAAM ~ L10OPRC + L10OBPC, data = calib)
ML_Mod3 <- lm(L10BSAAM ~ L10OPRC + L10OBPC + L10APSAB, data = calib)

# compare the models
summary(ML_Mod2)
summary(ML_Mod3)


## ----ResidualMod2, fig.cap="Residual plots for Model with 2 parameters"---------------------------------------
par(mfrow=c(2,2))
plot(ML_Mod2)
par(mfrow=c(1,1))

## ----ResidualMod3, fig.cap="Residual plots for Model with 3 parameters"---------------------------------------
par(mfrow=c(2,2))
plot(ML_Mod3)
par(mfrow=c(1,1))



## ----validExample---------------------------------------------------------------------------------------------
predict(ML_Mod2, newdata = valid)


## **ANSWER**
## 
## Bias and RMSE are low and seem to be more optimal for ML_Mod3
## ----accuracySolution, eval=T, echo=T-------------------------------------------------------------------------
# Accuracy (RMSE)
# Calibrations
(RMSE2 <- sqrt(mean((calib$L10BSAAM - predict(ML_Mod2))^2)))
(RMSE3 <- sqrt(mean((calib$L10BSAAM - predict(ML_Mod3))^2)))

# now validations
(RMSE2_valid <- sqrt(mean((valid$L10BSAAM - predict(ML_Mod2, newdata = valid))^2)))
(RMSE3_valid <- sqrt(mean((valid$L10BSAAM - predict(ML_Mod3, newdata = valid))^2)))

#Bias
(Bias2 <- mean(calib$L10BSAAM - predict(ML_Mod2)))
(Bias3 <- mean(calib$L10BSAAM - predict(ML_Mod3)))
# very small, as they should be!

(Bias2_valid <- mean(valid$L10BSAAM - predict(ML_Mod2, newdata = valid)))
(Bias3_valid <- mean(valid$L10BSAAM - predict(ML_Mod3, newdata = valid)))



## ----PlotObsPred, fig.cap = "Plots of predicted versus observed for model with 2 predictors (top) and model with 3 predictors (bottom)", fig.height = 9, fig.width = 5----

# plot predicted versus observed
par(mfrow = c(2,1), mar=c(4,4,3,2))

# model 2
plot(calib$L10BSAAM, predict(ML_Mod2), 
     # colour = red, type = "16" and size is 20% larger
     pch = 16, col = "blue", cex = 1.2,
     # add titles for axes and main
     xlab = "Observed dataset", ylab = "Predicted",
     main="model 2 predictors")
# insert a 1:1 line, dashed line, width = 2
abline(0, 1, lty = 2, lwd = 2)

# add the validation data
points(valid$L10BSAAM, predict(ML_Mod2, newdata = valid), 
       # colour = blue, type = "16" and size is 20% larger
       col = "red", pch = 16, cex = 1.2)
# add a legend to the first plot
legend("topleft", c("calibration", "validation", "1 : 1 line"), 
       pch = c(16, 16, NA), lty = c(NA, NA, 2), col = c("blue", "red", 1),
       # 20% smaller
       cex=0.8)

# model 3
plot(calib$L10BSAAM, predict(ML_Mod3), 
     # colour = red, type = "16" and size is 20% larger
     pch = 16, col = "blue", cex = 1.2,
     # add titles for axes and main
     xlab = "Observed dataset", ylab = "Predicted",
     main="model 3 predictors")
# insert a 1:1 line, dashed line, width = 2
abline(0, 1, lty = 2, lwd = 2)
# add the validation data
points(valid$L10BSAAM, predict(ML_Mod3, newdata = valid), 
       # colour = blue, type = "16" and size is 20% larger
       col = "red", pch = 16, cex = 1.2)


## ----correlation, eval = T, echo = T--------------------------------------------------------------------------
#Correlation
cor(calib$L10BSAAM, predict(ML_Mod2))
cor(calib$L10BSAAM, predict(ML_Mod3))
# we already know this from summary()

cor(valid$L10BSAAM, predict(ML_Mod2, newdata = valid))
cor(valid$L10BSAAM, predict(ML_Mod3, newdata = valid))
# confirms bias and RMSE calculations

# r2
(cor(calib$L10BSAAM, predict(ML_Mod2))^2)
(cor(calib$L10BSAAM, predict(ML_Mod3))^2)

(cor(valid$L10BSAAM, predict(ML_Mod2, newdata = valid)))^2
(cor(valid$L10BSAAM, predict(ML_Mod3, newdata = valid)))^2


## ----LinsConcord, echo = T, eval = T, warnings = FALSE--------------------------------------------------------
#if needed
#install.packages("epiR")
library(epiR, quietly = TRUE)

calib.lcc<-epi.ccc(calib$L10BSAAM, predict(ML_Mod2))
calib.lcc$rho.c
valid.lcc<-epi.ccc(valid$L10BSAAM, predict(ML_Mod2, newdata = valid))
valid.lcc$rho.c

calib.lcc<-epi.ccc(calib$L10BSAAM, predict(ML_Mod3))
calib.lcc$rho.c
valid.lcc<-epi.ccc(valid$L10BSAAM, predict(ML_Mod3, newdata = valid))
valid.lcc$rho.c



## 
## Here we are looking for a summary of your findings. We would say something like:
## 
## Evident from a slightly higher adjusted *r*^2^ (*r*^2^-adj = 0.878 for ML_Mod2, 0.880 for ML_Mod3), ML_Mod3 is a more suitable model to predict variable stream runoff volume at the selected site near Bishop, California (L10BSAAM; measured in ML/year).
## 
## To support our conclusion, we investigated further by comparing accuracy between the two models, in terms of RMSE, Bias, and LCCC. Bias and RMSE are low and seem to be more optimal for ML_Mod3 (Bias = -0.037 ML/year and RMSE = 0.047 ML/year). Furthermore, the LCCC is higher/closer to 1 for ML_Mod3 compared to ML_Mod2 (LCCC = 0.873, LCCC = 0.862, for ML_Mod3 and ML_Mod2, respectively).
## 
## **Note:**
## 1. Bias and RMSE are in the same units as the response variable, so we provide units when reporting the values.
## 2. We have mainly reported the validation statistics in the summary; however we still need calibration statistics so we can compare the two (e.g. RMSE from calib vs RMSE from valid) and assess how well the model predicts, and identify whether there is overfitting. If the model is overfitting, we would see a difference between the model fit statistics between calib and valid datasets. In general, model fit will usually be lower in the predictions.
## 
## **Model 2: L10BSAAM ~ L10OPRC + L10OBPC**
## 
## Model fit :
## 
## Bias (calib) = 5.075315e-16
## 
## Bias (valid) = -0.0346165
## 
## RMSE (calib) = 0.0479611
## 
## RMSE(valid) = 0.04899388
## 
## Calib LCCC= 0.9390808
## 
## Valid LCCC = 0.8620478	
## 
## **Model 3:L10BSAAM ~ L10OPRC + L10OBPC + L10APSAB**
## 
## Model fit:
## 
## Bias (calib) = -4.060266e-16
## 
## Bias (valid) = -0.03694093
## 
## RMSE (calib) = 0.04677635
## 
## RMSE(valid) = 0.04734575
## 
## Calib LCCC = 0.9422251
## 
## Valid LCCC = 0.8732522
## 
