############################################# 
#-------- Assignment 2 - Momentum ----------#
#############################################

# load packages
library(readr) 
library(dplyr) 
library(stargazer) 
library(na.tools) 
library(lmtest) 
library(car) 
library(magrittr) 
library(robustHD) 
library(DescTools)


##############################################
#---------------- Task 1 --------------------#
##############################################

# First we download, load, and investigate the data:

# Returns based on size

return_size <- read.csv("equity_size_portfolios_monthly_vw.csv")

# Returns sorted on book-to-market (BM)
return_BM <- read.csv("book_market_portfolios_monthly_vw.csv")

# Pricing factors including momentum (UMD)
pricing_factors <- read.csv("pricing_factors_monthly.csv")


# Overview
#View(return_size)
str(return_size, give.attr = FALSE) # Date is in wrong format (integer)
summary(return_size)

#View(return_BM)
str(return_BM, give.attr = FALSE) # Date is in wrong format (integer)
summary(return_BM)

#View(pricing_factors)
str(pricing_factors, give.attr = FALSE) # Date is in wrong format (integer)
summary(pricing_factors)


# Changing the date formats
return_size <- transform(return_size, date=as.Date(as.character(date), "%Y%m%d")) 

return_BM <- transform(return_BM, date= as.Date(as.character(date), "%Y%m%d")) 

pricing_factors <- transform(pricing_factors, date=as.Date(as.character(date), "%Y%m%d"))

# Are there any outliers?
summary(pricing_factors)


##############################################
#---------------- Task 2 --------------------#
##############################################

# Each portfolio's exposure to the momentum (UMD) factor:
# Starting with a crucial component, we have to merge the pricing factors with the size and BM data frames, to
# create two new data frames.
# Another crucial step is to remove NAs, since they will disturb our further analysis. 

# Merging the data frames to create two new data frames
size_pricing_fac <- left_join(return_size,pricing_factors)
BM_pricing_fac <- left_join(return_BM,pricing_factors)

# Removing NAs
size_pricing_fac <- na.rm(size_pricing_fac)
BM_pricing_fac <- na.rm(BM_pricing_fac)

for(i in 2:15){ 
  boxplot(size_pricing_fac[,i], main = i)
}

# Winsorizing
for(i in 2:15){
  size_pricing_fac[,i] <- winsorize(size_pricing_fac[,i],probs=c(0.01,0.99)) 
  }

summary(size_pricing_fac)

for(i in 2:15){ 
  boxplot(size_pricing_fac[,i], main = i)
}

for(i in 2:15){ 
  hist(size_pricing_fac[,i], main = i)
}

# With the cleaned up and ordered data we can save the coefficients to an object
coef.vector <- rep(NA, 10)

print(coef.vector)

# iteration
for(i in 1:10){
  # construct relevant data
  relevant.data <- data.frame(size_pricing_fac[i+1],size_pricing_fac$UMD)
  colnames(relevant.data) <- c("Return","UMD")
    
  # regression
  fit <- lm(Return ~ UMD, data = relevant.data)
  
  # save coefficient
  coef.vector[i] <- fit$coefficient["UMD"]
}

print(coef.vector)


##############################################
#---------------- Task 3 --------------------#
##############################################

# Plot of each individual size portfolio's exposure and average return

avg.vector <- rep(NA,10)

for(i in 1:10){
  relevant.col = size_pricing_fac[i+1]
  colnames(relevant.col) <- c("Return")
  avg.vector[i] <- mean(relevant.col$Return)
}

print(avg.vector)
summary(avg.vector)

plot(coef.vector,avg.vector)

# Is there difference in the portfolios?
pred <- tibble(UMD = seq(min(size_pricing_fac$UMD, na.rm = TRUE), max(size_pricing_fac$UMD, na.rm = TRUE),
                         length.out = 10000))

# Fits
fit1.size <- lm(X1..small.size. ~ UMD, data = size_pricing_fac) 
fit2.size <- lm(X2 ~ UMD, data = size_pricing_fac)
fit3.size <- lm(X3 ~ UMD, data = size_pricing_fac)
fit4.size <- lm(X4 ~ UMD, data = size_pricing_fac)
fit5.size <- lm(X5 ~ UMD, data = size_pricing_fac)
fit6.size <- lm(X6 ~ UMD, data = size_pricing_fac)
fit7.size <- lm(X7 ~ UMD, data = size_pricing_fac)
fit8.size <- lm(X8 ~ UMD, data = size_pricing_fac)
fit9.size <- lm(X9 ~ UMD, data = size_pricing_fac)
fit10.size <- lm(X10..large.size. ~ UMD, data = size_pricing_fac)

# Preds
pred$x1 <- predict(fit1.size, pred)
pred$x2 <- predict(fit2.size, pred) 
pred$x3 <- predict(fit3.size, pred) 
pred$x4 <- predict(fit4.size, pred) 
pred$x5 <- predict(fit5.size, pred) 
pred$x6 <- predict(fit6.size, pred) 
pred$x7 <- predict(fit7.size, pred) 
pred$x8 <- predict(fit8.size, pred) 
pred$x9 <- predict(fit9.size, pred) 
pred$x10 <- predict(fit10.size, pred)

# Plot
ggplot(data = pred, aes(x = (UMD))) +
  geom_line(aes(y =(x1), colour = "Portfolio.1")) + 
  geom_line(aes(y =(x2), colour = "Portfolio.2")) + 
  geom_line(aes(y =(x3), colour = "Portfolio.3")) + 
  geom_line(aes(y =(x4), colour = "Portfolio.4")) + 
  geom_line(aes(y =(x5), colour = "Portfolio.5")) + 
  geom_line(aes(y =(x6), colour = "Portfolio.6")) + 
  geom_line(aes(y =(x7), colour = "Portfolio.7")) + 
  geom_line(aes(y =(x8), colour = "Portfolio.8")) + 
  geom_line(aes(y =(x9), colour = "Portfolio.9")) + 
  geom_line(aes(y =(x10), colour = "Portfolio.10")) + 
  ylab("Return")


##############################################
#---------------- Task 4 --------------------#
##############################################

# This is a cross-sectional regression of the returns of the size portfolio for each month in the sample.

# We will first create a gamma coefficient for the Fama-Macbeth second order regression
gamma.coef <- rep(NA,nrow(size_pricing_fac))
print(gamma.coef)

for(i in 1:nrow(size_pricing_fac)){
  #constructing relevant data
  ind.month <- t(data.frame(size_pricing_fac[i,]))          #Select and transform monthly data to individual columns
  ind.month <- ind.month[-c(1,12:15),]                      #Remove the date and factor variables
  return.coef <- data.frame(ind.month,coef.vector)          #Create data frame with monthly returns and coefficients
  colnames(return.coef) <- c("Monthly_return","Gamma_coefficients")
  print(return.coef)
  #regression
  fit <- lm(Monthly_return~Gamma_coefficients, data=return.coef)
  
  #saving the gamma coefficient
  gamma.coef[i] <- fit$coefficient["Gamma_coefficients"]
}

# Are the coefficients normally distributed?
hist(gamma.coef)

plot.ts(gamma.coef)

print(gamma.coef)


##############################################
#---------------- Task 5 --------------------#
##############################################

# Testing whether the average is positive and statistically different from zero:

t.test <- t.test(gamma.coef,
                 mu = 0,
                 alternative = "two.sided")

print(t.test)                           # p-value = 0.003742
                                        # Interpretation of the p-value:
                                        # The p-value is lower than 5%, and it follows that
                                        # the true mean does not equal 0.
                                        # This indicates that stocks exposed to momentum earn a
                                        # higher return. 


##############################################
#---------------- Task 6 --------------------#
##############################################

# Next, we will repeat the above analysis in tasks 2-5, only with the 10 BM portfolios.


###----- STEP 1 - Each portfolio's exposure to the momentum (UMD) factor -----###

for(i in 2:15){ 
  boxplot(BM_pricing_fac[,i], main = i)
}

# Winsorizing
for(i in 2:15){
  BM_pricing_fac[,i] <- winsorize(BM_pricing_fac[,i],probs=c(0.01,0.99)) 
}

summary(BM_pricing_fac)

for(i in 2:15){ 
  boxplot(BM_pricing_fac[,i], main = i)
}

for(i in 2:15){ 
  hist(BM_pricing_fac[,i], main = i)
}

# With the cleaned up and ordered data we can save the 10 coefficients to a new object
coef.vec2 <- rep(NA, 10)

# iteration
for(i in 1:10){
  # construct relevant data
  rel.data2 <- data.frame(BM_pricing_fac[i+1],BM_pricing_fac$UMD)
  colnames(rel.data2) <- c("Return","UMD")
  
  # regression
  fit <- lm(Return ~ UMD, data = rel.data2)
  
  # save coefficient
  coef.vec2[i] <- fit$coefficient["UMD"]
}

print(coef.vec2)



###----- STEP 2 - Plot of each individual size portfolio's average return -----###

avg.vec2 <- rep(NA,10)

for(i in 1:10){
  rel.col2 = return_BM[i+1]
  colnames(rel.col2) <- c("Return")
  avg.vec2[i] <- mean(rel.col2$Return)
}

print(avg.vec2)

plot(coef.vec2,avg.vec2)

# Is there difference in the portfolios?
pred2 <- tibble(UMD = seq(min(BM_pricing_fac$UMD,na.rm = TRUE), max(BM_pricing_fac$UMD,na.rm = TRUE), 
                         length.out = 10000))

# Fits
fit1.BM <- lm(X1..low.bm. ~ UMD, data = BM_pricing_fac) 
fit2.BM <- lm(X2 ~ UMD, data = BM_pricing_fac)
fit3.BM <- lm(X3 ~ UMD, data = BM_pricing_fac)
fit4.BM <- lm(X4 ~ UMD, data = BM_pricing_fac)
fit5.BM <- lm(X5 ~ UMD, data = BM_pricing_fac)
fit6.BM <- lm(X6 ~ UMD, data = BM_pricing_fac)
fit7.BM <- lm(X7 ~ UMD, data = BM_pricing_fac)
fit8.BM <- lm(X8 ~ UMD, data = BM_pricing_fac)
fit9.BM <- lm(X9 ~ UMD, data = BM_pricing_fac)
fit10.BM <- lm(X10..high.bm. ~ UMD, data = BM_pricing_fac)

# Preds
pred2$x1 <- predict(fit1.BM, pred2)
pred2$x2 <- predict(fit2.BM, pred2) 
pred2$x3 <- predict(fit3.BM, pred2) 
pred2$x4 <- predict(fit4.BM, pred2) 
pred2$x5 <- predict(fit5.BM, pred2) 
pred2$x6 <- predict(fit6.BM, pred2) 
pred2$x7 <- predict(fit7.BM, pred2) 
pred2$x8 <- predict(fit8.BM, pred2) 
pred2$x9 <- predict(fit9.BM, pred2) 
pred2$x10 <- predict(fit10.BM, pred2)

# Plot
ggplot(data = pred2, aes(x = (UMD))) +
  geom_line(aes(y =(x1), colour = "Portfolio.1")) + 
  geom_line(aes(y =(x2), colour = "Portfolio.2")) + 
  geom_line(aes(y =(x3), colour = "Portfolio.3")) + 
  geom_line(aes(y =(x4), colour = "Portfolio.4")) + 
  geom_line(aes(y =(x5), colour = "Portfolio.5")) + 
  geom_line(aes(y =(x6), colour = "Portfolio.6")) + 
  geom_line(aes(y =(x7), colour = "Portfolio.7")) + 
  geom_line(aes(y =(x8), colour = "Portfolio.8")) + 
  geom_line(aes(y =(x9), colour = "Portfolio.9")) + 
  geom_line(aes(y =(x10), colour = "Portfolio.10")) + 
  ylab("Return")


###----- STEP 3 - Cross-sectional regression -----###

gamma.coef2 <- rep(NA,nrow(BM_pricing_fac))
print(gamma.coef2)

for(i in 1:nrow(BM_pricing_fac)){
  #constructing relevant data
  ind.month2 <- t(data.frame(BM_pricing_fac[i,]))
  ind.month2 <- ind.month2[-c(1,12:15),]
  return.coef2 <- data.frame(ind.month2,coef.vec2)
  colnames(return.coef2) <- c("Monthly_return","Gamma_coefficients")
  print(return.coef2)
  #regression
  fit <- lm(Monthly_return~Gamma_coefficients, data=return.coef2)
  
  #saving the gamma coefficient
  gamma.coef2[i] <- fit$coefficient["Gamma_coefficients"]
}

# Are the coefficients normally distributed?
hist(gamma.coef2, breaks = 10)

plot.ts(gamma.coef2)

print(gamma.coef2)


###----- STEP 4 - Two-sided t-test -----###

# Testing whether the average is positive and statistically different from zero:

t.test2 <- t.test(gamma.coef2,
                 mu = 0,
                 alternative = "two.sided")

print(t.test2)                              # p-value = 0.3759
                                            # Interpretation of the p-value:
                                            # Non-significant p-value at 37.59%. This means that
                                            # the null hypothesis is accepted. 





