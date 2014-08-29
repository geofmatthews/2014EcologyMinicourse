#######################################################
# Source Files for R Minicourse Part 3, Sept 2-3, 2014
# R. Matthews, Huxley College of the Environment               
# G. Matthews, Computer Science Department                     
# Western Washington University
#######################################################


#######################################################
# Linear Regression Confidence Intervals - Lakes Data
#######################################################
lakes = read.table("lakes.csv", T, sep=",")
attach(lakes)

### Step 1:  create linear model (chl ~ alk)
alkchl.lm = lm(log10(chl) ~ alk)

### Step 2:  sort the x axis (unique values only)
alk.sort = sort(unique(alk))

### Step 3:  use predict to predict chl ~ tp from linear model
pred.chl = predict(alkchl.lm, 
    newdata = data.frame(alk = alk.sort), int="pred")

### Step 4:  plot original data and linear model
op=par(mfrow=c(1,1))  ### plot single figure on a page

plot(log10(chl) ~ alk,
     xlab="Alkalinity (mg/L)",
     ylab=expression(paste("Log10 Chlorophyll " (mu * "g/L"))),
     pch=21, bg="skyblue", cex=1.5)
abline(alkchl.lm, lwd=2, lty=2, col="red")

### Step 5:  add upper and lower CI
lines(alk.sort, pred.chl[,2], lty=2) #lower CI
lines(alk.sort, pred.chl[,3], lty=2) #upper CI

### Step 6:  add a legend with the linear model statistics
legend(x="topleft", 
   c(paste("Log10 Chl =", round(alkchl.lm$coef[2],4), 
     "* Alk + ", round(alkchl.lm$coeff[1],4)),
    paste("Adj.R-squared =", round(summary(alkchl.lm)$adj.r.squared, 4)),
    paste("P-value =", round(summary(alkchl.lm)$coef[8], 4))),
    bty="n", cex=1) 



#######################################################
# Advanced Plotting - Transformed/Untransformed
# Linear Models Using Box-Cox Transformation Estimates
#######################################################
X <- c(1:10)
Y <- c(0.021,0.671,1.094,1.390,1.602,1.792,1.950,2.085,2.201,2.311)
### Y is ln(X) plus noise

Yfit <- lm(Y~X)

library(MASS)
Yfit.bc <- boxcox(Yfit)
Yfit.bc$x[which.max(Yfit.bc$y)]  ### result=2

##### Best est. for transformation will be Y^2
YfitBC.trans <- lm(Y^2 ~ X)


### Set plotting output to 4x per page
par(mfrow=c(2,2))

### Plot untransformed Y and residuals from Yfit
plot(Y ~ X, main="Y vs X, untransformed",  pch=21, bg="red", cex=1.5)
abline(Yfit, lwd=2, lty=2, col="red")
legend(x="topleft", c(paste("Y =", round(Yfit$coef[2],4),
         "* X + ", round(Yfit$coef[1], 4)),
         paste("Adj.R-squared =", round(summary(Yfit)$adj.r.squared, 4)),
         paste("p-value =", round(summary(Yfit)$coef[8], 4))),
         bty="n", cex=0.7)
plot(Yfit$fitted.values, resid(Yfit), 
     main="Untransformed residuals",
     pch=21, bg="red", cex=1.5,  xlab="Fitted Values", ylab="Residuals")
abline(h=0, lwd=2, lty=2, col="red")

### Plot transformed Y using Box-Cox estimate and YfitBC residuals
plot(Y^2 ~ X, main="Y^2 vs X", pch=21, bg="red", cex=1.5)
abline(YfitBC.trans, lwd=2, lty=2, col="red")
legend(x="topleft", c(paste("Y^2 =", round(YfitBC.trans$coef[2],4),
         "* X ", round(YfitBC.trans$coef[1], 4)),
         paste("Adj.R-squared =", round(summary(YfitBC.trans)$adj.r.squared, 4)),
         paste("p-value =", round(summary(YfitBC.trans)$coef[8], 4))),
         bty="n", cex=0.7)
plot(YfitBC.trans$fitted.values, resid(YfitBC.trans), 
     main="Transformed residuals",
     pch=21, bg="red", cex=1.5, xlab="Fitted Values", ylab="Residuals")
abline(h=0, lwd=2, lty=2, col="red")

