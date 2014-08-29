op=par(mfrow=c(1,1))

#### Box-Cox procedure to find best power transformation (SPSS m-factor)
X = c(1:10)
Y = c(0.021,0.671,1.094,1.390,1.602,1.792,1.950,2.085,2.201,2.311)
Yfit = lm(Y~X)

library(MASS)
Yfit.bc = boxcox(Yfit)
Yfit.bc$x[which.max(Yfit.bc$y)]
YfitBC.trans <- lm(Y^2 ~X)

### Y with Y^2 fit:
op=par(mfrow=c(2,2))
plot(Y ~ X, main="Y vs X, untransformed",
     pch=21, bg="red", cex=1.5)
abline(Yfit, lwd=2, lty=2, col="red")
legend(x="topleft", c(paste("Y =", round(Yfit$coef[2],4),
         "* X + ", round(Yfit$coef[1], 4)),
         paste("Adj.R-squared =", round(summary(Yfit)$adj.r.squared, 4)),
         paste("p-value =", round(summary(Yfit)$coef[8], 4))),
         bty="n", cex=0.7)
plot(Yfit$fitted.values, resid(Yfit), main="Untransformed residuals",
     pch=21, bg="red", cex=1.5,
     xlab="Fitted Values", ylab="Residuals")
abline(h=0, lwd=2, lty=2, col="red")

plot(Y^2 ~ X, main="Y^2 vs X",
     pch=21, bg="red", cex=1.5)
abline(YfitBC.trans, lwd=2, lty=2, col="red")
legend(x="topleft", c(paste("Y^2 =", round(YfitBC.trans$coef[2],4),
         "* X ", round(YfitBC.trans$coef[1], 4)),
         paste("Adj.R-squared =", round(summary(YfitBC.trans)$adj.r.squared, 4)),
         paste("p-value =", round(summary(YfitBC.trans)$coef[8], 4))),
         bty="n", cex=0.7)
plot(YfitBC.trans$fitted.values, resid(YfitBC.trans), 
     main="Transformed residuals",
     pch=21, bg="red", cex=1.5,
     xlab="Fitted Values", ylab="Residuals")
abline(h=0, lwd=2, lty=2, col="red")
dev.print(postscript, "Y2bc.ps", horizontal=F, paper="letter")
par(op)



### Y with exp fit:
Yfit = lm(Y~X)                ### no change
Yexp.trans <- lm(exp(Y) ~X)   ### exponential fit

op=par(mfrow=c(2,2))
plot(Y ~ X, main="Y vs X, untransformed",
     pch=21, bg="red", cex=1.5)
abline(Yfit, lwd=2, lty=2, col="red")
legend(x="topleft", c(paste("Y =", round(Yfit$coef[2],4),
         "* X + ", round(Yfit$coef[1], 4)),
         paste("Adj.R-squared =", round(summary(Yfit)$adj.r.squared, 4)),
         paste("p-value =", round(summary(Yfit)$coef[8], 4))),
         bty="n", cex=0.7)
plot(Yfit$fitted.values, resid(Yfit), main="Untransformed residuals",
     pch=21, bg="red", cex=1.5,
     xlab="Fitted Values", ylab="Residuals")
abline(h=0, lwd=2, lty=2, col="red")

plot(exp(Y) ~ X, main="exp(Y) vs X",
     pch=21, bg="red", cex=1.5)
abline(Yexp.trans, lwd=2, lty=2, col="red")
legend(x="topleft", c(paste("exp(Y) =", round(Yexp.trans$coef[2],4),
         "* X ", round(Yexp.trans$coef[1], 4)),
         paste("Adj.R-squared =", round(summary(Yexp.trans)$adj.r.squared, 4)),
         paste("p-value =", round(summary(Yexp.trans)$coef[8], 4))),
         bty="n", cex=0.7)
plot(Yexp.trans$fitted.values, resid(Yexp.trans), main="Transformed residuals",
     pch=21, bg="red", cex=1.5,
     xlab="Fitted Values", ylab="Residuals")
abline(h=0, lwd=2, lty=2, col="red")

dev.print(postscript, "Y2exp.ps", horizontal=F, paper="letter")
par(op)
