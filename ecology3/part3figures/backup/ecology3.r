##### This file produces regression examples

x  <- c(1:15)
y1 <- c(2,4,7,9,12,10,22,16,22,30,27,42,25,30,60)
y2 <- c(2,5,6,11,15,14,18,20,25,26,28,26,30,35,38)
y3 <- c(2,4,8,18,32,64,128,256,500,1000,2000,4000,8000,16000,32000)
y4 <- c(25,10,5,5,2,0,0,7,9,15,21,34,50,65,75)

######## REGRESSION PLOT #1 (NO STATS)
##### Plots of y1-y4:
op <- par(mfrow=c(2,2))
plot(x, y1, pch=21, cex=1.5, bg="blue", 
     main="Linear Relationship", xlab=" ")
y1fit <- lm(y1 ~ x)
abline(y1fit, col="blue", lty=2)

plot(x, y2, pch=21, cex=1.5, bg="blue", 
     main="Linear Relationship", xlab=" ")
y2fit <- lm(y2 ~ x)
abline(y2fit, col="blue", lty=2)

plot(x, y3, pch=21, cex=1.5, bg="blue", 
     main="Monotonic, Nonlinear Relationship", xlab=" ")
y3fit <- lm(y3 ~ x)
abline(y3fit, col="blue", lty=2)

plot(x, y4, pch=21, cex=1.5, bg="blue", 
     main="Nonlinear Relationship", xlab=" ")
y4fit <- lm(y4 ~ x)
abline(y4fit, col="blue", lty=2)
dev.print (postscript, "regression1.ps", horizontal=F, paper="letter")



######## REGRESSION PLOT #2 - added stats
##### Plots of y1-y4:
op <- par(mfrow=c(2,2))
plot(x, y1, pch=21, cex=1.5, bg="blue", 
     main="Linear Relationship", xlab=" ")
y1fit <- lm(y1 ~ x)
abline(y1fit, col="blue", lty=2)
legend(x="topleft", 
          c("r2 = 0.78", "r = 0.89", "rho = 0.95", "tau = 0.86"),
          text.col="red", bty="n")
##### Summary statistics for y1
summary(y1fit)
cor.test(x, y1, method="pearson")
cor.test(x, y1, method="spearman")
cor.test(x, y1, method="kendall")
### edited output - y1
### Adjusted R-squared: 0.7787 
### Pearson's 0.8913725 
### Spearman's rho 0.951701 
### Kendall's tau 0.8558088 

plot(x, y2, pch=21, cex=1.5, bg="blue", 
     main="Linear Relationship", xlab=" ")
y2fit <- lm(y2 ~ x)
abline(y2fit, col="blue", lty=2)
legend(x="topleft", 
          c("r2 = 0.97", "r = 0.99", "rho = 0.99", "tau = 0.96"),
          text.col="red", bty="n")
##### Summary statistics for y2
summary(y2fit)
cor.test(x, y2, method="pearson")
cor.test(x, y2, method="spearman")
cor.test(x, y2, method="kendall")
summary(y2fit)
### edited output - y2
### Adjusted R-squared: 0.9748
### Pearson's 0.9882384
### Spearman's rho 0.9901702
### Kendall's tau 0.9569488 



plot(x, y3, pch=21, cex=1.5, bg="blue", 
     main="Monotonic, Nonlinear Relationship", xlab=" ")
y3fit <- lm(y3 ~ x)
abline(y3fit, col="blue", lty=2)
legend(x="topleft", 
          c("r2 = 0.44", "r = 0.69", "rho = 1.00", "tau = 1.00"),
          text.col="red", bty="n")
##### Summary statistics for y3
summary(y3fit)
cor.test(x, y3, method="pearson")
cor.test(x, y3, method="spearman")
cor.test(x, y3, method="kendall")
### edited output - y3
### Adjusted R-squared: 0.4423
### Pearson's 0.6943927
### Spearman's rho 1.0
### Kendall's tau 1.0


plot(x, y4, pch=21, cex=1.5, bg="blue", 
     main="Nonlinear Relationship", xlab=" ")
y4fit <- lm(y4 ~ x)
abline(y4fit, col="blue", lty=2)
legend(x="topleft", 
          c("r2 = 0.51", "r = 0.74", "rho = 0.64", "tau = 0.51"),
          text.col="red", bty="n")
##### Summary statistics for y4
summary(y4fit)
cor.test(x, y4, method="pearson")
cor.test(x, y4, method="spearman")
cor.test(x, y4, method="kendall")

### edited output - y4
### Adjusted R-squared: 0.5136 
### Pearson's 0.7405304
### Spearman's rho 0.6368525 
### Kendall's tau 0.5096389 

dev.print (postscript, "regression2.ps", horizontal=F, paper="letter")
