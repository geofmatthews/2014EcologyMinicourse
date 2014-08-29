lakes = read.table("lakes.csv", T, sep=",")
attach(lakes)

### ADVANCED FIG 1 - regression with CI intervals; chl ~ alk
op=par(mfrow=c(1,1))

##### Step 1:  create linear model (chl ~ alk)
alkchl.lm = lm(log10(chl) ~ alk)

##### Step 2:  sort the x axis (unique values only)
alk.sort = sort(unique(alk))

##### Step 3:  use predict to predict chl ~ tp from linear model
pred.chl = predict(alkchl.lm, 
    newdata = data.frame(alk = alk.sort), int="pred")

##### Step 4:  plot original data and linear model
plot(log10(chl) ~ alk,
     xlab="Alkalinity (mg/L)",
     ylab=expression(paste("Log10 Chlorophyll " (mu * "g/L"))),
     pch=21, bg="skyblue", cex=1.5)
abline(alkchl.lm, lwd=2, lty=2, col="red")


##### Step 5:  add upper and lower CI
lines(alk.sort, pred.chl[,2], lty=2) #lower CI
lines(alk.sort, pred.chl[,3], lty=2) #upper CI

##### Step 6:  add a legend with the linear model statistics
legend(x="topleft", 
   c(paste("Log10 Chl =", round(alkchl.lm$coef[2],4), 
     "* Alk + ", round(alkchl.lm$coeff[1],4)),
    paste("Adj.R-squared =", round(summary(alkchl.lm)$adj.r.squared, 4)),
    paste("P-value =", round(summary(alkchl.lm)$coef[8], 4))),
    bty="n", cex=1) 
dev.print(postscript, "advanced1.ps", horizontal=F, paper="letter")


##### Adv figure #2:  multiple transformations
par(mfcol=c(3,3))
plot(density(alk), xlab="", main="Alkalinity")
plot(density(ph), xlab="", main="pH")
plot(density(turb), xlab="", main="Turbidity")

plot(density(log(alk)), xlab="", main="Log Alkalinity")
plot(density(log(ph)), xlab="", main="Log pH")
plot(density(log(turb)), xlab="", main="Log Turbidity")

plot(density(sqrt(alk)), xlab="", main="Sqrt Alkalinity")
plot(density(sqrt(ph)), xlab="", main="Sqrt pH")
plot(density(sqrt(turb)), xlab="", main="Sqrt Turbidity")
dev.print(postscript, "advanced2.ps", horizontal=F, paper="letter")


##### Adv figure #3:  QQ plots on transformed data
par(mfcol=c(1,1))
qqnorm(alk, pch=21, bg="red", cex=1.7,
       main="QQ Plot of Alkalinity")
text(x=2.5, y=74, "lower than expected", cex=0.7)
text(x=-2.4, y=8, "higher than expected", cex=0.7)
qqline(alk, lty=2, lwd=3, col="red")
dev.print(postscript, "advanced3.ps", horizontal=F, paper="letter")

par(mfcol=c(3,3))
qqnorm(alk, xlab="", main="Alk", pch=21, bg="red")
   qqline(alk, col="red", lty=2)
qqnorm(ph, xlab="", main="pH", pch=21, bg="red")
   qqline(ph, col="red", lty=2)
qqnorm(turb, xlab="", main="Turb", pch=21, bg="red")
   qqline(turb, col="red", lty=2)

qqnorm(log(alk), xlab="", main="Log Alk", pch=21, bg="red")
   qqline(log(alk), col="red", lty=2)
qqnorm(log(ph), xlab="", main="Log pH", pch=21, bg="red")
   qqline(log(ph), col="red", lty=2)
qqnorm(log(turb), xlab="", main="Log Turb", pch=21, bg="red")
   qqline(log(turb), col="red", lty=2)

qqnorm(sqrt(alk), xlab="", main="Sqrt Alk", pch=21, bg="red")
   qqline(sqrt(alk), col="red", lty=2)
qqnorm(sqrt(ph), xlab="", main="Sqrt pH", pch=21, bg="red")
   qqline(sqrt(ph), col="red", lty=2)
qqnorm(sqrt(turb), xlab="", main="Sqrt Turb", pch=21, bg="red")
   qqline(sqrt(turb), col="red", lty=2)

dev.print(postscript, "advanced4.ps", horizontal=F, paper="letter")

