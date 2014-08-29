### see lecture notes - used "list" rather than data.frame for predict

##### Data; Y = x+50 + noise; Y2 = ln(x) plus noise
X = c(1:10)
Y = c(49.8,51.6,53.7,53.9,55.1,56.5,57.4,57.9,58.9,60.8)

##### Linear models and predicted values
Yfit = lm(Y~X)
Y.predict = predict(Yfit, list(X=6.7))

op=par(mfrow=c(1,1))
plot(Y ~ X, pch=21, bg="red", cex=2, ylab="Y")
abline(Yfit, lwd=2, lty=2, col="red")
legend(x="topleft", c(paste("R-squared =", round(summary(Yfit)$r.squared, 4)),
         paste("Adj.R-squared =", round(summary(Yfit)$adj.r.squared, 4)),
         paste("p-value =", round(summary(Yfit)$coef[8], 4))),
         bty="n", cex=1.25)
legend(x="bottomright", 
       c("# Simple syntax:   ", "lm(Y ~ X)", "plot(Y ~ X)", "abline(Yfit)"),
       text.col="red", bty="n")
text(2.7, 59, "(P-value <0.00001 =>rounds to 0)", col="red", bty="n", cex=0.7)
dev.print(postscript, "Y.ps", horizontal=F, paper="letter")

