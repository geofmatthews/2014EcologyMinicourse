##### This file produces examples of Type I and Type II errors

##### Normal distribution:plot(x, dnorm(x, 0, 1), type="l", ylim=c(0,1))
x <- seq(-3, 3, 0.1)


op=par(mfrow=c(1,1))
plot(x, dnorm(x, 0, 1), type="l")
dev.print (postscript, "normal.ps", horizontal=F, paper="letter")
par(op)

op = par(mfrow=c(2,2))
plot(x, dnorm(x, 0, 0.5), type="l", main="mean=0, sd=0.5", ylim=c(0,1))
plot(x, dnorm(x, 0, 2), type="l", main="mean=0, sd=2", ylim=c(0,1))
plot(x, dnorm(x, 2, 0.5), type="l", main="mean=2, sd=0.5", ylim=c(0,1))
    text(0, 0.9, "negative or left skewed distribution")
plot(x, dnorm(x, -2, 0.5), type="l", main="mean=-2, sd=0.5", ylim=c(0,1))
    text(0, 0.9, "positive or right skewed distribution")
par(op)
dev.print (postscript, "normal2.ps", horizontal=F, paper="letter")
