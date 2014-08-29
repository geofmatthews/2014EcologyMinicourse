##### This file produces qqnorm and qqline results for the iris species

data(iris)
attach(iris)

op = par(mfrow=c(2,2))

qqnorm(Sepal.Length[Species=="setosa"], main="setosa");qqline(Sepal.Length[Species=="setosa"])
qqnorm(Sepal.Length[Species=="versicolor"], main="versicolor");qqline(Sepal.Length[Species=="versicolor"])
qqnorm(Sepal.Length[Species=="virginica"], main="virginica");qqline(Sepal.Length[Species=="virginica"])

par(op)

dev.print (postscript, "qqplots.ps", horizontal=F, paper="letter")


