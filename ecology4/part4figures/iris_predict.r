#########################################
# THIS USES PREDICT TO ORDINATE NEW DATA
#
# UPDATED IN 2008 - PREVIOUS LECTURE NOTES USED
# PREDICT INCORRECTLY (USED INSTEAD OF SCORES)
##########################################

iris = read.table("iris_train.dat", T)
irispca = princomp(iris[, c(1:4)], cor = T)

new.iris = read.table("iris_newdata.dat", T)
attach(new.iris)

irispredict = predict(irispca, newdata=new.iris)
irispredict


plot(irispredict, type="n", 
   xlab="PC I", xlim=c(-3, 3),
   ylab="PC II", ylim=c(-3,3),
   main="New Iris Sample Ordination")
abline(h=0); abline(v=0)
text(irispredict, levels(Species)[Species])
dev.print(postscript, "irispredict.ps", horizontal=F, paper="letter")

