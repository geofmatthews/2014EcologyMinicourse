alldata = read.table("alldata_edited.csv", T, sep=",")
attach(alldata)

###################################################
# Create prcomp, omitting treatment and replicate
###################################################
alldataPCA = prcomp(alldata[, c(3:851)], scale=T)
summary(alldataPCA)

op=par(mfrow=c(1,2))
plot(alldataPCA$x[,c(1:2)], type="n",
  xlab="PC 1", xlim=c(-20, 20),
  ylab="PC 2", ylim=c(-40, 20),
  main="PCA Sample Loading")
rect(xleft=-2, ybottom=-36, xright=11, ytop=-7,
     col="pink",border="NA")
rect(xleft=9, ybottom=0, xright=16, ytop=9,
     col="skyblue",border="NA")
rect(xleft=-18, ybottom=-6, xright=3, ytop=9,
     col="plum",border="NA")
abline(h=0, col="red")
abline(v=0, col="red")
text(alldataPCA$x[,c(1:2)], levels(group)[group])


plot(alldataPCA$x[,c(2:3)], type="n",
  xlab="PC 2", xlim=c(-40, 20),
  ylab="PC 3", ylim=c(-20, 20),
  main="PCA Sample Loading")

rect(xleft=-36, ybottom=-17, xright=-7, ytop=19,
     col="pink",border="NA")
rect(xleft=-6, ybottom=-16, xright=8, ytop=10,
     col="plum",border="NA")
rect(xleft=0, ybottom=-10, xright=9, ytop=0,
     col="skyblue",border="NA")
rect(-12,13,-7,19, border="red", lty=2,lwd=2)
rect(-36,-16.5,-32,-14, border="red", lty=2,lwd=2)
abline(h=0, col="red")
abline(v=0, col="red")
text(alldataPCA$x[,c(2:3)], levels(group)[group])
dev.print(postscript, "PCAsampleplot.ps", horizontal=F, paper="letter")


