alldata = read.table("alldata_edited.csv", T, sep=",")
attach(alldata)

###################################################
# Create prcomp, omitting treatment and replicate
###################################################
alldataPCA = prcomp(alldata[, c(3:851)], scale=T)
summary(alldataPCA)

op=par(mfrow=c(1,1))
plot(alldataPCA, col=rainbow(10), main=" ")
dev.print(postscript, "PCAvariableplot.ps", horizontal=F, paper="letter")

## Importance of components:
##                           PC1   PC2     PC3     PC4    PC5     PC6     PC7
## Standard deviation     10.221 9.880 8.77070 8.08297 7.7312 7.28034 7.03630
## Proportion of Variance  0.123 0.115 0.09061 0.07695 0.0704 0.06243 0.05832
## Cumulative Proportion   0.123 0.238 0.32863 0.40559 0.4760 0.53842 0.59673
##                            PC8     PC9    PC10   PC11    PC12    PC13    PC14
## Standard deviation     6.75020 6.71040 6.52041 6.3837 6.27286 5.86208 5.52219
## Proportion of Variance 0.05367 0.05304 0.05008 0.0480 0.04635 0.04048 0.03592
## Cumulative Proportion  0.65040 0.70344 0.75352 0.8015 0.84786 0.88834 0.92426
##                           PC15    PC16    PC17      PC18
## Standard deviation     5.05834 4.56602 4.22722 3.321e-15
## Proportion of Variance 0.03014 0.02456 0.02105 0.000e+00
## Cumulative Proportion  0.95440 0.97895 1.00000 1.000e+00

## don't rewrite unless necessary (edited variables names)
## PCA.scores = data.frame(alldata$treatment, alldata$replicate, 
##                         round(alldataPCA$x, 3))
## write.table(PCA.scores, "alldataPCA.csv", quote=F, row.names=F, col.names=T, sep=",")


###################################################
# Hcluster vs. treatment - finding stable clusters
###################################################
newdata = read.table("alldataPCA.csv", T, sep=",")
attach(newdata)


## plot showing all 18 scores labeled by treatment
distances = dist(newdata[, c(3:20)], method="euclidean")
eward = hclust(distances, method="ward")
plot(eward, labels=treatment, hang=0, cex=0.65, xlab=" ", sub=" ",
      main="PC1-18 (100%)", ylab="Euclidean Distance")
dev.print(postscript, "hclustPCAall.ps", horizontal=F, paper="letter")


## plot PC1-7 examples labeled by treatment
op=par(mfrow=c(2,3))
### PC1-7 (60%)
distances = dist(newdata[, c(3:9)], method="euclidean")
eward = hclust(distances, method="ward")
plot(eward, labels=treatment, hang=-1, cex=0.65, xlab=" ", sub=" ",
      main="PC1-7 (60%)", ylab="Euclidean Distance")
### PC1-6 (54%)
distances = dist(newdata[, c(3:8)], method="euclidean")
eward = hclust(distances, method="ward")
plot(eward, labels=treatment, hang=-1, cex=0.65, xlab=" ", sub=" ",
      main="PC1-6 (54%)", ylab="Euclidean Distance")
### PC1-5 (48%)
distances = dist(newdata[, c(3:7)], method="euclidean")
eward = hclust(distances, method="ward")
plot(eward, labels=treatment, hang=-1, cex=0.65, xlab=" ", sub=" ",
      main="PC1-5 (48%)", ylab="Euclidean Distance")
### PC1-4 (41%)
distances = dist(newdata[, c(3:6)], method="euclidean")
eward = hclust(distances, method="ward")
plot(eward, labels=treatment, hang=-1, cex=0.65, xlab=" ", sub=" ",
      main="PC1-4 (41%)", ylab="Euclidean Distance")
### PC1-3 (33%)
distances = dist(newdata[, c(3:5)], method="euclidean")
eward = hclust(distances, method="ward")
plot(eward, labels=treatment, hang=-1, cex=0.65, xlab=" ", sub=" ",
      main="PC1-3 (33%)", ylab="Euclidean Distance")
### PC1-2 (24%)
distances = dist(newdata[, c(3:4)], method="euclidean")
eward = hclust(distances, method="ward")
plot(eward, labels=treatment, hang=-1, cex=0.65, xlab=" ", sub=" ",
      main="PC1-2 (24%)", ylab="Euclidean Distance")
dev.print(postscript, "hclustPCAsubset.ps", horizontal=F, paper="letter")


###################################################
# Hcluster vs. treatment - BEST PLOT and AA test
###################################################
op=par(mfrow=c(1,1))
### PC1-3 (33%)
distances = dist(newdata[, c(3:5)], method="euclidean")
eward = hclust(distances, method="ward")
plot(eward, labels=treatment, hang=-1, cex=0.65, xlab=" ", sub=" ",
      main="PC1-3 (33%)", ylab="Euclidean Distance")
rect.hclust(eward, 2, border="red")
dev.print(postscript, "hclustPCAbest.ps", horizontal=F, paper="letter")


plot(eward, labels=treatment, hang=-1, cex=0.65, xlab=" ", sub=" ",
      main="PC1-3 (33%)", ylab="Euclidean Distance")
rect.hclust(eward, 3, border="red")
dev.print(postscript, "hclustPCAbest3.ps", horizontal=F, paper="letter")


###################################################
# cluster significance
# NOTEL  checked "stability" = all have 1 mismatch until PC1-2
###################################################
HCgroups = cutree(eward, 2)
table(HCgroups, treatment)
chisq.test(HCgroups, treatment)

HCgroups = cutree(eward, 3)
table(HCgroups, treatment)
chisq.test(HCgroups, treatment)
#### etc


