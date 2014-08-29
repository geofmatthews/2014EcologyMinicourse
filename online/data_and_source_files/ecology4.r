#######################################################
# Source Files for R Minicourse Part 4, Sept 2-3, 2014
# R. Matthews, Huxley College of the Environment               
# G. Matthews, Computer Science Department                     
# Western Washington University
#######################################################


#######################################################
# Principal Components Analysis - Iris Data (prcomp)
#######################################################
data(iris); attach(iris)
iris.prcomp <- prcomp(iris[, c(1:4)], scale=T, center=T)

iris.prcomp$rotation  ### for princomp: iris.princomp$loading
iris.prcomp$x          ### for princomp: iris.princomp$scores


### plotting example:
op=par(mfrow=c(1,1))
plot(iris.prcomp$x,
  main="Principal Components Ordination of Iris Samples",
  pch=c(21,22,24)[unclass(iris$Species)],
  bg=c("pink", "violet", "purple")[unclass(iris$Species)],
  cex=1.5)
abline(h=0); abline(v=0)
legend(x="topright", c("I. setosa", "I. versicolor", "I. virginica"),
       pch=c(21, 22, 24), pt.bg=c("pink", "violet", "purple"),
       bty="n", cex=1)


#######################################################
# Hierarchical Clustering - Iris Data (10 rows/species)
#######################################################
### First example - Euclidean distance with farthest neighbor:
data(iris); attach(iris)

### Step 1:  select data subset and distance metric
edist <- dist(iris[c(1:10, 51:60, 101:110), c(1:4)],
        method="euclidean")

### Step 2: select clustering method
edist.complete <- hclust(edist, method="complete")

### Step 3: plot the results as an edited dendrogram
plot(edist.complete, labels=iris[c(1:10, 51:60, 101:110), 5],
     ylab="Euclidean Distance", xlab=" ", main=" ", sub=" ")

### Second example - Euclidean distance with Ward's minimum variance

edist.ward <- hclust(edist, method="ward")

### Add hang=-1 to place samples on x-axis
plot(edist.ward, labels=iris[c(1:10, 51:60, 101:110), 5],
     ylab="Euclidean Distance", xlab=" ", main=" ", sub=" ", hang=-1)


#######################################################
# Devisive Clustering - Iris Data (10 rows/species)
#######################################################
irispart <- iris[c(1:10, 51:60, 101:110),] # R shortcut!
kcluster3 <- kmeans(irispart[ , c(1:4)], 3)
kcluster3  #This produces the cluster summary

plot(irispart[, c(1:4)], col=kcluster3$cluster, pch=unclass(irispart[,5]))



#######################################################
# Devisive Clustering/Association Analysis - All Iris Data
#######################################################
data(iris); attach(iris)
kcluster3.all <- kmeans(iris[, c(1:4)], 3)
table(Species, kcluster3.all$cluster)

plot(Petal.Length, Petal.Width, 
     pch=c(21, 22, 24)[unclass(Species)],
     cex=1.7, xlab="Petal Length (cm)", ylab="Petal Width (cm)",
     main="Kmeans Clustering of Iris Data Into Three Groups",
     bg=c("pink", "violet", "purple")[kcluster3.all$cluster])
legend(x="topleft", c("I. setosa", "I. versicolor", "I. virginica"), 
       pch=c(21, 22, 24), bty="n")
legend(x="top", c("Group 1", "Group 2", "Group 3"), 
       fill=c("pink", "violet", "purple"), bty="n")
legend(x="bottomright", c("misclassification = 10.7 pct",
       "(2 versicolor + 14 virginica)"), bty="n")



#######################################################
# Advanced Clustering on Principal Components
# Microcosm Test Using Contaminated Sediments
#######################################################
#### create the PCA using OTUs (col 1-2 = treatment/replicate)
alldata <- read.csv("alldataOTU.csv", T); attach(alldata)
alldataPCA <- prcomp(alldata[, c(3:851)], scale=T)
summary(alldataPCA)

#### write the scores to a new data set:
PCA.scores <- data.frame(alldata$treatment, alldata$replicate, 
    round(alldataPCA$x, 3))

write.table(PCA.scores, "alldataPCA.csv", quote=F, row.names=F, 
    col.names=T, sep=",")

### plot PCA variance:
plot(alldataPCA, col=rainbow(10), main=" ")

### cluster on component scores
newdata <- read.csv("alldataPCA.csv", T); attach(newdata)
distances <- dist(newdata[, c(3:20)], method="euclidean")
eward <- hclust(distances, method="ward")
plot(eward, labels=treatment, hang=0, cex=0.65, xlab=" ", sub=" ",
      main="PC1-18 (100%)", ylab="Euclidean Distance")

