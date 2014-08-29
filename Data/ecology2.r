#######################################################
# Source Files for R Minicourse Part 2, Sept 2-3, 2014
# R. Matthews, Huxley College of the Environment               
# G. Matthews, Computer Science Department                     
# Western Washington University
#######################################################


################################
# Using the chron Library
################################
### EXAMPLE 1:
lakes <- read.table("lakes.csv", T, sep=",")
attach(lakes)
library(chron) ### this will load chron during your work session

mdy.chron <- function(month, day, year) {
  chron(dates.=paste(month, day, year, sep="/"))
}

plot(tp ~ mdy.chron(month, day, year))


### EXAMPLE 2:
### create a "nice" date range for the x-axis
xdates <- c(mdy.chron(1,1,2007), mdy.chron(12, 31, 2013))

### plot the data, with x/y axis labels and annotations
plot(tp ~ mdy.chron(month, day, year),
     xlim=xdates,
     xlab=" ",
     ylim=c(0, 1000),
     ylab="TP (ug-P/L)",
     pch=21, bg="skyblue", cex=1.5)

### add a text line to identify the outlier
text(x=mdy.chron(6,28,2010), y=850, "Wiser Lake, June 28, 2010",
     cex=0.75, col="red")



################################
# Advanced Scatterplot Features
################################
### Step #1: create an empty plot (type="n"); 
###          this will let us place the polygon layer beneath the data
plot(log10(chl), log10(tp), type="n",
     xlab=expression(paste("Chl"[log10] ~ (mu * "g/L"))),
     ylab=expression(paste("TP"[log10] ~ (mu * "g-P/L"))))

### Step #2: draw a shaded, borderless rectangle showing tp detection limit
rect(xleft=-1.1, ybottom=-0.5, xright=2.9, ytop=log10(2), col="pink", border=NA)

### Step #3: add the chlorophyll and total phosphorus data using points
points(log10(chl), log10(tp),
     pch=21, bg="skyblue", cex=1.5)

### Step #4: add a horizontal line at the tp "action" level (20 ug/L)
abline(h=log10(20), lty=2, lwd=2, col="red")

### Step #5: use text and legend to annotate the figure;
###          paste and expression are used to add math symbols and subscripts
text(x=2.5, y=0.25, "TP detection limit", cex=0.7, col="red")
legend(x="topleft", expression("20" * ~ mu * "g-P/L"),
       lty=2, lwd=2, col="red", bty="n")



################################
# Annotated Boxplots - Iris Data
################################
data(iris); attach(iris)
boxplot(Sepal.Width ~ Species, 
   ylab="Sepal Width (cm)", main="Boxplot of Iris Sepal Width",
   notch = T, boxwex = 0.5,
   col = c("mistyrose", "skyblue", "orchid"),
   border=c("hotpink4", "skyblue4", "mediumorchid4"),
   names = c("Iris setosa", "Iris versicolor", "Iris virginica"))


################################
# Paired Boxplots - Guinea Pig Data
################################
data(ToothGrowth); attach(ToothGrowth)
boxplot(len ~ dose,
     boxwex = 0.25, at = 1:3 - 0.2,
     subset = supp == "VC", col = "lightyellow",
     xlab = "Vitamin C dose (mg)",
     ylab = "Tooth Length (mm?)", ylim = c(0, 35), yaxs = "i")

boxplot(len ~ dose, add = TRUE,
     boxwex = 0.25, at = 1:3 + 0.2,
     subset = supp == "OJ", col = "darkorange")

legend(x="bottomright", c("Ascorbic acid", "Orange juice"),
     fill = c("lightyellow", "darkorange"))


################################
# Advanced Boxplot Features
################################
with(ToothGrowth,
     {
      boxplot(len~supp, border=c("darkorange3", "gold4"),
              col=c("darkorange", "gold"), boxwex=0.5,
              ylab="Tooth Growth",
              names=c("Orange Juice", "Vitamin C"))

      points(jitter(rep(1:2, each=30), 0.5),
             unlist(split(len, supp)),
             cex=1.25, pch=16, 
             col=c("gold4", "darkorange3")[unclass(ToothGrowth$supp)])
     })

