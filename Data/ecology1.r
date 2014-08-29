#######################################################
# Source Files for R Minicourse Part 1, Sept 2-3, 2014
# R. Matthews, Huxley College of the Environment               
# G. Matthews, Computer Science Department                     
# Western Washington University
#######################################################



################################
# Writing Simple Functions
################################
lakes <- read.csv("lakes.csv", header=TRUE)
attach(lakes); summary(lakes)

SE <- function(x) sqrt(var(x)/length(x))

ci95 <- function(x)   {
t.value <- qt(0.975, length(x)-1)
standard.error <- SE(x)
ci <- t.value * standard.error
cat("95 CI = ", round(mean(x) -ci, 1), "to ", round(mean(x) +ci, 1), "\n")  }


################################
# Saving Output to a Data Matrix
################################
lakes <- read.table("lakes.csv", T, sep=",")
attach(lakes)

sitemean <- round(tapply(cond, site, mean),1)
sitemed  <- round(tapply(cond, site, median),1)
sitemin  <- round(tapply(cond, site, min),1)
sitemax  <- round(tapply(cond, site, max),1)
siteN    <- tapply(cond, site, length)
siteID <- unique(site)

summary.stats <- cbind(sitemin, sitemean, sitemed, sitemax, siteN)

cond.stats <- data.frame(siteID, summary.stats)
names(cond.stats) <- c("Site", "Min", "Mean", "Median", "Max", "Count")

write.table(cond.stats, "conductivity.csv",
      quote=F, row.names=F, col.names=T, sep=",")


################################
# Advanced Topics - R and Dates
################################
lake.dates <- ISOdate(lakes$year, lakes$month, lakes$day)
lake.dates[1] + 1
lake.dates[1] + 24*60*60
strptime("2012/5/12", format="%Y/%m/%d")
strptime("12/5/12", format="%y/%m/%d")
strptime("2012/5/12 13:30:02" , format="%Y/%m/%d %H:%M:%S")
datestrings <- c("2012/5/1", "2015/2/22", "2011/12/25")
strptime(datestrings, format="%Y/%m/%d")
