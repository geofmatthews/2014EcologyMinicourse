### DO NOT USE EXACT=F ... BAD EFFECTS ON DATA WITH NO TIES
lakes = read.csv("lakes.csv", T); attach(lakes)



source("correlation.r")

lakecor <- correlation.matrix(lakes[, c(5:16)], method="kendall")
write.table(lakecor$statistics, "lakeSTATS.csv", row.names=T, col.names=T, sep=",")
write.table(lakecor$p.values, "lakePVALUES.csv", row.names=T, col.names=T, sep=",")

