pred <- read.table("Q2KW_White_River_predicted_data.csv",
                   header=T,
                   sep=",",
                   stringsAsFactors=F)
obs <- read.table("Q2KW_White_River_observed_data.csv",
                   header=T,
                   sep=",",
                   stringsAsFactors=F)

pred.date.time <- strptime(pred$Date.time, format="%d/%m/%y %H:%M")
obs.date.time <- strptime(obs$Date.time, format="%d/%m/%y %H:%M")
