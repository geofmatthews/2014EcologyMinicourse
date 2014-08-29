# whiteriver02.r
# compare observed and predicted for a all Reach/Distance pairs and all parameters

# clear objects for debugging:
rm(list=ls()[!grepl('^observed$',ls()) & !grepl('^predicted$',ls())])

# Erase the text file:
textfile.name <- "whiterivertext.txt"
capture.output(cat(""), file=textfile.name, append=F)

# Start the plot file:
plotfile.name <- "whiteriverplot.pdf"
pdf(file=plotfile.name, 
    paper="USr", # rotated letter size
    width=9,
    height=7,
    onefile=T, 
    title="White River Case Study")

# load the data
# loading the data takes a few seconds, and only needs to be done once, 
# so we only load it if it has not already been defined.  If you want
# to force a reload, remove existing variables with "rm(list = ls())"
if (!exists("predicted") || !exists("observed")) {
  predicted <- read.table("Q2KW_White_River_predicted_data.csv",
                          header=T,
                          sep=",",
                          stringsAsFactors=F) 
  observed <- read.table("Q2KW_White_River_observed_data.csv",
                         header=T,
                         sep=",",
                         stringsAsFactors=F)
  # We want to load "5/5/12" as a string, rather than a factor,
  # to make subsequent conversion to dates a bit easier.
  # Now replace date strings with actual dates:
  predicted$Date.time <- strptime(predicted$Date.time, format="%m/%d/%y %H:%M")
  observed$Date.time <- strptime(observed$Date.time, format="%m/%d/%y %H:%M")
  
}
# can examine data with dim(pred) or summary(pred) etc., but the large number
# of columns and rows makes this difficult to do visually

# We want to compare predicted and observed at similar sites, determined by reach and distance
# Observed reach is a subset of predicted reach:
observed.reaches <- unique(sort(observed$Reach.number))
capture.output(cat("Observed reaches:\n"),
               file=textfile.name, append=T)
capture.output(print(observed.reaches),
               file=textfile.name, append=T)

predicted.reaches <- unique(sort(predicted$Reach))
capture.output(cat("Predicted reaches:\n"),
               file=textfile.name, append=T)
capture.output(print(predicted.reaches),
               file=textfile.name, append=T)


# so we can simply select matching value for predicted to match a given observed reach
# Note that this column is inconsistently labelled: 
#     Reach.number in observed and Reach in predicted

# Observed distance is a different set of numbers entirely:
observed.distances <- unique(sort(observed$Distance))
predicted.distances <- unique(sort(predicted$Distance))
capture.output(cat("Observed distances:\n"),
               file=textfile.name,append=T)
capture.output(print(observed.distances),
               file=textfile.name,append=T)
capture.output(cat("Predicted distances:\n"),
               file=textfile.name,append=T)
capture.output(print(predicted.distances),
               file=textfile.name,append=T)

# so we need to select the closest predicted value to match a given observed distance

# Loop through all parameters.  Must make sure names match up from the two files,
# they are not consistently named and they do not have the same column numbers.
observed.parameter.names <- c("Temperature..degC.",
                              "Conductivity..uS.cm.25C.",
                              "Dissolved.oxygen..mgO2.L.",
                              "Inorganic.P..ugP.L.", 
                              "pH",
                              "Flow")
predicted.parameter.names <- c("Temperature..degC.",
                               "Conductivity..uS.cm.25C.",
                               "Dissolved.oxygen..mgO2.L.",
                               "Inorganic.P..ugP.L.", 
                               "pH",
                               "Flow..m.3.sec.")
for (which.name in seq(along=observed.parameter.names)) { 
  observed.parameter.name <- observed.parameter.names[which.name]
  predicted.parameter.name <- predicted.parameter.names[which.name]
  observed.parameter.column <- which(names(observed) == observed.parameter.name)
  predicted.parameter.column <- which(names(predicted) == predicted.parameter.name)
  
  # Loop through all reaches:
  for (which.reach in seq(along=observed.reaches)) {  
    my.reach <- observed.reaches[which.reach]
    
    # Loop through all distances:
    for (which.distance in seq(along=observed.distances)) {    
      my.observed.distance <- observed.distances[which.distance]
      
      my.id <- paste(observed.parameter.name,
                     "reach:", my.reach,
                     "distance:", my.observed.distance)
      capture.output(cat(paste("Sample ID:", my.id, "\n")),
                     file=textfile.name, append=T)
      
      # Find the closest predicted distance in the data:
      min.difference.index <- which.min(abs(predicted.distances - my.observed.distance))
      my.predicted.distance <- predicted.distances[min.difference.index]
      
      # select only the matching rows with data:
      observed.rows <- (observed$Reach.number == my.reach) & 
        (observed$Distance == my.observed.distance) &
        (!is.na(observed$Date.time)) &
        (!is.na(observed[,observed.parameter.column]))
      
      capture.output(cat(paste("N observed:", length(which(observed.rows)), "\n")),
                     file=textfile.name,append=T)
      
      predicted.rows <- (predicted$Reach == my.reach) & 
        (predicted$Distance == my.predicted.distance) &
        (!is.na(predicted$Date.time))&
        (!is.na(predicted[,predicted.parameter.column]))
      
      if ((length(which(predicted.rows)) < 3) | (length(which(observed.rows)) < 3) ) {
        capture.output(cat("Skipping because of insufficient data.\n" ),
                       file=textfile.name,append=T)
        next
      }
      
      capture.output(cat(paste("N predicted:", length(which(predicted.rows)), "\n")),
                     file=textfile.name, append=T)
      
      # We want to plot this against date, so we also get that column.
      # Again note the requirement for consistent column names here.
      date.name <- "Date.time"
      observed.date.column <- which(names(observed) == date.name)
      predicted.date.column <- which(names(predicted) == date.name)
      
      observed.columns <- c(observed.date.column, observed.parameter.column)
      predicted.columns <- c(predicted.date.column, predicted.parameter.column)
      
      #select the appropriate column:
      observed.subset <- observed[observed.rows, observed.columns]
      predicted.subset <- predicted[predicted.rows, predicted.columns]
      
      x.range <- as.numeric(range(c(observed.subset$Date.time,
                                    predicted.subset$Date.time)))
      y.range <- range(c(observed.subset[[observed.parameter.name]],
                         predicted.subset[[predicted.parameter.name]]))
      # Plot the data.frames
      plot(observed.subset,
           type="l",
           xlim = x.range,
           ylim = y.range,
           main=paste("Observed", my.id,
                      "N=",length(which(observed.rows))))
      plot(predicted.subset,
           type="l",
           xlim = x.range,
           ylim = y.range,
           main=paste("Predicted", my.id, 
                      "N=",length(which(predicted.rows))))
      
      # Plot both on same page
      #  Figure y-placement for legend
      y.median <- mean(y.range)
      if (predicted.subset[1,2] < y.median) {
        ypos <- max(y.range)
        yjus <- 1
      } else {
        ypos <- min(y.range)
        yjus <- 0
      }
      plot(observed.subset,
           type="l",
           xlim = x.range,
           ylim = y.range,
           col="blue",
           main=my.id)
      lines(predicted.subset,
            type="l",
            col="red")
      legend(x=min(x.range), y=ypos, yjust=yjus, 
             lty=1,
             col=c("blue","red"),
             legend=c("Observed","Predicted")
      )
      
      # Now we want to do some analysis of the fit between the observed
      # and the predicted, so we have to match up points.  We will take the 
      # observed subset, and compute a new column with the closest match in 
      # time from the predicted subset.
      
      # Since there may be tens of thousands of observed rows, we'll take 
      # a random subset to save time.
      
      n <- min(100, nrow(observed.subset))
      sample.rows <- sort(sample(1:nrow(observed.subset), n))
      observed.sample <- observed.subset[sample.rows,]
      predictions <- rep(0,n)
      for (row in 1:n) {
        obs.time <- observed.sample$Date.time[row]
        pred.row <- which.min(abs(predicted.subset$Date.time-obs.time))
        predictions[row] <- predicted.subset[[predicted.parameter.name]][pred.row] 
      }
      
      # Plot just the subsample
      
      x.range <- as.numeric(range(observed.sample$Date.time))
      y.range <- range(c(observed.sample[[observed.parameter.name]],
                         predictions))
      
      # Plot both on same page
      #  Figure y-placement for legend
      y.median <- mean(y.range)
      if (observed.sample[1,2] < y.median) {
        ypos <- max(y.range)
        yjus <- 1
      } else {
        ypos <- min(y.range)
        yjus <- 0
      }
      plot(observed.sample,
           type="l",
           xlim=x.range,
           ylim=y.range,
           col="blue",
           main=paste("Subsampled",my.id))
      lines(observed.sample$Date.time, predictions,
            type="l",
            col="red")
      legend(x=min(x.range), y=ypos, yjust=yjus,
             lty=1,
             col=c("blue","red"),
             legend=c("Observed","Predicted")
      )
      
      # Build a linear model
      my.model <- lm(predictions ~ observed.sample[[observed.parameter.name]])
      capture.output(summary(my.model),
                     file=textfile.name, append=T)
      capture.output(anova(my.model),
                     file=textfile.name, append=T)
      
      # Plot observed vs. predicted
      plot(observed.sample[[observed.parameter.name]], predictions,
           xlab="Observed",
           ylab="Predicted",
           main=paste("Linear model for", my.id))
      # Add the best fit line
      abline(my.model,col="red")
      legend(x=min(observed.sample[[observed.parameter.name]]),
             y=max(predictions),
             legend=c(paste("Slope:", round(my.model$coefficients[2],2)),
                      paste("Intercept:", round(my.model$coefficients[1],2))))
      
      # Plot it (4 on a page)
      par(mfrow=c(2,2))
      plot(my.model, main=my.id)
      par(mfrow=c(1,1))
    }
  }
}
# Close pdf file
dev.off()
