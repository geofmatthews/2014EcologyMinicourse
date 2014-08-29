#source("whiteriverloaddata.r")

pred.dist <- sort(unique(pred$Distance..Km.))
pred.midpoints <- pred.dist[2:length(pred.dist)] - 0.5*diff(pred.dist)
obs.dist <- sort(unique(obs$Distance..Km.))
obs.midpoints <- obs.dist[2:length(obs.dist)] - 0.5*diff(obs.dist)

boxplot(pred.dist, obs.dist,
        ylabel="Distance")
obsn <- length(obs.dist)
predn <- length(pred.dist)
points(c(rep(1,predn), rep(2, obsn)),
       c(pred.dist, obs.dist))

obs.dist.i <- obs$Distance..Km. < obs.midpoints[1]
pred.dist.i <- pred$Distance..Km. < pred.midpoints[1]

reach <- 8
obs.reach.i <- obs$Reach.number == reach
pred.reach.i <- pred$Reach == reach

i <-  obs.dist.i

plot(obs.date.time[i], obs$Temperature..degC.[i])

i <- pred.dist.i
plot(pred.date.time[i], pred$Temperature..degC.[i])