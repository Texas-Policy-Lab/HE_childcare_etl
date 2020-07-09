library(osrm)
data("berlin")
# Travel time matrix
distA <- osrmTable(loc = apotheke.sf[1:5,])
distA$durations
