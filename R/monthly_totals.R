library(prettymapr)
library(dplyr)
library(ggplot2)
library(ggspatial)

mt <- read.csv('data/monthly_totals.csv', stringsAsFactors = FALSE)
mt$posix <- lubridate::dmy(mt$Date)
mt$posix[is.na(mt$posix)] <- lubridate::mdy(mt$Date[is.na(mt$posix)])
mtgeo <- mt %>% group_by(Geo) %>% do({
  n <- nrow(.)
  geo <- ggmap::geocode(paste(unique(.$Geo), "Canada"))
  data.frame(n=n, lat=geo$lat, lon=geo$lon)
})
mtgeo <- mtgeo[mtgeo$Geo != "",]
write.table(mtgeo, 'data/monthly_totals_postals.csv', sep=',', row.names=F)

ggplot(mtgeo, aes(lon, lat)) + geom_osm() + geom_spatial(mapping=aes(size=n)) +
  scale_size()
