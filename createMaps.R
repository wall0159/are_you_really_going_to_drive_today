
#import required libraries
library(ggmap)
require(osrm)
library(geosphere)
library(leaflet)
library(tidyr)
library(tidyverse)
library(lubridate)

# load data from open databases:
# https://data.gov.au/dataset/ds-qld-5263cbd5-e569-4dc6-84b1-ad5e55fc9d1f/details?q=travel%20time
# http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=136&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=040913
# tmr.qld.gov.au/~/media/aboutus/corpinfo/Open%20data/bluetoothtraveltimes/Priority-Route-Bluetooth-field-descriptions.csv
# tmr.qld.gov.au/~/media/aboutus/corpinfo/Open%20data/bluetoothtraveltimes/Priority-Route-Link-Details-2019.csv

# these data contain the route times, using a pair of 4 digit codes to specify where the routes go
fRouteTravelTimes = 'Priority-Route-Bluetooth-Travel-Times-Mar-2019.csv';
routeTravelTimes = read.table(fRouteTravelTimes, header = TRUE, sep=",", stringsAsFactors = FALSE);

# these data define the routes from the 4 digit codes, and also provide latitude/longitude value pairs for each end of the route
fRouteDefs <- 'Priority-Route-Link-Details-2019.csv'; 
routeDefs<-read.table(fRouteDefs, header = TRUE, sep=",", stringsAsFactors = FALSE)
names(routeDefs)[3] <- 'lng'
names(routeDefs)[4] <- 'lat'

# precipitation data for Brisbane, March 2019
fBrisRain = 'mar19_BrisbaneRain.csv'
brisRain <- read.table(fBrisRain, header = TRUE, sep=",", stringsAsFactors = FALSE)
brisRain$date <- as.POSIXct(paste(brisRain$Year, 
 str_pad(brisRain$Month,width = 2, pad = "0", side = "left"),
 str_pad(brisRain$Day, width = 2, pad = "0", side = "left"),sep="-"),format="%Y-%m-%d")
names(brisRain)[6] <- 'rainfall'
brisRain <- brisRain[,c(9,6)]

# assemble the data, joining the various databases together
routeTravelTimes$date <- as.POSIXct(routeTravelTimes$INTERVAL_END,format="%d/%m/%Y %H:%M")

rainRoutes <- routeTravelTimes %>%
  gather("key", "value", -INTERVAL_END) %>%
  dplyr::mutate(key = str_replace(key, "X(.{4})\\.{2}(.{4})", "\\1 \\2"))
rainRoutes <- rainRoutes[rainRoutes$value < 1000000,]
names(rainRoutes)[1] <- "datetime"
rainRoutes$datetime <- as.POSIXct(rainRoutes$datetime,format="%d/%m/%Y %H:%M")
rainRoutes$date <- as.POSIXct(round(rainRoutes$datetime,'days'))
rainRoutes <- full_join(rainRoutes, brisRain, by="date")

rainRoutes$propOfDay <- hour(rainRoutes$datetime) + minute(rainRoutes$datetime)/60
# rainRoutes$timeOfDay <- strftime(rainRoutes$datetime, format="%H:%M:%S")

# create a model linking rainfall to the morning commute duration
morningRush = rainRoutes[abs(rainRoutes$propOfDay - 8.5)<0.55,]
morningRush = morningRush[morningRush$value > 600,]
mod <- lm(value ~ rainfall -1, morningRush)
summary(mod)
library(ggplot2)
ggplot(morningRush,aes(x=value))+geom_histogram()+facet_grid(rainfall ~ .)+theme_bw()

plot(rainRoutes$rainfall, rainRoutes$value)

# create a model linking rainfall to the afternoon commute duration
arvoRush = rainRoutes[abs(rainRoutes$propOfDay - 17.5)<0.55,]
mod <- lm(value ~ rainfall -1, arvoRush)
summary(mod)
ggplot(morningRush,aes(x=value))+geom_histogram()+facet_grid(rainfall ~ .)+theme_bw()
plot(arvoRush$rainfall, arvoRush$value)

# create a map showing the various routes and their sensitivity to trip time increases. 
# The figure shows how the maximum trip duration relates to the average trip duration
mod <- lm(value ~ rainfall -1, rainRoutes)
summary(mod)
plot(rainRoutes$rainfall)
plot(rainRoutes$value)
plot(rainRoutes$rainfall, rainRoutes$value)

routes <- routeTravelTimes %>%
  gather("key", "value", -INTERVAL_END) %>%
  dplyr::mutate(key = str_replace(key, "X(.{4})\\.{2}(.{4})", "\\1 \\2")) %>%
  dplyr::group_by(key) %>%
  dplyr::summarise(valueMed = median(value, na.rm =T), valueMax = max(value, na.rm = T),
                   ratio = (valueMax - valueMed)/valueMax)

coords <- routeDefs %>%
  dplyr::mutate(key = str_replace(LINK_DETAILS, "(.{4}).{2}(.{4})", "\\1 \\2"))

routes <- full_join(routes, coords, by="key")

# for (name in names(routeTravelTimes))
#   p1 <- substr(name, 2, 5)
#   p2 <- substr(name, 8, 11)
#   names(routeTravelTimes[name]) <- paste0(p1, '->', p2)
# end

routesList <- function(stLon,stLat,enLon,enLat){
  list(lng <- list(stLon, enLon), lat <- list(stLat, enLat)))
}




#m <- addMarkers(m, lng=routeDefs$lng, lat=routeDefs$lat, popup="The birthplace of R")
#m <-apply(routes, 1, addLine)

ratio_to_hex <- function(ratio){
  str_pad(as.hexmode(floor(256 * ratio)), width = 2, pad = "0", side = "left")
}

color_map <- function(ratio){
  red <- ratio_to_hex(ratio)
  blue <- ratio_to_hex(1-ratio)
  green <- "00"  
  # Hex code for the colour
  hex_code <- paste0("#", red, green, blue)
}

# plot the map
m <- leaflet()
m <- addTiles(m)
for (i in 1:nrow(routes)){
  row = routes[i,]
  #thisRoute <- {list(row$lng, row$DEST_LONGITUDE), list(row$lat, row$DEST_LATITUDE)}
  m <- addPolylines(map = m, lng = c(row$lng, row$DEST_LONGITUDE), lat = c(row$lat, row$DEST_LATITUDE), 
                    color=color_map(row$ratio^3))
}
m
