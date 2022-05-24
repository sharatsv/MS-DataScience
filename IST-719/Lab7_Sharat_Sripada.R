# Week-7 Labs

# Choose the maplecturedata.csv
fname <- file.choose()
map <- read.csv(fname, header = T,stringsAsFactors = FALSE)

head(map)

plot(map$x, map$y)
polygon(map$x, map$y, col='firebrick1', border=NA)

library(maps)
library(mapproj)

map(database='world')
map('world', regions=c('India', 'Pakistan'), fill=T, col=c('orange', 'brown'))
map('world', regions='Finland')

# Get state data
m <- map('state')
m
plot(m$x, m$y)

map('state', fill=T, col=c('orange','yellow','red'))
map('county', region='New York', fill=T, col=terrain.colors(20))

library(rnaturalearth)
india <- ne_states(country='India')
india
attributes(india)
names(india)

india$name

map(india, namefield='name', 
    region=c('Gujarat', 'Rajastan', 'Madhya', 'Pradesh')
    , fill=T
    , col=c('orangered', 'white', 'springgreen4'))

library(raster)
india <- raster::getData('GADM', country='IND', level=1)
map(india)
india$NAME_1
map(india, namefield='NAME_1', region='Gujarat')

india <- raster::getData('GADM', country='IND', level=2)
map(india)
india$NAME_2
map(india, namefield='NAME_2', region='North 24 Parganas'
    , fill=T, col='springgreen4')


# Load the shootings.Rda (native R data)
my.dir <- '/Users/venkatasharatsripada/Downloads/'
fname <- paste0(my.dir, 'shootings.Rda')
load(fname)

head(shootings)

sort(shootings$State)
tmp.vec <- gsub("^\\s+|\\s+$", "", shootings$State)

shootings$State <- tmp.vec

agg.dat <- aggregate(shootings$Total.Number.of.Fatalities, list(shootings$State), sum)
colnames(agg.dat) <- c('state', 'victims')

# Let's color this data
num.cols <- 10
my.color.vec <- rev(heat.colors(num.cols))

# Map the color to the number of victims
library(plotrix)
agg.dat$index <- round(rescale(x=agg.dat$victims, c(1, num.cols)), 0)
agg.dat$color <- my.color.vec[agg.dat$index]

# Exercise with gsub
gsub("[[:upper:]]|\\s+|e", "", shootings$State)

# Load the map data
m <- map('state')
m$names

state.order <- match.map(database = 'state', regions=agg.dat$state
                         , exact = F, warn = T)

cbind(m$names, agg.dat$state[state.order])

map('state', col=agg.dat$color[state.order], fill=T, resolution = 0
    , lty=1, projection='polyconic', border='tan')

# Review the steps to create a choropleth map
# Pre-req: Figure out what you're mapping. In this case, we're mapping states, and we're mapping victims. 
# Step-1: 
# Map data to values of color - may require re-scaling data (see usage with plotrix) 

# Step-2:
# Map locations in the data to the map database locations using match.map() 

# Step-3:
# Print the map with the colors in the order that the map data needs them to be in. 

# Summary:
# Map your data to values of color, and then map your data, your locations, to the map 
# locations in the database, and then finally, make the map with the correct order.


# putting points on maps using lats/long
library(ggmap)

# Read the Newyorklibraries.csv
libs <- read.csv('/Users/venkatasharatsripada/Downloads/newyorklibraries.csv', header=T, quote="\"", stringsAsFactors = F)

# Playing with lats/longs
map('world')

# Plot lat,long - 0,0 on world map 
points(0,0,col='red',cex=3,pch=8)

# Plot Syracuse with (43, -76) co-ordinates  
# Lat
abline(h=43, col='blue', lty=3) 

# Long
abline(v=-76, col='blue', lty=3)

us.cities
map('state')
my.cols <- rep(rgb(1,.6,.2,.7), length(us.cities$name))
my.cols[us.cities$capital>0] <- rgb(.2,.6,1,.9)

points(us.cities$long, us.cities$lat, col=my.cols
       , pch=16
       , cex=rescale(us.cities$pop, c(0.5,7)))


geocode('3649 Erie Blvd East, Dewit, ny', source='google')

# New code since, dsk/google sources won't work
library(tmaptools)
geocode_OSM('3649 Erie Blvd East, Dewit, ny'
            ,return.first.only=T
            ,server = "http://nominatim.openstreetmap.org"
)

table(libs$CITY)
index <- which(libs$CITY %in% c('SYRACUSE', 'DEWITT', 'FAYETTEVILLE'))
addr <- paste(libs$ADDRESS[index], libs$CITY[index], libs$STABR[index], sep=', ')
map('county', 'new york', fill=T, col = 'orange')
g.codes <- geocode_OSM(addr, server = "http://nominatim.openstreetmap.org")
points(g.codes$lon, g.codes$lat, col='blue', cex=1.1, pch=16)


# RWorldsMaps

library(rworldmap)
library(plotrix)

countries <- read.csv('/Users/venkatasharatsripada/Downloads/countries.csv', sep=';', header=T, quote="\"", stringsAsFactors = F)

# Remove the countries with index 0.0
zap <- which(countries$Life.expectancy == 0.0)
countries <- countries[-zap, ]

num.cat <- 10

iso3.codes <- tapply(countries$Country..en. , 1:length(countries$Country..en.)
                     , rwmGetISO3)

df <- data.frame(country = iso3.codes, labels=countries$Country..en., life=countries$Life.expectancy)

df.map <- joinCountryData2Map(df, joinCode = 'ISO3', nameJoinColumn = 'country')
par(mar=c(0,0,1,0))
mapCountryData(df.map
               , nameColumnToPlot = 'life'
               , numCats = num.cat
               , catMethod = c('pretty','fixedWidth','diverging','quantiles')[2]
               , colourPalette = colorRampPalette(c('orangered', 'palegoldenrod', 'forestgreen'))(num.cat)
               , oceanCol = 'blue'
               , borderCol = 'peachpuff4'
               , mapTitle = 'Life Expectancy'
)


# ggmaps
library(ggmap)
library(raster)
reported <- read.csv('/Users/venkatasharatsripada/Downloads/IndiaReportedRapes.csv'
                     , header=T, quote="\"", stringsAsFactors = F)

india <- raster::getData('GADM', country='IND', level=1)
cbind(unique(reported$Area_name), india$NAME_1)

india$NAME_1[india$NAME_1 == 'NCT of Delhi'] <- 'Delhi'
india$NAME_1 <- gsub(" and ", " & ", india$NAME_1)

# install.packages("gpclib", type="source")

library(rgdal)
library(rgeos)

map <- fortify(india, region='NAME_1')
head(map)

crimes <- aggregate(reported$Cases, list(reported$Area_Name), sum)
colnames(crimes) <- c('id', 'ReportedRapes')
crimes[order(crimes$ReportedRapes), ]
my.map <- merge(x=map, y=crimes, by='id')

ggplot() + geom_map(data=my.map, map=my.map) + 
  aes(x=long, y=lat, map_id=id, group=group, fill=ReportedRapes) +
  theme_minimal() + ggtitle('Reported Rapes in India')


# Shapefiles - Shapes of buildings, streets on maps
# bikes.rds and nyct2010_17a

library(stringr)
library(rgdal)
library(raster)
library(TeachingDemos) # for zoomplot

shape.dat.dir <- '/Users/venkatasharatsripada/Downloads/shapefiles/'
bikes <- readRDS(paste0(shape.dat.dir, 'bikes.rds'))
nypp <- readOGR(paste0(shape.dat.dir, 'nyct2010_17a')
                , 'nyct2010', stringsAsFactors = F)

my.dir <- '/Users/venkatasharatsripada/Downloads/'
syr.neighborhood <- readOGR(paste0(my.dir, 'syracuse-neighborhoods_ny.geojson'))

par(mar=c(.5,.5,.5,.5))
plot(nypp, border='bisque4', lwd=.5)
zoomplot(c(978000, 999800), ylim=c(185000, 225000))

df <- data.frame(lat=bikes$start.station.latitude, lon=bikes$start.station.longitude)
head(df)

point.tab <- sort(table(paste(df$lat, df$lon)), decreasing = T)
point.tab[1:3]

df2 <- data.frame(lat = as.numeric(word(names(point.tab), 1))
                   , lon = as.numeric(word(names(point.tab), 2))
                   )
df2$size <- as.numeric(point.tab)

coordinates(df2) <- ~lon + lat

# projection
crs(df2) <- CRS('+proj=longlat + datum=WGS84')

# spatial transformation
df2 <- spTransform(df2, crs(nypp))

tmp.size <- .2 + (2 * df2$size / max(df2$size))
points(df2$lon, df2$lat, col='red', pch=19
       , cex=tmp.size)
