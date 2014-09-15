data <- read.delim("/Users/flannanbrody/github/dataviz-home/maps-intro/merged-multirace.txt")

sf <- subset(data, county == "San Francisco")

plot(sf$year, sf$pcthispanic, type="l", ylim=c(0,max(data$pcthispanic)), main="Hispanics in San Francisco County")

sf <- sf[order(sf$year), ]
plot(sf$year, sf$pcthispanic, type="l", ylim=c(0,max(data$pcthispanic)), main="Hispanics in San Francisco County")

data$hispanic_pop <- data$totalpop * data$pcthispanic
data$white_pop <- data$totalpop * data$pctwhite
data$black_pop <- data$totalpop * data$pctblack
data$asian_pop <- data$totalpop * data$pctasian
data$amind_pop <- data$totalpop * data$pctamind
data$other_pop <- data$totalpop * data$pctother

names(data)

ca_hispanic_pop <- aggregate(data$hispanic_pop, list(data$year), sum)
names(ca_hispanic_pop)
names(ca_hispanic_pop) <- c("year", "tot_hisp_pop")

total_ca_pop <- aggregate(data$totalpop, list(data$year), sum)
names(total_ca_pop)
names(total_ca_pop) <- c("year", "tot_ca_pop")

sf <- subset(data, county == "San Francisco")
sf <- sf[order(sf$year),]

state_totals$pcthispanic <- state_totals$tot_hisp_pop/state_totals$tot_ca_pop

state_totals <- merge(total_ca_pop, ca_hispanic_pop, by="year")
head(state_totals)

plot(state_totals$year, state_totals$pcthispanic, main="Calif. pct Hispanic", ylim=c(0,max(data$pcthispanic)), type="l", col="red", lwd=2)

plot(sf$pcthispanic, type="l", ylim=c(0,max(data$pcthispanic)) )

plot(state_totals$year, state_totals$pcthispanic, main="Calif. pct Hispanic", ylim=c(0,max(data$pcthispanic)), type="l", col="red", lwd=2)
lines(sf$year, sf$pcthispanic)

plot_lines_for_county <- function(county) {
  county_name <- county
  this_county <- subset(data, county == county_name)
  this_county <- this_county[order(this_county$year),]
  lines(this_county$year, this_county$pcthispanic)
}

plot(state_totals$year, state_totals$pcthispanic, main="Calif. pct Hispanic", ylim=c(0,max(data$pcthispanic)), type="l", col="red", lwd=2)
plot_lines_for_county("Los Angeles")

plot(state_totals$year, state_totals$pcthispanic, main="Calif. pct Hispanic", ylim=c(0,max(data$pcthispanic)), type="l", col="red", lwd=2)

#all the counties
for (i in unique(data$county)) {
  plot_lines_for_county(i)
}

plot_weighted_lines_for_county <- function(county) {
  county_name <- county
  this_county <- subset(data, county == county_name)
  this_county <- this_county[order(this_county$year),]
  lines(this_county$year, this_county$pcthispanic, lwd= this_county$hispanic_pop/100000 )
  
  y2020 <- subset(this_county, year == 2020)
  text(y2020$year, y2020$pcthispanic, labels=y2020$county, adj=0, cex=.5)
}

plot(state_totals$year, state_totals$pcthispanic, main="Calif. pct Hispanic with population sizing", ylim=c(0,max(data$pcthispanic)), type="l", col="red", lwd=2, xlim=c(1980, 2030))

#all the counties
for (i in unique(data$county)) {
  plot_weighted_lines_for_county(i)
}

install.packages("maptools")
library(maptools)

shapes <- readShapePoly("/Users/flannanbrody/github/dataviz-home/maps-intro/shapes/ca/counties.shp")

data.frame(shapes)

map_data <- data.frame(shapes)

data$FIPS <- sprintf("%05d",data$fips)

y2010 <- subset(data, year == 2010)

y2010$FIPS%in%map_data$FIPS

match(map_data$FIPS,y2010$FIPS)

match_order <- match(map_data$FIPS,y2010$FIPS)

y2010$pcthispanic
y2010$pcthispanic[match_order]

map_data$pctHispanic2010 <- y2010$pcthispanic[match_order]

hist(map_data$pctHispanic2010)

map_breaks <- c(0, .1, .2, .3, .4, 1)

buckets <- cut(map_data$pctHispanic2010,breaks=map_breaks)
numeric_buckets <- as.numeric(buckets)

colors <- brewer.pal(5,"YlOrRd")

colors[numeric_buckets]

plot(shapes, col=colors[numeric_buckets])
title("Pct Hispanic by County, 2010")










#multiple maps....small multiple in maps

#we'll need these
library(maptools)
library(RColorBrewer)

#load your data
data <- read.delim("merged-multirace.txt")

#download the shape file
shapes <- readShapePoly("shapes/ca/counties.shp")

#a data frame of the map attribute data
map_data <- data.frame(shapes)

#string work to add a leading zero and make a new vector that matches map_data's fips code.
data$FIPS <- sprintf("%05d",data$fips)


plot_counties_for_decade_and_race <- function(decade, race_field_name) {
  
  # only data for this decade
  this_decade <- subset(data, year == decade)
  
  #match order of data and shapes fields
  match_order <- match(map_data$FIPS,this_decade$FIPS)
  
  # add this field to map_data
  # NOTE: this syntax is weird. yes. but it's the same as:
  # map_data$pcthispanic <- this_decade$pcthispanic[match_order]
  map_data[,race_field_name] <- this_decade[,race_field_name][match_order]
  
  # breaks for colors. these are semi arbitrary, picked manually.
  map_breaks <- c(0, .1, .2, .3, .4, 1)
  
  # put our data into 5 groups
  buckets <- cut(map_data[,race_field_name], breaks = map_breaks)
  
  # make the groups numbers so they're less terrifying to look at.
  numeric_buckets <- as.numeric(buckets)
  
  #a vector of 5 colors. pick your own by typing display.brewer.all() in the R console.
  colors <- c("white", brewer.pal(4,"YlOrRd"))
  
  #plot the map!
  plot(shapes, col=colors[numeric_buckets], border="lightgrey")
  
  # give it a title so we know what we're looking at.
  title(paste(race_field_name, decade))
}

#fields we want to plot
race_fields <- c("pctwhite", "pcthispanic", "pctblack", "pctasian" )

# mfrow puts the plot commands into a 4x4 grid.
# mar helps with the margins for each plot so they're not tiny.
par(mfrow=c(4, 4), mar=c(1, 1, 1, 1))

for (i in race_fields) {
  for (j in c(1990, 2000, 2010, 2020)) {
    plot_counties_for_decade_and_race(j,i)
  }
}