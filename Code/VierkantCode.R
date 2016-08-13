# PROJECT: Code to prepare maps on basis of CBS 100m Raster for the Netherlands
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# # Code to prepare data for Joy Division Style plot
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````

# PACKAGES
BasePackages<- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr", "data.table", "foreach", "doParallel")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)

# SET WORKING DIRECTORY
wdpath<-"D:\\Data\\Maps\\CBS_Vierkant_data"
setwd(wdpath)

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# SETUP MAPS
datapath<-"D:\\Data\\Maps\\CBS_Vierkant_data\\2014-cbs-vierkant-500m"

# Get map
dsn=paste(datapath, "CBSvierkant500m201410.shp", sep="/")
#ogrListLayers(dsn)
#ogrInfo(dsn, layer="CBSvierkant500m201410")
NL500m<-readOGR(dsn, layer = "CBSvierkant500m201410")
NL500m.data<-NL500m@data # save attribute information as dataframe (note that @ is used instead of $)
#NL500m_geom<-fortify(NL500m, region="OBJECTID") # transform shapefile in dataframe for ggplot. OBJECTID is the ID for the countries. ISO3 and ISONUM are not defind for some small islands so not used here!

# # Determine target region Amsterdam for checking
# TargetRegion<-"Amsterdam"
# TargetRegion.p<-qmap(TargetRegion, zoom=10, color = "color", legend = "topleft") # qmap is identical to getmap and ggmap combined.
# TargetRegion.p
# TargetC<-gglocator(2)
# TargetC
# TargetBBox<-matrix(TargetC, nrow=2, ncol=2, byrow=FALSE, 
#                    dimnames=list(c("x","y"),
#                                  c("min", "max")))
# 
# # read in map of the Netherlands as a SpatialPolygonsDataFrame
# NL <- getData('GADM', country = "NLD", level = 1) 
# # Reproject to NL projection from degrees to (probably) meters
# NL_rp<-spTransform(NL, CRS(proj4string(NL500m)))
# # Select Noord Holland for testing
# N_H<-NL_rp[NL_rp$NAME_1=="Noord-Holland",]
# # Clip Noord_Holland from NL vierkant map. 
# #N_H500m<-crop(NL500m, N_H)
# #save(N_H500m, file="N_H500m.Rdata")
# load("N_H500m.Rdata")
# # Create raster with extent of 500 x 500 m
# # First analyse extent of NL projection
# extent(N_H500m)
# # Round to km and calc lenght in km
# ext<-extent(94000,150000, 464000,577000)
# length(94:150)
# length(464:577)
# # Create raster. 
# #Note that length 57, 114 leads to a res of slightly lower than 1000 m probably because of rounding 56 adn 113 results in exactly 1000m 
# r <- raster(ext, ncol=56, nrow=113)
# # Doubling leads to a raster of 500 x 500
# r <- raster(ext, ncol=112, nrow=226)
# #NH500m.r<-rasterize(N_H500m, r)
# #save(NH500m.r, file="NH500m.r.Rdata")
# load("NH500m.r.Rdata")

extent(NL500m)
# Round to km and calc lenght in km
ext<-extent(13000,279000, 307000,620000)
length(13:279)
length(307:620)
# #Note that length 267, 314 leads to a res of slightly lower than 1000 m probably because of rounding now it results in exactly 1000m 
# Create raster. 
r <- raster(ext, ncol=266, nrow=313)
# Doubling leads to a raster of 500 x 500
r <- raster(ext, ncol=532, nrow=626)
#NL500m.r<-rasterize(NL500m, r)
save(NL500m.r, file="NL500m.r.Rdata")
load("NL500m.r.Rdata")

# Select one of the attributes and turn it into a raster
Data.r<- deratify(NL500m.r, "INW2014")
# remove missing values indicated by certain numbers
Data.r[Data.r %in% c(-99997, -99998, -99999)]<-NA

# Convert the raster file to a points file
Data.points <- rasterToPoints(Data.r)
all.data <- as.data.table(Data.points)
setnames(all.data, c("x", "y", "value"))

# If you have your data in a CSV file, use this instead
# file <- "./DataSets/NBBuildingsWGS84.csv"
# all.data <- data.table(fread(file))

# The following are used to manipulate various data sets
# colnames(all.data) <- c("Name", "Mass", "Latitude", "Longitude") # Meteorites
# all.data$X <- as.numeric(all.data$X)
# all.data$Y <- as.numeric(all.data$Y)
# all.data$Mass <- as.numeric(all.data$Mass)

startEnd <- function(lats, lngs) {
  # Find the "upper left" (NW) and "bottom right" (SE) coordinates 
  # of a set of data.
  #
  # Args:
  #  lats: A list of latitude coordinates
  #  lngs: A list of longitude coordinates
  #
  # Returns: 
  #   A list of values corresponding to the northwest-most and 
  # southeast-most coordinates
  
  # Convert to real number and remove NA values
  lats <- na.omit(as.numeric(lats))
  lngs <- na.omit(as.numeric(lngs))
  
  topLat <- max(lats)
  topLng <- min(lngs)
  botLat <- min(lats)
  botLng <- max(lngs)
  
  return(c(topLat, topLng, botLat, botLng))
}

startEndVals <- startEnd(all.data$y, all.data$x)
remove(startEnd)

startLat <- startEndVals[1]
endLat <- startEndVals[3]
startLng <- startEndVals[2]
endLng <- startEndVals[4]
remove(startEndVals)

num_intervals = 100.0
interval <- (startLat - endLat) / num_intervals
remove(num_intervals)

lat.list <- seq(startLat, endLat + interval, -1*interval)

# testLng <- -66.66152983 # Fredericton
# testLat <- 45.96538183 # Fredericton

# Prepare the data to be sent in
# If you have a value you want to sum, use this
data <- all.data[,list(x, y, value)]

# If you want to perform a count, use this
# data <- all.data[,list(x, y)]
# data[,Value:=1]

sumInsideSquare <- function(pointLat, pointLng, interval, data) {
  # Sum all the values that fall within a square on a map given a point,
  # an interval of the map, and data that contains lat, lng and the values
  # of interest
  
  setnames(data, c("lng", "lat", "value"))
  
  # Get data inside lat/lon boundaries
  lng.interval <- c(pointLng, pointLng + interval)
  lat.interval <- c(pointLat - interval, pointLat)
  data <- data[lng %between% lng.interval][lat %between% lat.interval]
  
  return(sum(data$value))
}

# Debugging
# squareSumTemp <- sumInsideSquare(testLat, testLng, interval, data)

# Given a start longitude and an end longitude, calculate an array of values
# corresponding to the sums for that latitude

calcSumLat <- function(startLng, endLng, lat, interval, data) {
  row <- c()
  lng <- startLng
  while (lng < endLng) {
    row <- c(row, sumInsideSquare(lat, lng, interval, data))
    lng <- lng + interval
  }
  
  return(row)
}

# Debugging
# rowTemp <- calcSumLat(startLng, endLng, testLat, interval, data)
# write.csv(rowTemp, file = "Temp.csv", row.names = FALSE)

# Set up parallel computing with the number of cores you have
#cl <- makeCluster(detectCores(), outfile = "./Progress.txt") # does not work on windows
#getDoParWorkers()
cl<-makeCluster(2)
registerDoParallel(cl)

all.sums <- foreach(lat=lat.list, .packages=c("data.table")) %dopar% {
  
  lat.data <- calcSumLat(startLng, endLng, lat, interval, data)
  
  # Progress indicator that works on Mac/Windows
  print((startLat - lat)/(startLat - endLat)*100) # Prints to Progress.txt
  
  lat.data
  
}

#stopCluster(cl = NULL)

# Convert to data frame
all.sums.table <- as.data.table(all.sums)


# Save to disk so I don't have to run it again
if (!file.exists("./GeneratedData")) {
  dir.create("./GeneratedData")
}
output.file <- "./GeneratedData/NL500m.csv"
write.csv(all.sums.table, file = output.file, row.names = FALSE)

# End timer
totalTime <- proc.time() - start
print(totalTime)

# remove(cl, endLat, endLng, startLat, startLng, lat.list, start, calcSumLat, sumInsideSquare, interval)


library(foreach)
library(doParallel)
library(data.table)
library(raster)

# Time the code
start <- proc.time()

if (!file.exists("./DataSets")) {
  dir.create("./DataSets")
}

# Data Source:
# http://sedac.ciesin.columbia.edu/data/set/gpw-v3-population-count/data-download
# Format: .ascii, 1/2 degree, 2000

population.file <- "./DataSets/glp00ag30.asc"
# Load the raster file
population.raster <- raster(population.file)
# Convert the raster file to a points file
population.points <- rasterToPoints(population.raster)
all.data <- as.data.table(population.points)
setnames(all.data, c("x", "y", "population"))

# If you have your data in a CSV file, use this instead
# file <- "./DataSets/NBBuildingsWGS84.csv"
# all.data <- data.table(fread(file))


# The following are used to manipulate various data sets
# colnames(all.data) <- c("Name", "Mass", "Latitude", "Longitude") # Meteorites
# all.data$X <- as.numeric(all.data$X)
# all.data$Y <- as.numeric(all.data$Y)
# all.data$Mass <- as.numeric(all.data$Mass)

startEnd <- function(lats, lngs) {
  # Find the "upper left" (NW) and "bottom right" (SE) coordinates 
  # of a set of data.
  #
  # Args:
  #  lats: A list of latitude coordinates
  #  lngs: A list of longitude coordinates
  #
  # Returns: 
  #   A list of values corresponding to the northwest-most and 
  # southeast-most coordinates
  
  # Convert to real number and remove NA values
  lats <- na.omit(as.numeric(lats))
  lngs <- na.omit(as.numeric(lngs))
  
  topLat <- max(lats)
  topLng <- min(lngs)
  botLat <- min(lats)
  botLng <- max(lngs)
  
  return(c(topLat, topLng, botLat, botLng))
}

startEndVals <- startEnd(all.data$y, all.data$x)
remove(startEnd)

startLat <- startEndVals[1]
endLat <- startEndVals[3]
startLng <- startEndVals[2]
endLng <- startEndVals[4]
remove(startEndVals)

num_intervals = 200.0
interval <- (startLat - endLat) / num_intervals
remove(num_intervals)

lat.list <- seq(startLat, endLat + interval, -1*interval)

# testLng <- -66.66152983 # Fredericton
# testLat <- 45.96538183 # Fredericton

# Prepare the data to be sent in
# If you have a value you want to sum, use this
data <- all.data[,list(x, y, population)]

# If you want to perform a count, use this
# data <- all.data[,list(x, y)]
# data[,Value:=1]

sumInsideSquare <- function(pointLat, pointLng, interval, data) {
  # Sum all the values that fall within a square on a map given a point,
  # an interval of the map, and data that contains lat, lng and the values
  # of interest
  
  setnames(data, c("lng", "lat", "value"))
  
  # Get data inside lat/lon boundaries
  lng.interval <- c(pointLng, pointLng + interval)
  lat.interval <- c(pointLat - interval, pointLat)
  data <- data[lng %between% lng.interval][lat %between% lat.interval]
  
  return(sum(data$value))
}

# Debugging
# squareSumTemp <- sumInsideSquare(testLat, testLng, interval, data)

# Given a start longitude and an end longitude, calculate an array of values
# corresponding to the sums for that latitude

calcSumLat <- function(startLng, endLng, lat, interval, data) {
  row <- c()
  lng <- startLng
  while (lng < endLng) {
    row <- c(row, sumInsideSquare(lat, lng, interval, data))
    lng <- lng + interval
  }
  
  return(row)
}

# Debugging
# rowTemp <- calcSumLat(startLng, endLng, testLat, interval, data)
# write.csv(rowTemp, file = "Temp.csv", row.names = FALSE)

# Set up parallel computing with the number of cores you have
cl <- makeCluster(detectCores(), outfile = "./Progress.txt")
cl <- makeCluster(2)
registerDoParallel(cl)

all.sums <- foreach(lat=lat.list, .packages=c("data.table")) %dopar% {
  
  lat.data <- calcSumLat(startLng, endLng, lat, interval, data)
  
  # Progress indicator that works on Mac/Windows
  print((startLat - lat)/(startLat - endLat)*100) # Prints to Progress.txt
  
  lat.data
  
}

stopCluster(cl = NULL)

# Convert to data frame
all.sums.table <- as.data.table(all.sums)


# Save to disk so I don't have to run it again
if (!file.exists("./GeneratedData")) {
  dir.create("./GeneratedData")
}
output.file <- "./GeneratedData/WorldPopulation2.csv"
write.csv(all.sums.table, file = output.file, row.names = FALSE)

# End timer
totalTime <- proc.time() - start
print(totalTime)

# remove(cl, endLat, endLng, startLat, startLng, lat.list, start, calcSumLat, sumInsideSquare, interval)


# Other plot data
data(world.cities) # world city database from Maps
Capitals<-subset(world.cities, capital==1)
WorldMap.p<-ggplot()+
  geom_polygon(data=WorldMap_geom, aes(long,lat, group=group), colour="black", fill="white")
+
  geom_point(data=Capitals,aes(x=long, y=lat, size=pop), colour="green")+
  coord_equal()+
  labs(x="", y="",  size="Capitals")+
  theme_bw()+
  theme(legend.key=element_blank())
WorldMap.p

# Determine target region
#TargetRegion<-"Eastern Europe"
#TargetRegion.p<-qmap(TargetRegion, zoom=4, color = "color", legend = "topleft") # qmap is identical to getmap and ggmap combined.
#TargetRegion.p
#TargetC<-gglocator(2)
TargetBBox<-matrix(c(8, 61, 55, 31), nrow=2, ncol=2, byrow=FALSE, 
                   dimnames=list(c("x","y"),
                                 c("min", "max")))
# Crop WorldMap and fortify
WorldMap.Target<- crop(WorldMap, extent(TargetBBox))
WorldMap.Target.fort<-fortify(WorldMap.Target)


# Change extent because it does not correspond with standard (longitude 0=GreenWich)
extent(wheat_yg)<-c(-180, 180,-90, 90)
# Crop to TargetRegion
Wheat.p<-crop(wheat_yg, extent(TargetBBox))
# Transform raster to data.frame


wheat_yg_geom<-raster2dataframe.f(Wheat.p)
df<-wheat_yg_geom

# Get Infrastructure data
datapath<-"D:\\Data\\IPOP\\Data\\"
Infra1<-readGDAL(paste(datapath,"MarketVerburg", "mkt_infind_5m", sep="/"))
Infra1<-raster(Infra1)
plot(Infra1)
Infra1.Target<-crop(Infra1, TargetBBox)
plot(Infra1.Target)


Infra2<-readGDAL(paste(datapath,"MarketVerburg", "mkt_access_5m", sep="/"))
Infra2<-raster(Infra2)
Infra2.Target<-crop(Infra2, TargetBBox)
plot(Infra2.Target)
Infra2.fort<-fortify(Infra2.Target)



Infra2.fort<-PrepRasterPlot.f(TargetBBox, Infra2)

wheat_yg_geom<-raster2dataframe.f(Wheat.p)
# Get Yield Gap map from Licker
datapath<-"D:\\Data\\IPOP\\Data\\"
Licker_Wheat_yg<-raster(paste(datapath,"Licker_2010_data/Yield_gap", "wheat_yield_gap.nc", sep="\\"))
# Change extent because it does not correspond with standard (longitude 0=GreenWich)
Licker_Wheat_yg.fort<-PrepRasterPlotforNC.f(TargetBBox, Licker_Wheat_yg)
MapPlot.f(Licker_Wheat_yg.fort, "Licker Yield Gap")

# Plot FAO GAEZ yield gap
GAEZ_Wheat_yg<-readGDAL(paste(datapath,"GAEZ", "Wheat_yield_gap", "data.asc", sep="/"))
GAEZ_Wheat_yg.fort<-PrepRasterPlot.f(TargetBBox, GAEZ_Wheat_yg)
MapPlot.f(GAEZ_Wheat_yg.fort, "GAEZ Yield Gap")
plot(GAEZ_Wheat_yg)

# Plot Target Region using WorldMap.Target as basis
raster2dataframe.f<-function(rasterfile){
  TMP<-rasterfile %>% rasterToPoints %>% data.frame %>%
    dplyr::rename(Long=x, Lat=y)
  return(TMP)
}

PrepRasterPlot.f<-function(TargetBBox, file){
  TMP<-raster(file)
  TMP<-crop(TMP,TargetBBox) 
  TMP<-raster2dataframe.f(TMP)
  names(TMP)<-c("Long", "Lat", "value")
  return(TMP)
}

PrepRasterPlotforNC.f<-function(TargetBBox, file){
  extent(file)<-c(-180, 180,-90, 90)
  TMP<-crop(file,TargetBBox) 
  TMP<-raster2dataframe.f(TMP)
  names(TMP)<-c("Long", "Lat", "value")
  return(TMP)
}


MapPlot.f<-function(rasterfile, legendTitle){
  ggplot()+
    geom_polygon(data=WorldMap.Target.fort, aes(long,lat, group=group), colour="black", fill="white")+
    geom_raster(data=rasterfile, aes(y=Lat, x=Long, fill = value)) +
    geom_path(data=WorldMap.Target.fort, aes(long,lat, group=group), colour="black")+
    coord_equal()+
    theme_bw()+
    ylab("")+xlab("")+ggtitle("")+
    theme(panel.background=element_rect(fill="lightsteelblue1"), # colour background
          panel.grid.major = element_blank(),  # Remove major grid
          panel.grid.minor = element_blank())+ # Remove minor grid
    scale_x_continuous(expand=c(0, 0))+ # Removes standard space to left and right of plot
    scale_y_continuous(expand=c(0, 0))+ # Removes standard space above and below plot
    scale_fill_gradientn(colours = rev(terrain.colors(10)), 
                         guide = guide_colorbar(title=legendTitle)) # Control legend
  # or scale_fill_gradient(low="darkorange", high="darkorchid4") # for colour palette
}



TMP<-raster2dataframe.f(x)
names(TMP)<-c("Long", "Lat", "value")
ggplot()+
  geom_raster(data=TMP, aes(y=Lat, x=Long, fill = value))+
  coord_equal()+
  theme_bw()+
  ylab("")+xlab("")+ggtitle("")+
  #theme(panel.background=element_rect(fill="lightsteelblue1"), # colour background
  #      panel.grid.major = element_blank(),  # Remove major grid
  #      panel.grid.minor = element_blank())+ # Remove minor grid
  scale_x_continuous(expand=c(0, 0))+ # Removes standard space to left and right of plot
  scale_y_continuous(expand=c(0, 0))+ # Removes standard space above and below plot
  scale_fill_gradientn(colours = rev(terrain.colors(10)), 
                       guide = guide_colorbar(title="TEST")) # Control legend

PrepRasterPlot.f<-function(TargetBBox, file){
  TMP<-raster(file)
  TMP<-crop(TMP,TargetBBox) 
  TMP<-raster2dataframe.f(TMP)
  names(TMP)<-c("Long", "Lat", "value")
  return(TMP)
}


raster2dataframe.f<-function(rasterfile){
  TMP<-rasterfile %>% rasterToPoints %>% data.frame %>%
    dplyr::rename(Long=x, Lat=y)
  return(TMP)
}
