# Data manipulation 

# [1] create grids for kriging interpolations 

# x <- seq(5800000, 6100000,400)
# y <- seq(19333000, 19633000,400)

x <- seq(5749040.9338320437818766, 6159040.9338320437818766,500)
y <- seq(19251409.7630450688302517, 19747409.7630450688302517,500)
data.grid <- meshgrid(x,y)

data.grid <- data.frame(unlist(matrix(data.grid$X)), unlist(matrix(data.grid$Y)))
colnames(data.grid) <- c("x","y")
data.grid$DEM = 0
coordinates(data.grid) <- ~ x + y
crs(data.grid) <- CRS("+proj=aea +lat_0=31.25 +lon_0=-100 +lat_1=27.5 +lat_2=35 +x_0=1500000 +y_0=6000000 +datum=NAD83 +units=us-ft +no_defs")
data.grid

# [2] crop DEM topography raster to the same extent as the river and measured water levels 

# create a empty raster to store topography DEM 
data.raster <- rasterFromXYZ(data.frame(data.grid@coords), res=c(500,500))
dim(data.raster) = c(992,820)
extent(data.raster) = c(5749040.9338320437818766, 6159040.9338320437818766,19251409.7630450688302517, 19747409.7630450688302517)
data.raster[] = 0
data.raster

DEM_raster <- raster(ncols = 1000, nrows= 1000)

crs(DEM_raster) <- CRS("+proj=aea +lat_0=31.25 +lon_0=-100 +lat_1=27.5 +lat_2=35 +x_0=1500000 +y_0=6000000 +datum=NAD83 +units=us-ft +no_defs ")
extent(DEM_raster) <- extent(data.grid)
DEM.raster <- resample(DEM, DEM_raster, method = "bilinear")
DEM.raster
    

# [3] Data wrangling, select specific cols for subsequent analysis and convert to spatial point data frame with correct CRS

library(dplyr)
library(pracma)
# choose the year of WL you want to work on
#data <- water_level %>%
#  dplyr::select(Longitude, Latitude, SWN, DEM, Depth, ScrTop1, ScrBot1, ScrTop2, ScrBot2, SH_Note, Aq2Use, WL2019,WL2020) %>%
#  # filter out NAs in both WL2019 and WL2020 to keep same number of measured water levels for both years
#  filter(WL2019 != 0 & WL2020 != 0) 
#head(data)

data <- water_level %>%
  dplyr::select(Longitude, Latitude, SWN, DEM, Depth, ScrTop1, ScrBot1, ScrTop2,
                ScrBot2, SH_Note, Aq2Use, cal_year) # hpham



# we can also only exclude NAs for individual years
#data_2019 <- data %>% 
#  dplyr::select(Longitude, Latitude, WL2019) %>%
#  filter(WL2019 != 'NA')

#data_2020 <- data %>% 
#  dplyr::select(Longitude, Latitude, WL2020) %>%
#  filter(WL2020 != 'NA')

data_2020 <- data %>% 
  dplyr::select(Longitude, Latitude, cal_year) %>%
  filter(data[,cal_year] != 0) # hpham



# create spatial point data frame with geographic coordinate systems
#data_sp2019 <- SpatialPointsDataFrame(SpatialPoints(data_2019[,1:2]), data.frame(data_2019[,3]))
#crs(data_sp2019) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

data_sp2020 <- SpatialPointsDataFrame(SpatialPoints(data_2020[,1:2]), data.frame(data_2020[,3]))
crs(data_sp2020) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# create spatial point data frames with projected coordinate systems
new_crs <- CRS("+proj=aea +lat_0=31.25 +lon_0=-100 +lat_1=27.5 +lat_2=35 +x_0=1500000 +y_0=6000000 +datum=NAD83 +units=us-ft +no_defs ")
#data_sp2019 <- spTransform(data_sp2019,new_crs)
data_sp2020 <- spTransform(data_sp2020,new_crs)

# rename the colnames
#colnames(data_sp2019@coords)[1:2] = c("x","y")
#colnames(data_sp2019@data)        = c("WL2019")  
colnames(data_sp2020@coords)[1:2] = c("x","y")
colnames(data_sp2020@data)        = c("WL2020")  

# create combined spatial point data frame for both years (2019 & 2020) with projected coordinate systems
#data_spadf <- SpatialPointsDataFrame(SpatialPoints(data[,1:2]),data=data[,12:13])
#crs(data_spadf) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#data_spadf <- spTransform(data_spadf,new_crs)
#data_spadf@bbox <- t(matrix(extent(data.grid),2))  #crop
#data_spadf

#data_regdf = as.data.frame(data_spadf)
#Normalized Data
normalized = function (x) {(x-min(x))/(max(x)-min(x))}                              # create a normalization function
#data_sp2019@data$WL2019n <- normalized(data_sp2019@data$WL2019)
data_sp2020@data$WL2020n <- normalized(data_sp2020@data$WL2020)
#data_spadf@data$WL2019n  <- normalized(data_spadf@data$WL2019)
#data_spadf@data$WL2020n  <- normalized(data_spadf@data$WL2020)

# [4] preprocessing the GAM (simulated water levels), and extract a subset 
# spatialpointdataframe to variogram and co-krige with measured water levels

# convert the selected GAM raster for either 2019 or 2020 to a spatial point data frame, specify the year of the data
sim_wl.data <- rasterToPoints(sim_WL2020, spatial = TRUE, progress = "window")
#str(sim_wl.data)

# rename the colname
colnames(sim_wl.data@data)[1] = "sim_wl"
# normalized the data
sim_wl.data@data$sim_wln <- normalized(sim_wl.data@data$sim_wl)
sim_wl.data
str(sim_wl.data)
summary(sim_wl.data)

# convert the spatial point df to a regular dataframe
sim_wl.dataframe <- as.data.frame(sim_wl.data)
colnames(sim_wl.dataframe) <- c("sim_wl","sim_wln", "x", "y")

#str(sim_wl.dataframe)

# subset 1 in every 500 data for variogram modeling
sim_wl.dataframesubset <- subset(sim_wl.dataframe[seq(1,nrow(sim_wl.dataframe), 500),])
str(sim_wl.dataframesubset)
head(sim_wl.dataframesubset)

# create a subset DEM spatialpointdataframe for variogram modeling
sim_wl2020.datasubset <- SpatialPointsDataFrame(SpatialPoints(sim_wl.dataframesubset[,3:4]),data=data.frame(sim_wl.dataframesubset[,1:2]))
crs(sim_wl2020.datasubset) <- crs(data_sp2020)

#sim_wl2019.datasubset <- SpatialPointsDataFrame(SpatialPoints(sim_wl.dataframesubset[,3:4]),data=data.frame(sim_wl.dataframesubset[,1:2]))
#crs(sim_wl2019.datasubset) <- crs(data_spadf)

#summary(sim_wl2019.datasubset)
head(sim_wl2020.datasubset)



# [5] Display histogram of the covariates (DEM) and target variables (measured water level)

# convert DEM raster to spatialpointdataframe
DEM.data <- rasterToPoints(DEM.raster, spatial=TRUE, progress="window")
DEM.data@bbox <- t(matrix(extent(data.grid),2))
# change the colname for the spatialpointdataframe
colnames(DEM.data@data)[1] = "DEM"
# normalized the data
DEM.data@data$DEMn <- normalized(DEM.data@data$DEM)
DEM.data
# convert to a dataframe
DEM.dataframe <- as.data.frame(DEM.data)
colnames(DEM.dataframe) <- c("DEM","DEMn", "x", "y")
DEM.dataframesubset <- subset(DEM.dataframe[seq(1,nrow(DEM.dataframe), 800),])
str(DEM.dataframesubset)
head(DEM.dataframesubset)

# create a subset DEM spatialpointdataframe for variogram modeling 
DEM.datasubset <- SpatialPointsDataFrame(SpatialPoints(DEM.dataframesubset[,3:4]),data=data.frame(DEM.dataframesubset[,1:2]))
DEM.datasubset@bbox <- t(matrix(extent(data.grid),2))
crs(DEM.datasubset) <- crs(DEM.data)
DEM.datasubset

plot(DEM.raster)

# plot the histograms to compare distributions 
h1 <- histogram(~ DEM, DEM.dataframe, xlab="DEM", col="lightblue",main ="DEM", nint=12)
#h2 <- histogram(~ DEM, river.data, xlab="River_DEM", col="red4", main = "River DEM", nint=12)
breaks = seq(150, 550, 25)
#h3 <- histogram(~ WL2020, data_regdf, xlab="Measured Water Level (2020)", main = "Measured Water Level", col="green", breaks = breaks)
#h4 <- histogram(~ WL2019, data_regdf, xlab="Measured Water Level (2019)", main = "Measured Water Level", col="green", breaks = breaks)
#length(data_regdf$WL2020)
#h1
#h2
#h3
#h4

