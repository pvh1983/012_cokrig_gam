
# Load libraries ==============================================================
library(rgdal)
library(smerc)                                 # Implements statistical methods for analyzing the counts of areal data, with a 
library(maptools)
library(sf)
library(raster)
library(rasterVis)                             # The rasterVis package complements the raster package, providing a set of methods for enhanced         
library(dismo)
library(spatial)
library(sp)
library(maxstat)
library(rgdal)
library(RColorBrewer)
library(LaplacesDemon)
library(imager)
library(pracma)
library(maps)
library(viridis)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)
library(ggplot2)
library(wesanderson)
library(mltools)
library(RColorBrewer)
library(gganimate)                             # create anamination
library(tools)
library(maps)
library(zoo)
library(lubridate)
library(ggnewscale)                            # use multiple colour and fill scales in ggplot2
library(gstat)
library(taRifx)
library(splines)

setwd("c:/Users/hpham/OneDrive - INTERA Inc/projects/012_rscript_geostats_s/scripts/")

# read input data =============================================================

# [1] Read in the water level .csv file ---------------------------------------
path_waterlevel = ("input/data/water_levels")
#water_level <- read.csv(paste(path_waterlevel,"/Shallow.Combined.Master_WellInfo_w_ShallowWLdata_1Yr_wBRAA_v2.csv",sep='')) # Used in OK
#water_level     <- read.csv(paste(path_waterlevel,"/Master_WellInfo_w_ShallowWLdata_1Yr_wBRAA_v2.csv",sep=''))
water_level     <- read.csv(paste(path_waterlevel,"/Master_WellInfo_w_ShallowWLdata_1Yr_wBRAA_v3.csv",sep=''))

RiverRegression <- read.csv(paste(path_waterlevel,"/Linear_Relationship_of_River_Elevation.csv", sep = ''))

# [2] Read in the spatial polygons for each acquifer --------------------------
path <- ("input/GIS/shapefiles/geology/District_Geology_Boundaries")
Yegua_Jackson <- readOGR(dsn = path, layer = "YJ_400_original")
Sparta        <- readOGR(dsn = path, layer = "SP_400_original")
Queen_City    <- readOGR(dsn = path, layer = "QC_400_original")
Carrizo       <- readOGR(dsn = path, layer = "CZ_400_original")
Calvert_Bluff <- readOGR(dsn = path, layer = "CB_400_original")
Simboro       <- readOGR(dsn = path, layer = "SB_400_original")
Hopper        <- readOGR(dsn = path, layer = "HP_400_original")
ShallowZone   <- readOGR(dsn = path, layer = "Total_400_original")
crs(ShallowZone) <- crs(Hopper)

# [3] Read in the spatial line df for river -----------------------------------
path_river = ("input/GIS/shapefiles/hydrology")
#river <-readOGR(dsn = path_river, layer = "NHD_Flowline_Clp_perennial_clean_GAM")
river <-readOGR(dsn = path_river, layer = "NHD_Flowline_Clp_perennial_clean_pts_clip")


# [3] Read the DEM .tif file --------------------------------------------------
# cliped DEM .tif file
#DEM <- raster("C:/Users/12819/Dropbox/My PC (DESKTOP-9VC04LJ)/Documents/Research/Ground Water/rasters/500ft_res/DEMs/DEM_ShallowArea_Resample500.tif")
#DEM <- raster("C:/Users/12819/Dropbox/My PC (DESKTOP-9VC04LJ)/Documents/Research/Ground Water/rasters/DEM_POSGCD_NeighboringCounties.tif")
DEM <- raster("input/GIS/rasters/DEM_ShallowArea_Resample500.tif") # hpham: This script reads a different DEM? 


D2River_Grid = readOGR(dsn = path_river, 
layer = "GridCells_500_within_6000_ft_Smooth_Perennial_Streams_wNearestSmoothStreamPt")

# import the pumping raster (difference in water level after pumping )
pumping500_2018 <- raster("input/GIS/rasters/500ft_res/Pumping.Drawdown/Shallow.Combined.DrawDown.1929.2018.500ft.pst15.tif")
pumping500_2019 <- raster("input/GIS/rasters/500ft_res/Pumping.Drawdown/Shallow.Combined.DrawDown.1929.2019.500ft.pst15.tif")
pumping500_2020 <- raster("input/GIS/rasters/500ft_res/Pumping.Drawdown/Shallow.Combined.DrawDown.1929.2020.500ft.pst15.tif")

# import the GAM (simulated water levels) 
#sim_WL2020 <- raster("input/GIS/rasters/500ft_res/Hds.Surfaces.Rasters.500ft.pst15/Shallow.Combined.Hds.2020.500ft.pst15.tif")
#sim_WL2019 <- raster("input/GIS/rasters/500ft_res/Hds.Surfaces.Rasters.500ft.pst15/Shallow.Combined.Hds.2019.500ft.pst15.tif")

# Ross updated these rasters on 08/26/2020
sim_WL2019 <- raster("input/GIS/rasters/500ft_res/Hds.Surfaces.Rasters.500ft.pst18.YJ/Shallow.Combined.Hds.2020.500ft.pst18.YJ.tif")
sim_WL2020 <- raster("input/GIS/rasters/500ft_res/Hds.Surfaces.Rasters.500ft.pst18.YJ/Shallow.Combined.Hds.2020.500ft.pst18.YJ.tif")



#plot(sim_WL2020)
#plot(sim_WL2019)

# import pumping drawdown information 
pumpingdrawdown_path = ("input/data/pumping")
pumping_drawdown2018 = readOGR(dsn = pumpingdrawdown_path, layer = "Pts.Shallow.Below.DD.2018.500ft.pst18.YJ")
pumping_drawdown2019 = readOGR(dsn = pumpingdrawdown_path, layer = "Pts.Shallow.Below.DD.2019.500ft.pst18.YJ")
pumping_drawdown2020 = readOGR(dsn = pumpingdrawdown_path, layer = "Pts.Shallow.Below.DD.2020.500ft.pst18.YJ")


