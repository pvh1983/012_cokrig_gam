# [1] extract values for each acquifer for cokriging with DEM

# extract values from spatial polygons
Yegua_Jackson_df      <- as.data.frame(k.cc[Yegua_Jackson,])
Sparta_df             <- as.data.frame(k.cc[Sparta,])
Queen_City_df         <- as.data.frame(k.cc[Queen_City,])
Carrizo_df           <- as.data.frame(k.cc[Carrizo,])
Calvert_Bluff_df      <- as.data.frame(k.cc[Calvert_Bluff,])
Simboro_df        <- as.data.frame(k.cc[Simboro,])
Hopper_df             <- as.data.frame(k.cc[Hopper,])
ShallowZone_df        <- as.data.frame(k.cc[ShallowZone,]) 
min <- min(data_spadf@data$WL2020)
max <- max(data_spadf@data$WL2020)
# back transform to original scales
InvNormalize <- function (x) {x * (max - min) + min}

# find one average value for each acquifer
tol_avg   <- mean(k.cc$WL.PUMPING.predn)
YJ_avg    <- mean(Yegua_Jackson_df$WL.PUMPING.predn)
SPA_avg   <- mean(Sparta_df$WL.PUMPING.predn)
QC_avg    <- mean(Queen_City_df$WL.PUMPING.predn)
CAR_avg   <- mean(Carrizo_df$WL.PUMPING.predn)
CB_avg    <- mean(Calvert_Bluff_df$WL.PUMPING.predn)
SIM_avg   <- mean(Simboro_df$WL.PUMPING.predn)
HOP_avg   <- mean(Hopper_df$WL.PUMPING.predn)
SHAZ_avg  <- mean(ShallowZone_df$WL.PUMPING.predn)
CO_avg <- data.frame(name = c("YJ", "SPA", "QC", "CAR", "CB", "SIM", "HOP", "TOL", "SHAZ"), avg = c(YJ_avg, SPA_avg, QC_avg, CAR_avg, CB_avg, SIM_avg, HOP_avg, tol_avg, SHAZ_avg))
CO_avg



# [2] create raster and write on file
# create raster and store it 
k.cc@data$WL2020.predN <- InvNormalize(k.cc$WL2020.pred)
k.cc
k.temp <- k.cc[,-(1:5)]
k.temp
# ck.raster <- raster(nrows = 10000,ncols =10000)
# crs(ck.raster) <- CRS("+proj=aea +lat_0=31.25 +lon_0=-100 +lat_1=27.5 +lat_2=35 +x_0=1500000 +y_0=6000000 +datum=NAD83 +units=us-ft +no_defs")
# extent(ck.raster) <- extent(ko)
# ck.raster <- rasterize(k.c, ck.raster, field = "WL2020.predN", fun = mean)
# ck.raster
#writeRaster(ck.raster, "CKDEM_raster_2020", "GTiff", overwrite = TRUE)
image_path = "output/Shapefile"
writeOGR(obj = k.temp, layer = "CK_TOPORIVER_2019", dsn = image_path, driver = "ESRI Shapefile", overwrite_layer = TRUE)
