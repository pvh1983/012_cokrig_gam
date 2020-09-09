# Modelling the spatial structure of the target variable - make variogram
# 1. the variogram method computes the experimental variogram as a variogram object
# 2. the vgm method creates a variogram model object
# 3. the fit.variogram method adjusts a variogram model object to a variogram object


# look at the spatial point data frame
#data_spadf
#str(data_spadf)


# plot the empirical variogram for measured water levels at specific years
v_WL2020n <- variogram(WL2020n ~ 1 , data=data_sp2020, cutoff=thre_cutoff, width=thre_width)
#str(v_WL2020n)
#summary(v_WL2020n)
#v_WL2020n$np
#v_WL2020n$dist

# Plot empirical variogram
plot(v_WL2020n, main = "Empirical Variogram",pl = T)

# create a model for the empirical variogram
variogram_model_WL2020n <- vgm(0.05,"Gau",100000,0.003)
#str(variogram_model_WL2020n)
#summary(variogram_model_WL2020n)

# plot both the fitted variogram model with the empirical variogram
#plot(v_WL2020n, pl=T, model=variogram_model_WL2020n, main = "Variogram Model")
#(variogram_model_WL2020n_fit <- fit.variogram(v_WL2020n, variogram_model_WL2020n))

variogram_model_WL2020n_fit <- fit.variogram(v_WL2020n, variogram_model_WL2020n)
#str(variogram_model_WL2020n_fit)
#summary(variogram_model_WL2020n_fit)


# export fig to png
ofile = paste("output/",method,"_fitted_variogram_",data_col_name,".png", sep="")
png(ofile,width=10,height=8,units="in",res=300)
vplot <- plot(v_WL2020n, pl=T, model=variogram_model_WL2020n_fit, 
              main = paste("data=", data_col_name))
#plot(v_WL2020n, pl=T, model=variogram_model_WL2020n_fit, 
#              main = paste("Fitted Variogram, data=", data_col_name))

print(vplot,width = 3, height = 3, dpi = 300)
dev.off()

# Save to a csv file
ofile = paste("output/", method, "_fitted_variogram_",data_col_name,".csv",sep="")
write.csv(v_WL2020n,ofile)



## Ordinary Kriging Interpolation of the Target Variable

# use krige function for oridinary kriging 
ko <- krige(WL2020n~1, locations=data_sp2020, newdata=data.grid, model=variogram_model_WL2020n_fit)
# extract the kriging estimates and kriging variances 
#ko_pred <- data.frame(ko$var1.pred)
#ko_var <- data.frame(ko$var1.var)

ko@data$var1.pred <- ifelse(ko@data$var1.pred >= 1.0, 1, ko@data$var1.pred)
ko@data$var1.pred <- ifelse(ko@data$var1.pred <= 0.0, 0, ko@data$var1.pred)

summary(ko)

min <- min(data_sp2020@data$WL2020)
max <- max(data_sp2020@data$WL2020)

# back transform to original scales
InvNormalize <- function (x) {x * (max - min) + min}
ko@data$WL_pred <- InvNormalize(ko@data$var1.pred)
ko@data$WL_var <- ko@data$WL_pred*ko@data$var1.var
summary(ko)

# # use spplot to plot the kriging results 
# ofile = paste("output/WL_raster_",data_col_name,"_cutoff_",toString(thre_cutoff),".png",sep="")
# png(ofile,width=10,height=8,units="in",res=300)
# tmp_png <- sp::spplot(ko, "WL_pred", pch=19, col = "transparent",
#      scales=list(draw=TRUE), main="Ordinary Kirging Estimates")
# print(tmp_png)
# dev.off()
# 
# #sp::spplot(ko, "var1.var", pch=19, col = "transparent",
# #     scales=list(draw=TRUE), main="Ordinary Kirging Estimates")



# plot contours

ko.dataframe = as.data.frame(ko)

# Write to csv
ofile = paste("output/", method ,"_pred_wl_",data_col_name,".csv",sep="")
write.csv(ko.dataframe,ofile)

# Export interpolated water levels
ofile = paste("output/", method, "_pred_wl_",data_col_name,".png",sep="")
#png(ofile,width=10,height=8,units="in",res=300)
ggplot(ko.dataframe, aes(x ,y, z = WL_pred)) +
  geom_contour_filled(bins = 20)
ggsave(ofile, width = 8, height = 6, dpi = 300)
#dev.off()

# Export Kriging variance
ofile = paste("output/", method, "_kriging_var_",data_col_name,".png",sep="")
#png(ofile,width=10,height=8,units="in",res=300)
ggplot(ko.dataframe, aes(x ,y, z = WL_var)) +
  geom_contour_filled(bins = 20)
ggsave(ofile, width = 8, height = 6, dpi = 300,)
#dev.off()



#var <- ggplot(ko.dataframe, aes(x ,y, z = var1.var)) +
#  geom_contour_filled(bins = 20)
#pred


# extract values for each acquifer
# 
# # extract values of each spatial polygon from the kriging estimates, which is a spatial point data frame
# Yegua_Jackson_df      <- as.data.frame(ko[Yegua_Jackson,])
# Sparta_df             <- as.data.frame(ko[Sparta,])
# Queen_City_df         <- as.data.frame(ko[Queen_City,])
# Carrizo_df            <- as.data.frame(ko[Carrizo,])
# Calvert_Bluff_df      <- as.data.frame(ko[Calvert_Bluff,])
# Simboro_df            <- as.data.frame(ko[Simboro,])
# Hopper_df             <- as.data.frame(ko[Hopper,])
# ShallowZone_df        <- as.data.frame(ko[ShallowZone,]) 
# 
# # find the min and max of the original scale measured water levels
# 
# # find one average value for each acquifer on the original scale using InvNormalize function
# tol_avg   <- mean(InvNormalize(ko$var1.pred))
# YJ_avg    <- mean(InvNormalize(Yegua_Jackson_df$var1.pred))
# SPA_avg   <- mean(InvNormalize(Sparta_df$var1.pred))
# QC_avg    <- mean(InvNormalize(Queen_City_df$var1.pred))
# CAR_avg   <- mean(InvNormalize(Carrizo_df$var1.pred))
# CB_avg    <- mean(InvNormalize(Calvert_Bluff_df$var1.pred))
# SIM_avg   <- mean(InvNormalize(Simboro_df$var1.pred))
# HOP_avg   <- mean(InvNormalize(Hopper_df$var1.pred))
# SHAZ_avg  <- mean(InvNormalize(ShallowZone_df$var1.pred))
# OK_avg <- data.frame(name = c("YJ", "SPA", "QC", "CAR", "CB", "SIM", "HOP", "TOL", "SHAZ"), avg = c(YJ_avg, SPA_avg, QC_avg, CAR_avg, CB_avg, SIM_avg, HOP_avg, tol_avg, SHAZ_avg))
# OK_avg
# 
# 
# 
# 
# 
# # create raster and write on file
# 
# 
# 
# #ko@data$WL_pred <- InvNormalize(ko$var1.pred)
# #ko@data$WL_pred <- InvNormalize(ko$var1.pred)
# #ko$WL_pred = InvNormalize(ko$var1.pred)
# #ko$WL_var = ko$WL_pred * ko$var1.var
# 
# 
# ko.raster <- raster(nrows = 10000,ncols =10000)
# crs(ko.raster) <- CRS("+proj=aea +lat_0=31.25 +lon_0=-100 +lat_1=27.5 +lat_2=35 +x_0=1500000 +y_0=6000000 +datum=NAD83 +units=us-ft +no_defs")
# extent(ko.raster) <- extent(ko)
# ko.raster <- rasterize(ko, ko.raster, field = "var1.pred", fun = mean)
# ko.raster
# #writeRaster(ko.raster, "output/CKDEM_raster_2020", "GTiff", overwrite = TRUE)

# specify the image path on disk
#image_path = "output/Shapefile/"
# create raster and store it 
# only extract the predicted water levels 
#ko@data$WL_pred <- InvNormalize(ko$var1.pred)
#ko_tmp <- ko[,-(1:2)]

#ko_tmp
# ko.raster <- raster(nrows = 10000,ncols =10000)
# crs(ko.raster) <- CRS("+proj=aea +lat_0=31.25 +lon_0=-100 +lat_1=27.5 +lat_2=35 +x_0=1500000 +y_0=6000000 +datum=NAD83 +units=us-ft +no_defs")
# extent(ko.raster) <- extent(ko)
# ko.raster <- rasterize(ko, ko.raster, field = "WL_pred", fun = mean)
# ko.raster

# write the kriging estimates as a spatial point data frame and save as a shape file 
#writeOGR(obj = ko_tmp, layer = "OK_2020", dsn = image_path, driver = "ESRI Shapefile", overwrite_layer = TRUE)

# references
# http://gsp.humboldt.edu/OLM/R/04_01_Variograms.html