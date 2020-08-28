# Modelling the spatial structure of the target variable - make variogram
# 1. the variogram method computes the experimental variogram as a variogram object
# 2. the vgm method creates a variogram model object
# 3. the fit.variogram method adjusts a variogram model object to a variogram object

# look at the spatial point data frame
#data_spadf$WL2020n

# plot the empirical variogram for measured water levels at specific years
v.WLn <- variogram(WL2020n ~ 1 , data=data_sp2020, cutoff=thre_cutoff, width=thre_width)
plot(v.WLn, main = "Empirical Variogram",pl = T)

# create a model for the empirical variogram
m.WLn <- vgm(0.05,"Gau",200000,0.003)

# plot both the fitted variogram model with the empirical variogram
plot(v.WLn, pl=T, model=m.WLn, main = "Variogram Model")
m.WLn.f <- fit.variogram(v.WLn, m.WLn)
#plot(v.WLn, pl=T, model=m.WLn.f, main = "Fitted OK Variogram Model")




## Ordinary Kriging Interpolation of the Target Variable

# use krige function for oridinary kriging 
ko <- krige(WL2020n~1, locations=data_sp2020, newdata=data.grid, model=m.WLn.f)
# extract the kriging estimates and kriging variances 
ko_pred <- data.frame(ko$var1.pred)
ko_var <- data.frame(ko$var1.var)

# find the min and max of the original scale measured water levels
min <- min(data_sp2020@data$WL2020)
max <- max(data_sp2020@data$WL2020)
# back transform to original scales

InvNormalize <- function (x) {x * (max - min) + min}

ko$var1.predn = InvNormalize(ko$var1.pred)

# use spplot to plot the kriging results 
#sp::spplot(ko, "var1.pred", pch=19, col = "transparent",
#     scales=list(draw=TRUE), main="Ordinary Kirging Estimates")
#sp::spplot(ko, "var1.var", pch=19, col = "transparent",
#     scales=list(draw=TRUE), main="Ordinary Kirging Estimates")


# plot contours

ko.dataframe = as.data.frame(ko)
#pred <- ggplot(ko.dataframe, aes(x ,y, z = var1.pred)) +
#  geom_contour_filled(bins = 20)
#var <- ggplot(ko.dataframe, aes(x ,y, z = var1.var)) +
#  geom_contour_filled(bins = 20)
#pred


# create a spline model that model the distance relationship of river elevation 

# filter out 5000 ft
RiverRegression_subset = RiverRegression %>%
  dplyr::filter(Distance <= 5000)

# set the values to numeric
RiverRegression_subset$median = as.numeric(RiverRegression_subset$median)

# create a spline model
summary(model1 <- glm(median ~ bs(Distance, df = 3), data = RiverRegression_subset, family = "gaussian"))

# using the spline model to make prediction
Predicted_RiverElevation = predict(model1, data.frame(Distance = RiverRegression_subset$Distance))

RiverRegression_subset$prediction = Predicted_RiverElevation
# plot the prediction and the data
ggplot(data = RiverRegression_subset) +
  geom_point(aes(x = Distance, y = median)) +
  geom_line(aes(x = Distance, y = prediction))


# convert spatial point data frame into simple feature in sf package
ko_sf = st_as_sf(ko)
D2River_Grid_sf = st_as_sf(D2River_Grid)


ko_sfc = st_as_sfc(ko)
D2River_Grid_sfc = st_as_sfc(D2River_Grid)


# create a buffer zone
ko_sfc_buf = st_buffer(ko_sfc, 2000)



# find the intersection of kriging data frame and river data frame 
# create a sparse geometry binary predicate, also called sparse matrix, which is a list with integer vectors only holding the indices for each polygon that intersects. 
# find x that s in y
koRiver_sgbp = st_intersects(x = ko_sfc_buf, y= D2River_Grid_sfc)
Riverko_sgbp = st_intersects(x = D2River_Grid_sfc, y = ko_sfc_buf)

class(koRiver_sgbp)

koRiver_logical = lengths(koRiver_sgbp) > 0
River_logical = lengths(Riverko_sgbp) > 0 

ko_river_intersect = ko_sf[koRiver_logical,]
river_ko_intersect = D2River_Grid_sf[River_logical,]

ko_river_intersect
river_ko_intersect 


sum(is.na(river_ko_intersect$SmoothWL))

#ko_river_intersect$river_pred = 0


#river_ko_intersect[is.na(river_ko_intersect$SmoothWL),]$SmoothWL = rep(mean(D2River_Grid_sf$SmoothWL),length(river_ko_intersect[is.na(river_ko_intersect$SmoothWL),]$SmoothWL))

ko_river_intersect$river_pred = (river_ko_intersect$SmoothWL +  predict(model1, data.frame(Distance = river_ko_intersect$Distance))) * 0.8 + ko_river_intersect$var1.predn * 0.2

sum(is.na(ko_river_intersect$river_pred))

diff = ko_river_intersect$river_pred - ko_river_intersect$var1.predn
hist(river_ko_intersect$SmoothWL)



ko_spatial = as_Spatial(ko_river_intersect)
ko.dataframe = as.data.frame(ko_spatial)
pred <- ggplot(ko.dataframe, aes(coords.x1 ,coords.x2, z = river_pred)) +
  geom_contour_filled(bins = 20)
pred
D2River_dataframe = as.data.frame(D2River_Grid)
pred <- ggplot(D2River_dataframe, aes(coords.x1 ,coords.x2, z = SmoothWL)) +
  geom_contour_filled(bins = 20)
pred

# normalize the column
normalized = function (x) {(x-min(x))/(max(x)-min(x))}    
ko_spatial$river_predn = normalized(ko_spatial@data$river_pred)
plot(ko_river_intersect)





# convert to a dataframe
ko.dataframe <- as.data.frame(ko_spatial)
#colnames(DEM.dataframe) <- c("DEM","DEMn", "x", "y")
# select 1 in every 1000 data for variogram modeling 
set.seed(123)
ko.dataframesubset <- subset(ko.dataframe[seq(1,nrow(ko.dataframe), 300),])

str(ko.dataframesubset)

# create a subset DEM spatialpointdataframe for variogram modeling 
ko.datasubset <- SpatialPointsDataFrame(SpatialPoints(ko.dataframesubset[,6:7]),data=data.frame(ko.dataframesubset[,4:5]))
ko.datasubset@bbox <- t(matrix(extent(data.grid),2))
crs(ko.datasubset) <- crs(DEM.data)
ko.datasubset


## Co-kriging with topography (DEM) 

plot(v.DEMn <- variogram(DEMn ~ 1, DEM.datasubset, cutoff=300000, width=20000), main = "Empirical Variogram w/ DEMn", pl = T)

# model by eye
m.DEMn <- vgm(.025, "Gau", 100000, .001)
# fit
(m.DEMn.f <- fit.variogram(v.DEMn, m.DEMn))

plot(v.DEMn, pl=T, model=m.DEMn.f, main = "Fitted Topography DEM Variogram")




(g <- gstat(NULL, id = "WL.RIVER", form = river_predn ~ 1, data=ko.datasubset))

(g <- gstat(g, id = "DEM", form = DEMn ~ 1, data=DEM.datasubset))

# Compute and display the two direct variogram and one cross-variogram

v.cross <- variogram(g)
str(v.cross)
plot(v.cross, pl=T)




(g <- gstat(g, id = "WL.RIVER", model = m.DEMn.f, fill.all=T))
# To ensure a positive definite co-krigingg system, use fit.lmc (fit linear model of co-regionalization)
# This takes the initial estimate, fits all the variograms, and then each of the partial sills is adjusted 
(g <- fit.lmc(v.cross, g, fit.method=1, correct.diagonal=1.01))
#g
#plot(variogram(g), model=g$model)

# export fig to png
ofile = paste("output/",method,"_fitted_variogram_",cal_year,"_cutoff_", toString(thre_cutoff),".png", sep="")
png(ofile,width=10,height=8,units="in",res=300)
vplot <- plot(variogram(g), pl=T, model=g$model, main = paste("Fitted Topography Streams model", 
                                                       cal_year,"_cutoff_",toString(thre_cutoff)))
print(vplot, width = 6, height = 4, dpi = 300)
dev.off()





k.cc <- predict(g, data.grid)
k.cc@data$WL.RIVER.pred <- ifelse(k.cc@data$WL.RIVER.pred >= 1.0, 1, k.cc@data$WL.RIVER.pred)
k.cc@data$WL.RIVER.pred <- ifelse(k.cc@data$WL.RIVER.pred <= 0.0, 0, k.cc@data$WL.RIVER.pred)

k.cc@data$WL_pred = InvNormalize(k.cc@data$WL.RIVER.pred)
k.cc@data$WL_var <- k.cc@data$WL_pred*k.cc@data$WL.RIVER.var
#k.cc


#sp::spplot(k.cc, "WL.RIVER.pred", pch=19, col = "transparent",
#     scales=list(draw=TRUE), main="Co-Kirging Estimates with DEM", xlab = "x", ylab = "y", widths = 10)

out_df = as.data.frame(k.cc)

# Export interpolated water levels
ofile = paste("output/", method, "_pred_wl_",cal_year,"_cutoff_",toString(thre_cutoff),".png",sep="")
#png(ofile,width=10,height=8,units="in",res=300)
ggplot(out_df, aes(x ,y, z = WL_pred)) +
  geom_contour_filled(bins = 20)
ggsave(ofile, width = 8, height = 6, dpi = 300)
#dev.off()

# Export Kriging variance
ofile = paste("output/", method, "_kriging_var_",cal_year,"_cutoff_",toString(thre_cutoff),".png",sep="")
#png(ofile,width=10,height=8,units="in",res=300)
ggplot(out_df, aes(x ,y, z = WL_var)) +
  geom_contour_filled(bins = 20)
ggsave(ofile, width = 8, height = 6, dpi = 300,)
#dev.off()

# Save variogram to a csv file
ofile = paste("output/", method, "_fitted_variogram_",cal_year,"_cutoff_",toString(thre_cutoff),".csv",sep="")
write.csv(v.cross,ofile)

# Write all outputs to csv
ofile = paste("output/", method ,"_pred_wl_",cal_year,"_cutoff_",toString(thre_cutoff),".csv",sep="")
write.csv(out_df,ofile)