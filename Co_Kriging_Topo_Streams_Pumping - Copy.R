# Modelling the spatial structure of the target variable - make variogram
# 1. the variogram method computes the experimental variogram as a variogram object
# 2. the vgm method creates a variogram model object
# 3. the fit.variogram method adjusts a variogram model object to a variogram object

# look at the spatial point data frame
data_spadf$WL2020n

# plot the empirical variogram for measured water levels at specific years
plot(v.WLn <- variogram(WL2019n ~ 1 , data=data_spadf, cutoff=300000, width=10000), main = "Empirical Variogram",pl = T)
# create a model for the empirical variogram
m.WLn <- vgm(0.05,"Gau",200000,0.003)
# plot both the fitted variogram model with the empirical variogram
plot(v.WLn, pl=T, model=m.WLn, main = "Variogram Model")
(m.WLn.f <- fit.variogram(v.WLn, m.WLn))

plot(v.WLn, pl=T, model=m.WLn.f, main = "Fitted OK Variogram Model")


## Ordinary Kriging Interpolation of the Target Variable

# use krige function for oridinary kriging 
ko <- krige(WL2019n~1, locations=data_spadf, newdata=data.grid, model=m.WLn.f)
# extract the kriging estimates and kriging variances 
ko_pred <- data.frame(ko$var1.pred)
ko_var <- data.frame(ko$var1.var)

# find the min and max of the original scale measured water levels
min <- min(data_spadf@data$WL2019)
max <- max(data_spadf@data$WL2019)
# back transform to original scales
InvNormalize <- function (x) {x * (max - min) + min}

ko$var1.predn = InvNormalize(ko$var1.pred)

# use spplot to plot the kriging results 
sp::spplot(ko, "var1.pred", pch=19, col = "transparent",
     scales=list(draw=TRUE), main="Ordinary Kirging Estimates")
sp::spplot(ko, "var1.var", pch=19, col = "transparent",
     scales=list(draw=TRUE), main="Ordinary Kirging Estimates")



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



# add the river elevation on the the prdinary kriging estimates of measured water levels

# convert spatial point data frame into simple feature in sf package
ko_sf = st_as_sf(ko)
D2River_Grid_sf = st_as_sf(D2River_Grid)

ko_sfc = st_as_sfc(ko)
D2River_Grid_sfc = st_as_sfc(D2River_Grid)

# create a buffer zone
ko_sfc_buf = st_buffer(ko_sfc, 1000)

# find the intersection of kriging data frame and river data frame 
# create a sparse geometry binary predicate, also called sparse matrix, which is a list with integer vectors only holding the indices for each polygon that intersects. 
# find x that s in y

### old
###koRiver_sgbp = st_intersects(x = ko_sfc_buf, y= D2River_Grid_sfc)
###Riverko_sgbp = st_intersects(x = D2River_Grid_sfc, y = ko_sfc_buf)
# find the logical vector 
###koRiver_logical = lengths(koRiver_sgbp) > 0
###River_logical   = lengths(Riverko_sgbp) > 0 

# find the intersection
###ko_river_intersect = ko_sf[koRiver_logical,]
###river_ko_intersect = D2River_Grid_sf[River_logical,]

# New 09/09/2020
koRiver_sgbp = st_intersects(x = ko_sfc_buf, y= D2River_Grid_sfc)
Riverko_sgbp = st_intersects(x = D2River_Grid_sfc, y = ko_sfc_buf)

class(koRiver_sgbp)

koRiver_logical = lengths(koRiver_sgbp) > 0
River_logical   = lengths(Riverko_sgbp) > 0 

ko_river_intersect = ko_sf[koRiver_logical,]
river_ko_intersect = D2River_Grid_sf[River_logical,]




sum(is.na(river_ko_intersect$SmoothWL))

### old
### calculate the river pred from distance and smooth water level from 
###ko_river_intersect$WLpumping_river = (river_ko_intersect$SmoothWL +  predict(model1, data.frame(Distance = river_ko_intersect$Distance))) * 0.6 + ko_river_intersect$var1.predn * 0.4

### new
# create a linear ratio of distance to assign approporiate weights
LinearDistance_ratio = 1 - river_ko_intersect$Distance/ max(river_ko_intersect$Distance)
# calculate the river pred from distance and smooth water level from 
ko_river_intersect$WLpumping_river = (river_ko_intersect$SmoothWL +  predict(model1, data.frame(Distance = river_ko_intersect$Distance))) * (LinearDistance_ratio * 0.6) + ko_river_intersect$var1.predn * 0.4


sum(is.na(ko_river_intersect$river_pred))

diff = ko_river_intersect$river_pred - ko_river_intersect$var1.predn
hist(river_ko_intersect$SmoothWL)

ko_spatial = as_Spatial(ko_river_intersect)


# normalize the column
normalized = function (x) {(x-min(x))/(max(x)-min(x))}    
ko_spatial$WLpumping_rivern = normalized(ko_spatial@data$WLpumping_river )





# preprocessing the pumping information and hard correct the measured water levels

# convert to sf
WL_sf = st_as_sf(data_spadf)
pumping_sf = st_as_sf(pumping_drawdown2019)

# convert to sfc
WL_sfc = st_as_sfc(data_spadf)
pumping_sfc = st_as_sfc(pumping_drawdown2019)

# create a buffer zone
WL_sfc_buf = st_buffer(WL_sfc, 750)

# create a sgbp (sparse geometry binary predicate)
pumpingWL_sgbp = st_intersects(x= pumping_sfc, y = WL_sfc_buf)
WLpumping_sgbp = st_intersects(x= WL_sfc_buf, y = pumping_sfc)
WLpumping_sgbp[1]
# create a logical vector that indicate whether the points are in the buffer zone 
WLpumping_logical = lengths(WLpumping_sgbp) > 0
pumpingWL_logical = lengths(pumpingWL_sgbp) > 0
# create subsets of the data 
WLpumping_intersect = WL_sf[WLpumping_logical,]
pumpingWL_intersect = pumping_sf[pumpingWL_logical,]
WLpumping_intersect
pumpingWL_intersect

# correct the measured water level data 
WLpumping_intersect$WLpumping_river =  aggregate(x= pumpingWL_intersect, by=WL_sfc_buf, FUN = mean)$Drawdown + WLpumping_intersect$WL2019

# convert sf objects to spatialpointdataframe
WLpumping_spatial = as_Spatial(WLpumping_intersect)

# normalize the column
normalized = function (x) {(x-min(x))/(max(x)-min(x))}    
WLpumping_spatial$WLpumping_rivern= normalized(WLpumping_spatial$WLpumping_river)



# combine two simple features together

# only extract revelant columns from the two simple features: (1) the WLpumping and (2) the ko_river
WLpumping_only_sf = st_set_geometry(data.frame(WLpumping_river = WLpumping_intersect$WLpumping_river), WL_sfc) 

ko_river_only_sf = st_set_geometry(data.frame(WLpumping_river = ko_river_intersect$WLpumping_river), st_geometry(ko_river_intersect)) 

# combine the two simple features
WLpumping_koriver_combined_sf = rbind(WLpumping_only_sf, ko_river_only_sf)

# convert it to a spatialpointdataframe
WLpumpingriver_combined_spadf = as_Spatial(WLpumping_koriver_combined_sf)





# convert to a dataframe
WLpumpingriver_combined.dataframe <- as.data.frame(WLpumpingriver_combined_spadf)
#colnames(DEM.dataframe) <- c("DEM","DEMn", "x", "y")
# select 1 in every 1000 data for variogram modeling 
set.seed(123)
WLpumpingriver_combined.dataframesubset <- subset(WLpumpingriver_combined.dataframe[seq(1,nrow(WLpumpingriver_combined.dataframe), 50),])

# create a subset DEM spatialpointdataframe for variogram modeling 
WLpumpingriver_combined.datasubset <- SpatialPointsDataFrame(SpatialPoints(WLpumpingriver_combined.dataframesubset[,2:3]),data=data.frame(WLpumpingriver_combined.dataframesubset[,1]))
WLpumpingriver_combined.datasubset@bbox <- t(matrix(extent(data.grid),2))
crs(WLpumpingriver_combined.datasubset) <- crs(DEM.data)

# change the column name 
colnames(WLpumpingriver_combined.datasubset@data) = "WLpumpingriver"
# normalize the column values
WLpumpingriver_combined.datasubset$WLpumpingrivern = normalized(WLpumpingriver_combined.datasubset$WLpumpingriver)



## Co-kriging with topography (DEM) 

plot(v.DEMn <- variogram(DEMn ~ 1, DEM.datasubset, cutoff=300000, width=15000), main = "Empirical Variogram w/ DEMn", pl = T)

# model by eye
m.DEMn <- vgm(.025, "Gau", 250000, .001)
# fit
(m.DEMn.f <- fit.variogram(v.DEMn, m.DEMn))

plot(v.DEMn, pl=T, model=m.DEMn.f, main = "Fitted Topography DEM Variogram")

# construct he cross-variogram model


(g <- gstat(NULL, id = "WL.PUMPINGRIVER", form = WLpumpingrivern ~ 1, data=WLpumpingriver_combined.datasubset))

(g <- gstat(g, id = "DEM", form = DEMn ~ 1, data=DEM.datasubset))

# Compute and display the two direct variogram and one cross-variogram

v.cross <- variogram(g)
str(v.cross)
plot(v.cross, pl=T)


# model the cross-variogram structure


(g <- gstat(g, id = "WL.PUMPINGRIVER", model = m.DEMn.f, fill.all=T))
# To ensure a positive definite co-krigingg system, use fit.lmc (fit linear model of co-regionalization)
# This takes the initial estimate, fits all the variograms, and then each of the partial sills is adjusted 
(g <- fit.lmc(v.cross, g, fit.method=1, correct.diagonal=1.01))
g
plot(variogram(g), model=g$model)




# make the co-kriging prediction

k.cc <- predict(g, data.grid)
k.cc@data$WL.PUMPINGRIVER <- ifelse(k.cc@data$WL.PUMPINGRIVER.pred >= 1.0, 1, k.cc@data$WL.PUMPINGRIVER.pred)
k.cc@data$WL.PUMPINGRIVER <- ifelse(k.cc@data$WL.PUMPINGRIVER.pred <= 0.0, 0, k.cc@data$WL.PUMPINGRIVER.pred)
k.cc@data$WL.PUMPINGRIVER.predn = InvNormalize(k.cc@data$WL.PUMPINGRIVER.pred)
k.cc


sp::spplot(k.cc, "WL2020n.pred", pch=19, col = "transparent",
     scales=list(draw=TRUE), main="Co-Kirging Estimates with DEM", xlab = "x", ylab = "y", widths = 10)
