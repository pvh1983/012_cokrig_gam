# Modelling the spatial structure of the target variable - make variogram
# 1. the variogram method computes the experimental variogram as a variogram object
# 2. the vgm method creates a variogram model object
# 3. the fit.variogram method adjusts a variogram model object to a variogram object

# [1] look at the spatial point data frame
data_spadf

# plot the empirical variogram for measured water levels at specific years
plot(v.WLn <- variogram(WL2020n ~ 1 , data=data_spadf, cutoff=300000, width=10000), main = "Empirical Variogram",pl = T)
# create a model for the empirical variogram
m.WLn <- vgm(0.05,"Gau",200000,0.003)
# plot both the fitted variogram model with the empirical variogram
plot(v.WLn, pl=T, model=m.WLn, main = "Variogram Model")
(m.WLn.f <- fit.variogram(v.WLn, m.WLn))

plot(v.WLn, pl=T, model=m.WLn.f, main = "Fitted OK Variogram Model")


## [2] Ordinary Kriging Interpolation of the Target Variable

# use krige function for oridinary kriging 
ko <- krige(WL2020n~1, locations=data_spadf, newdata=data.grid, model=m.WLn.f)
# extract the kriging estimates and kriging variances 
ko_pred <- data.frame(ko$var1.pred)
ko_var <- data.frame(ko$var1.var)

# find the min and max of the original scale measured water levels
min <- min(data_spadf@data$WL2020)
max <- max(data_spadf@data$WL2020)
# back transform to original scales
InvNormalize <- function (x) {x * (max - min) + min}

ko$var1.predn = InvNormalize(ko$var1.pred)

# use spplot to plot the kriging results 
sp::spplot(ko, "var1.pred", pch=19, col = "transparent",
     scales=list(draw=TRUE), main="Ordinary Kirging Estimates")
sp::spplot(ko, "var1.var", pch=19, col = "transparent",
     scales=list(draw=TRUE), main="Ordinary Kirging Estimates")



# [3] plot contours

ko.dataframe = as.data.frame(ko)
pred <- ggplot(ko.dataframe, aes(x ,y, z = var1.pred)) +
  geom_contour_filled(bins = 20)
var <- ggplot(ko.dataframe, aes(x ,y, z = var1.var)) +
  geom_contour_filled(bins = 20)
pred




# [4] preprocessing the pumping information and hard correct the measured water levels

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

WLpumping_intersect$WLpumping =  aggregate(x= pumpingWL_intersect, by=WL_sfc_buf, FUN = mean)$Drawdown + WLpumping_intersect$WL2019

WLpumping_spatial = as_Spatial(WLpumping_intersect)
sum(is.na(WLpumping_spatial$WLpumping))
# normalize the column
normalized = function (x) {(x-min(x))/(max(x)-min(x))}    
WLpumping_spatial$WLpumpingn= normalized(WLpumping_spatial$WLpumping)








# [5] plot the empirical variogram for measured water levels at specific years
plot(v.WLn <- variogram(WLpumpingn ~ 1 , data=WLpumping_spatial, cutoff=300000, width=10000), main = "Empirical Variogram",pl = T)
# create a model for the empirical variogram
m.WLn <- vgm(0.05,"Gau",200000,0.003)
# plot both the fitted variogram model with the empirical variogram
plot(v.WLn, pl=T, model=m.WLn, main = "Variogram Model")
(m.WLn.f <- fit.variogram(v.WLn, m.WLn))

plot(v.WLn, pl=T, model=m.WLn.f, main = "Fitted OK Variogram Model")




# [6] Co-kriging with topography (DEM) 


plot(v.DEMn <- variogram(DEMn ~ 1, DEM.datasubset, cutoff=300000, width=10000), main = "Empirical Variogram w/ DEMn", pl = T)

# model by eye
m.DEMn <- vgm(.025, "Gau", 250000, .001)
# fit
(m.DEMn.f <- fit.variogram(v.DEMn, m.DEMn))

plot(v.DEMn, pl=T, model=m.DEMn.f, main = "Fitted Topography DEM Variogram")




# [7] 
(g <- gstat(NULL, id = "WL.PUMPING", form = WLpumpingn ~ 1, data=WLpumping_spatial))

(g <- gstat(g, id = "DEM", form = DEMn ~ 1, data=DEM.datasubset))

# Compute and display the two direct variogram and one cross-variogram
v.cross <- variogram(g)
str(v.cross)
plot(v.cross, pl=T)




# [8] ---
(g <- gstat(g, id = "WL.PUMPING", model = m.DEMn.f, fill.all=T))
# To ensure a positive definite co-krigingg system, use fit.lmc (fit linear model of co-regionalization)
# This takes the initial estimate, fits all the variograms, and then each of the partial sills is adjusted 
(g <- fit.lmc(v.cross, g, fit.method=1, correct.diagonal=1.01))
g
plot(variogram(g), model=g$model)




# [9] ---
k.cc <- predict(g, data.grid)
k.cc@data$WL2020.pred <- ifelse(k.cc@data$WL.PUMPING.pred >= 1.0, 1, k.cc@data$WL.PUMPING.pred)
k.cc@data$WL2020.pred <- ifelse(k.cc@data$WL.PUMPING.pred <= 0.0, 0, k.cc@data$WL.PUMPING.pred)
k.cc@data$WL.PUMPING.predn = InvNormalize(k.cc@data$WL.PUMPING.pred)
k.cc
ko_pred <- data.frame(data = k.cc$WL.PUMPING.pred, k.cc@coords)
ko_pred

sp::spplot(k.cc, "WL.PUMPING.pred", pch=19, col = "transparent",
     scales=list(draw=TRUE), main="Co-Kirging Estimates with DEM", xlab = "x", ylab = "y", widths = 10)

