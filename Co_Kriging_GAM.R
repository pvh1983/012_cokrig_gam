
# preprocessing the GAM (simulated water levels), and extract a subset spatialpointdataframe to variogram and co-krige with measured water levels

# convert the selected GAM raster for either 2019 or 2020 to a spatial point data frame, specify the year of the data
sim_wl.data <- rasterToPoints(sim_WL2020, spatial = TRUE, progress = "window")
#str(sim_wl.data)

# rename the colname
colnames(sim_wl.data@data)[1] = "sim_wl"
# normalized the data
sim_wl.data@data$sim_wln <- normalized(sim_wl.data@data$sim_wl)
sim_wl.data
#str(sim_wl.data)
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
#plot(v_WL2020n, main = "Empirical Variogram",pl = T)

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
ofile = paste("output/fitted_variogram_",cal_year,"_cutoff_",toString(thre_cutoff),"_GAM_res_",GAM_res,".png", sep="")
png(ofile,width=10,height=8,units="in",res=300)
vplot <- plot(v_WL2020n, pl=T, model=variogram_model_WL2020n_fit, main = paste("Fitted OK Variogram Model ", cal_year,"_cutoff_",toString(thre_cutoff)))
print(vplot, width = 6, height = 4, dpi = 300)
dev.off()

# Save to a csv file
ofile = paste("output/fitted_variogram_",cal_year,"_cutoff_",toString(thre_cutoff),"_GAM_res_",GAM_res,".csv", sep="")
write.csv(v_WL2020n,ofile)




# Co-Kriging with GAM (simulated water levels)
# Model the spatial structure of a covariate and its covariance with the target variable. This is called co-regionalisation.


# plot the empirical variogram for simulated water levels, change the dataset to model the varigram for a different year
v.sim_wln <- variogram(sim_wln ~ 1, sim_wl2020.datasubset, cutoff=thre_cutoff, width=thre_width)
plot(v.sim_wln, main = "Empirical Variogram",pl = T)

# change width from 20,000 to 1,000 to change bin size
#plot(v.sim_wln <- variogram(sim_wln ~ 1, sim_wl2020.datasubset, cutoff=300000, width=1000), main = "Empirical Variogram",pl = T)

#plot(v.sim_wln, pl = T)

# fit the variogram model
m.sim_wln <- vgm(.04, "Gau",100000,0.001)
# examine the fitted variogram model
m.sim_wln.f <- fit.variogram(v.sim_wln, m.sim_wln)
#plot(v.sim_wln, pl=T, model=m.sim_wln.f, main = "Variogram MOdeling on GAM")

#head(m.sim_wln)




# Building a data structure to model co-regionalisation
# We have to fit models to both the direct and cross-variograms simultaneously, 
# and these models must lead to a positive definite cokriging system.
# Fit a linear model of co-regionalisation: all models (direct and cross) have 
# the same shape and range, but may have different partial sills and nuggest.

# create a model of co-regionalisation, be sure to check the years for both datasets
# mea wl data_sp2020
g <- gstat(NULL, id = "WL", form = WL2020n ~ 1, data=data_sp2020)
#summary(g)
#str(g)
#print(data_spadf)
#print(sim_wl2020.datasubset)
#print(data_sp2020)

# 
g <- gstat(g,  id = "SIM_WL", form = sim_wln ~ 1, data=sim_wl2020.datasubset)

# Compute and display the two direct variogram and one cross-variogram
v.cross <- variogram(g)
#str(v.cross)
#plot(v.cross, pl=T, main = "Cross-Variogram on WL and GAM")
#write.csv(v.cross,'v.cross.csv')


# Fitting a linear model of co-regionalisation

g <- gstat(g, id = "WL", model = m.sim_wln.f, fill.all=T)

# To ensure a positive definite co-krigingg system, use fit.lmc (fit linear model of co-regionalization)
# This takes the initial estimate, fits all the variograms, and then each of the partial sills is adjusted
g <- fit.lmc(v.cross, g, fit.method=1, correct.diagonal=1.01)
#print(g)
# examine the model fit for all variogram models
#plot(variogram(g), model=g$model, main = "Cross Variogram Models")


# export fig to png
ofile = paste("output/crossvariogram_",cal_year,"_cutoff_",toString(thre_cutoff),"_GAM_res_",GAM_res,".png", sep="")
png(ofile,width=10,height=8,units="in",res=300)
vplot2 <- plot(variogram(g), pl=T, model=g$model, main = paste("Cross-Variograms on WL and GAM", cal_year,"GAM_res = ", toString(GAM_res)))

print(vplot2, width = 8, height = 8, dpi = 600)
dev.off()



# Save to a csv file
ofile = paste("output/cross_variogram_",cal_year,"_cutoff_",toString(thre_cutoff),"_GAM_res_",GAM_res,".csv", sep="")
write.csv(v.cross, ofile)


# Perform co-kriging with simulated water levels

# use the predict function from gstat to interpolate the co-kriging estimates
coK_sim <- predict(g, data.grid)
#coK_sim

# constrain the predicted values strictly between 0 and 1
coK_sim@data$SIM_WL.pred <- ifelse(coK_sim@data$SIM_WL.pred >= 1.0, 1, coK_sim@data$SIM_WL.pred)
coK_sim@data$SIM_WL.pred <- ifelse(coK_sim@data$SIM_WL.pred <= 0.0, 0, coK_sim@data$SIM_WL.pred)

# find the min and max of the simulated water levels in the original scale
min <- min(sim_wl2020.datasubset@data$sim_wl)
max <- max(sim_wl2020.datasubset@data$sim_wl)

# back transform to original scales
InvNormalize <- function (x) {x * (max - min) + min}
coK_sim@data$SIM_WLn.pred = InvNormalize(coK_sim@data$WL.pred)
coK_sim@data$SIM_WLn.var = InvNormalize(coK_sim@data$WL.var)

sp::spplot(coK_sim, "SIM_WLn.pred", pch=19, col = "transparent",
     scales=list(draw=TRUE), main="Co-Kriging Estimates of WL with simulated WL2019", xlab = "x", ylab = "y", widths = 10)
