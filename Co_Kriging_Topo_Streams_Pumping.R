# Modelling the spatial structure of the target variable - make variogram
# 1. the variogram method computes the experimental variogram as a variogram object
# 2. the vgm method creates a variogram model object
# 3. the fit.variogram method adjusts a variogram model object to a variogram object

# plot the empirical variogram for measured water levels at specific years
v.WLn <- variogram(WL2020n ~ 1 , data=data_sp2020, cutoff=thre_cutoff, width=thre_width)
plot(v.WLn, main = "Empirical Variogram",pl = T)

# create a model for the empirical variogram
m.WLn <- vgm(0.05,"Gau",200000,0.003)

# plot both the fitted variogram model with the empirical variogram
plot(v.WLn, pl=T, model=m.WLn, main = "Variogram Model")
(m.WLn.f <- fit.variogram(v.WLn, m.WLn))

#plot(v.WLn, pl=T, model=m.WLn.f, main = "Fitted OK Variogram Model")


## Ordinary Kriging Interpolation of the Target Variable

# use krige function for oridinary kriging 
ko <- krige(WL2020n~1, locations=data_sp2020, newdata=data.grid, model=m.WLn.f)

# extract the kriging estimates and kriging variances 
ko_pred <- data.frame(ko$var1.pred)
ko_var <- data.frame(ko$var1.var)

# find the min and max of the original scale measured water levels for each year
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



# create a spline model that model the distance relationship of river elevation 

# filter out distance longer than 5000 ft
RiverRegression_subset = RiverRegression %>%
  dplyr::filter(Distance <= 1000)
str(RiverRegression_subset)

# set the values to numeric
RiverRegression_subset$median = as.numeric(RiverRegression_subset$median)


# create a spline model
model1 <- glm(median ~ bs(Distance, df = 4), data = RiverRegression_subset, family = "gaussian")
summary(model1)
str(model1)

# using the spline model to make prediction
dist_to_river_cell = data.frame(Distance = RiverRegression_subset$Distance) # new df
head_diff = predict(model1, dist_to_river_cell) # pred using dist_to_river_cell

RiverRegression_subset$prediction <- head_diff

# plot the prediction and the data
ggplot(data = RiverRegression_subset) +
  geom_point(aes(x = Distance, y = median)) +
  geom_line(aes(x = Distance, y = prediction)) +
  labs(title = "The Median Value of River WL and GW Level Difference Away From River Streams", x = "Distance From River Streams (ft)", y = "Median Value of WL diff (ft)")


# add the river elevation on the the prdinary kriging estimates of measured water levels

# convert spatial point data frame into simple feature in sf package
ko_sf = st_as_sf(ko) # ko has var1.pred, var1.var, var1.predn
str(ko)
str(ko_sf)

# st_as_sf Spatial objects can be converted back to sf in the same way or with st_as_sf():
D2River_Grid_sf = st_as_sf(D2River_Grid) # st_as_sf Convert Foreign Object To An Sf Object
#str(D2River_Grid)     # D2River_Grid is a SpatialPointsDataFrame
str(D2River_Grid_sf)  # Classes 'sf' and 'data.frame' 179554 obs: GAMx, GAMy, SmoothWL, DEM_500, Distance
#class(D2River_Grid)
#class(D2River_Grid_sf)

ko_sfc = st_as_sfc(ko) # Convert foreign geometry object to an sfc object
                       # sfc: A simple feature geometry list-column       
str(ko_sfc)  # sfc_POINT of length 815253; 
             # first list element:  'XY' num [1:2] 5749041 19251410
# ko_sfc[1]  # POINT (5749041 19251410)
# summary(ko_sfc) # POINT       epsg:NA +proj=aea ... 815253
# class(ko_sfc) # "sfc_POINT" "sfc"
# plot(st_geometry(ko_sfc), add=T, col="green")

D2River_Grid_sfc = st_as_sfc(D2River_Grid) # hp: Geometry set of 179554 river cells
str(D2River_Grid_sfc) # sfc_POINT of length 179554 [point payer] [Geometry set]
#plot(st_geometry(D2River_Grid_sfc[1:3])) # Show three points
plot(st_geometry(D2River_Grid_sfc[1:30]), axes = TRUE) # Show three points

# create a buffer zone, 1000 ft
ko_sfc_buf = st_buffer(ko_sfc, 1000) # Computes a buffer (circle) at a location
str(ko_sfc_buf) # sfc_POLYGON of length 815253 (create 815253 circles, r=1000 ft)
# ko_sfc_buf[1] # POLYGON xmin: 5748041 ymin: 19250410 xmax: 5750041 ymax: 19252410
#plot(as_Spatial(ko_sfc_buf[1:30]))
plot(st_geometry(ko_sfc_buf[1:30]), axes = TRUE) # 3 circle

# find the intersection of kriging data frame and river data frame 
# create a sparse geometry binary predicate, also called sparse matrix, 
# which is a list with integer vectors only holding the indices for 
# each polygon that intersects. 

# find x that is in y
koRiver_sgbp = st_intersects(x = ko_sfc_buf, y= D2River_Grid_sfc)
Riverko_sgbp = st_intersects(x = D2River_Grid_sfc, y = ko_sfc_buf)
# River_logical: Sparse geometry binary predicate list of length 179554
#str(Riverko_sgbp)   # List of 179554
#class(Riverko_sgbp) # "sgbp" "list"
#summary(Riverko_sgbp) 


# find the logical vector 
koRiver_logical = lengths(koRiver_sgbp) > 0
River_logical   = lengths(Riverko_sgbp) > 0 # cells that intersect river cells

#summary(River_logical) # TRUE 179554
#class(River_logical)  # "logical"
#str(River_logical)    # logi [1:179554] TRUE TRUE TRUE TRUE TRUE TRUE

# find the intersection
ko_river_intersect = ko_sf[koRiver_logical,]         # ko pred values
#str(ko_river_intersect)

river_ko_intersect = D2River_Grid_sf[River_logical,] # River values
#river_ko_intersect # Simple feature collection with 179554 points
#str(river_ko_intersect) # Classes 'sf' and 'data.frame':	179554 obs
tmp_sf = as_Spatial(river_ko_intersect)

#sp::spplot(tmp_sf[1:5], "SmoothWL", pch=19, col = "transparent",
#           scales=list(draw=TRUE), main="SmoothWL")



#sum(is.na(river_ko_intersect$SmoothWL))

# create a linear ratio of distance to assign approporiate weights
LinearDistance_ratio = 1 - river_ko_intersect$Distance/ max(river_ko_intersect$Distance)

# river_ko_intersect$Distance: [1.596, 5999.976]
# max(river_ko_intersect$Distance) = 5999.976 ft
# river_ko_intersect$Distance

#min(LinearDistance_ratio) #= 0
#max(LinearDistance_ratio) #= 0.999734


# calculate the river pred from distance and smooth water level from 
df2 <- data.frame(Distance = river_ko_intersect$Distance)
#min(df2) # 1.596
#max(df2) # 5999.98

# medium value of river elevations
riv_pred <- predict(model1, df2) # func of distance to the nearest river cell
#str(riv_pred)
#min(riv_pred)  # = 3.368 
#max(riv_pred)  # = 24.789

Surface_WL <- river_ko_intersect$SmoothWL # from Jevon's shp file above
#str(Surface_WL)
#class(Surface_WL) # "numeric"
#summary(Surface_WL)
#size(Surface_WL) # 179554

OK_pred_WL <- ko_river_intersect$var1.predn
#str(ko_river_intersect$var1.predn)
#str(OK_pred_WL)

cof <- 0.8*LinearDistance_ratio
cof <- 1
ko_river_intersect$WLpumping_river = (Surface_WL + riv_pred) * cof + OK_pred_WL * (1-cof)

cof2 <- 0.00001*LinearDistance_ratio
ko_river_intersect$WLpumping_river2 = (Surface_WL + riv_pred) * cof2 + OK_pred_WL * (1-cof2)


ko_spatial2 = as_Spatial(ko_river_intersect)     # hp
df_OK2 = as.data.frame(ko_spatial2)
pred2 <- ggplot(df_OK2, aes(coords.x1 ,coords.x2, z = WLpumping_river)) +
  geom_contour_filled(bins = 20)
#pred2

# Save to a csv file
ofile = paste("output/", "test2.csv",sep="")
write.csv(df_OK2,ofile)





# river_ko_intersect$SmoothWL: SmoothWL is from the shp file GridCells_500_within_6000_ft_Smooth_Perennial_Streams_wNearestSmoothStreamPt.shp
#min(Surface_WL) #= 141.343
#max(Surface_WL) #= 493.805

#min(OK_pred_WL) #= 169.09 ft
#max(OK_pred_WL) #= 466.53

#loc <- 100
#Surface_WL[loc] # 360.041

#riv_pred[loc]   # 4.191
#LinearDistance_ratio[loc] # 0.793

#tmp <- (Surface_WL[loc] + riv_pred[loc]) # 364.233
#tmp*LinearDistance_ratio[loc]*0.2 # 57.809

#OK_pred_WL[loc]      # = 451.80 ft
#OK_pred_WL[loc]*0.8  # = 361.44 ft

#RHS1 = (Surface_WL[loc] + riv_pred[loc]) * LinearDistance_ratio[loc] * 0.2 # 57.81 ft
#RHS2 = OK_pred_WL[loc] * 0.8 # 361.44 ft

#TOTAL = RHS1 + RHS2 # = 419.253, after accounting for river

# compared with OK_pred_WL[loc] = 451.8 ft.

#sum(is.na(ko_river_intersect$river_pred))

#diff = ko_river_intersect$river_pred - ko_river_intersect$var1.predn
#hist(Surface_WL)


# refs
# https://bookdown.org/robinlovelace/geocompr/spatial-class.html

