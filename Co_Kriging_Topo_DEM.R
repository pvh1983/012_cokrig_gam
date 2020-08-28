## Co-kriging with topography (DEM) 

#v.DEMn <- variogram(DEMn ~ 1, DEM.datasubset, cutoff=300000, width=10000)
v.DEMn <- variogram(DEMn ~ 1 , data=DEM.datasubset, cutoff=thre_cutoff, width=thre_width)
plot(v.DEMn, main = "Empirical Variogram w/ DEMn", pl = T)

# model by eye
m.DEMn <- vgm(.03, "Gau", 250000, .001)
# fit
(m.DEMn.f <- fit.variogram(v.DEMn, m.DEMn))

plot(v.DEMn, pl=T, model=m.DEMn.f, main = "Fitted Topography DEM Variogram")



# create the cross-variogram for measured water level and topography
(g <- gstat(NULL, id = "WL", form = WL2020n ~ 1, data=data_sp2020))

(g <- gstat(g, id = "TOPO", form = DEMn ~ 1, data=DEM.datasubset))

# Compute and display the two direct variogram and one cross-variogram
v.cross <- variogram(g)
str(v.cross)
plot(v.cross, pl=T)



(g <- gstat(g, id = "WL_DEM", model = m.DEMn.f, fill.all=T))
# To ensure a positive definite co-krigingg system, use fit.lmc (fit linear model of co-regionalization)
# This takes the initial estimate, fits all the variograms, and then each of the partial sills is adjusted 
(g <- fit.lmc(v.cross, g, fit.method=1, correct.diagonal=1.01))
#g
#plot(variogram(g), model=g$model)

# export fig to png
ofile = paste("output/",method,"_fitted_variogram_",cal_year,"_cutoff_", toString(thre_cutoff),".png", sep="")
png(ofile,width=10,height=8,units="in",res=300)
vplot <- plot(variogram(g), pl=T, model=g$model, main = paste("Fitted Topography DEM Variogram", 
                                                                               cal_year,"_cutoff_",toString(thre_cutoff)))
print(vplot, width = 6, height = 4, dpi = 300)
dev.off()




k.cc <- predict(g, data.grid)
k.cc@data$WL.pred <- ifelse(k.cc@data$WL.pred >= 1.0, 1, k.cc@data$WL.pred)
k.cc@data$WL.pred <- ifelse(k.cc@data$WL.pred <= 0.0, 0, k.cc@data$WL.pred)


min <- min(data_sp2020@data$WL2020)
max <- max(data_sp2020@data$WL2020)
# back transform to original scales
InvNormalize <- function (x) {x * (max - min) + min}

k.cc@data$WL_pred = InvNormalize(k.cc@data$WL.pred)
k.cc@data$WL_var <- k.cc@data$WL_pred*k.cc@data$WL.var


#k.cc
#k.cc_pred <- data.frame(data = k.cc$WLn.pred, k.cc@coords)
#k.cc_pred

#sp::spplot(k.cc, "WL_pred", pch=19, col = "transparent",
#     scales=list(draw=TRUE), main="Co-Kirging Estimates with DEM", xlab = "x", ylab = "y", widths = 10)

#k.cc@data$var1.pred <- ifelse(k.cc@data$var1.pred >= 1.0, 1, k.cc@data$var1.pred)
#k.cc@data$var1.pred <- ifelse(k.cc@data$var1.pred <= 0.0, 0, k.cc@data$var1.pred)

#summary(k.cc)

#k.cc@data$var1.predn <- InvNormalize(k.cc@data$var1.pred)
#k.cc@data$var1.varn <- k.cc@data$var1.predn*k.cc@data$var1.var
#summary(k.cc)

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
