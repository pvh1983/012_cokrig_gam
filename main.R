


# [0] Specify input parameters and choose a Kriging technique
#cal_year <- "WL2019_3yr"
cal_year <- "WL2020.minus.Shallow.Below.1929.500"
#cal_year <- "shallow_below_2019_3yr"
#cal_year <- "shallow_below_2020_3yr"

thre_cutoff <- as.integer(300000)
thre_width <- as.integer(10000)

GAM_res <- as.integer(500) # 500 ft or 32000 ft


# [1] Read in librareis and input data
setwd("c:/Users/hpham/OneDrive - INTERA Inc/projects/012_rscript_geostats_s/scripts/")
source("get_input.R")

# [2] Choose a scenario to run
#cal_year <- "WL2019_minus_Shallow.Combined.1929"
#cal_year <- "WL2020_minus_Shallow.Combined.1929"
#cal_year <- "WL2019_minus_Shallow.Below.1929"
#cal_year <- "WL2020_minus_Shallow.Below.1929"



method_script_name <- "OK_Water_Levels" # OK_Water_Levels, Co_Kriging_GAM, Co_Kriging_Topo_Streams, Co_Kriging_Topo_DEM
method <- "OK_Water_Levels" # OK_Water_Levels, Co_Kriging_GAM, Co_Kriging_Topo_Streams

# Prepare data
source("data_manipulation.R")

# Call a Kriging/Co-Kriging technique
source(paste(method_script_name,".R", sep=""))



# extract values for each aquifer for cokriging with DEM
# create raster and write on file
#source("write_outputs.R")
































