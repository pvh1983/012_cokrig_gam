
# Notes:
# - Specify path to file in get_input.R
# - Run post_process.py to get water level input files
# - 

setwd("c:/Users/hpham/OneDrive - INTERA Inc/projects/012_rscript_geostats_s/scripts/")

# [1] Load libraries
source("get_libs.R")


# [2] Specify input parameters and choose a Kriging technique
GAM_sim_WL_yr <- as.integer(2020)
path_waterlevel = ("input/data/water_levels")
ifile_WL <- paste(path_waterlevel,"/Shallow.Wells.GMA12.YJ.hds.Smooth_",GAM_sim_WL_yr,"_hp.csv",sep='')
water_level <- read.csv(ifile_WL)


thre_cutoff <- as.integer(300000)
thre_width <- as.integer(10000)
list_ras_res <- c(500) # 500, 4000, 8000, 16000, 32000, 64000, 128000


method_script_name <- "Co_Kriging_Topo_Streams_Pumping" # OK_Water_Levels, Co_Kriging_GAM, Co_Kriging_Topo_Streams, Co_Kriging_Topo_DEM
method <- "CK_Topo_Str_Pmp" # OK_Water_Levels, Co_Kriging_GAM, Co_Kriging_Topo_Streams


# [3] Run each dataset
for (GAM_res in list_ras_res) {
  
  #data_col_name <- paste("WL",GAM_sim_WL_yr,".minus.Shallow.Below.",GAM_sim_WL_yr,".", GAM_res, sep="")
  data_col_name <- "WL2020"
  
  # [1] Read in librareis and input data
  
  source("get_input.R")
  
  
  # Prepare data
  source("data_manipulation.R")
  
  # Call a Kriging/Co-Kriging technique
  source(paste(method_script_name,".R", sep=""))
  
  
  
  # extract values for each aquifer for cokriging with DEM
  # create raster and write on file
  #source("write_outputs.R")
}
































