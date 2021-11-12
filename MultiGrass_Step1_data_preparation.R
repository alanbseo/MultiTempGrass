##### Analysis of UAV experiment from Apr 2018 - create RF/GBM prediction map ; use SEQ data #####

# CREATED BY:     BS & AS
# LAST MODIFIED:  30/10/2021

# Background:     Experiment to test suitability of 2 drone-based multispectral sensors to estimate
#                 vegetation parameters in alpine & prealpine grasslands
# PURPOSE:        Estimate vegetation parameter (biomass) comparing different models (RF, GBM, Linear Model)
#                 Create map of predicted biomass and maps/graphs for confidence interval
#                                  
# INPUT:          Raster image of UAV spectral data,
#                 csv-table with subplot data
# OUTPUT:         different figures for model comparison
#                 maps of predicted biomass
######################################################################################################

library(tidyverse)
library(rsample)      # data splitting 

library(raster)       # handling raster data
library(SpatialTools) # with function 
# library(GSIF)         # create_blank_raster
# library(geoR)
library(rgdal)


library(caret)          # an aggregator package for performing many machine learning models
# library(pdp)            # model visualization


# library(scales) # alpha channel

# Parallel processing 
library(parallel)
library(doSNOW)
n_cores = floor(detectCores() / 1.2) # # number of cpu cores to use


# including zero biomass values
includingZero = FALSE

#############################
### Data (pre-)processing ###
#############################

#path.base = "G:/BioAtmo/schucknecht-a/R/UAV_experiment/N_estimation_ModelComparison/"
path.base = "~/Nextcloud/SUSALPS-RS2018_modelPaper/Gitlab/grasslanduas/"
path_uav_images = "/DATA10TB/Grassland_Multi/neuprozessierung_2019_2020_fe_rb_gesamt/"
path_canopy_heights = "/DATA10TB/Grassland_Multi/"

# set working directory
setwd(path.base)
source("UAV_commons.R")
source("UAV_exp_VI_zerobiomass.R")

path_data = "~/Nextcloud/MultiTempGrass/"
path_out = "~/Nextcloud/MultiTempGrass/tmp/"

ncell_window = 3 



################ 
# DATA SPLIT ###
#######################
#####################


#### APPLICABLE TO THE BOTH SENSORS ######################
#######################
# sensor_idx = 1 # 1: REM, 2: SEQ
# resp_idx = 1 # 1: DM_SI, 2: N_mean
# data_idx = 1 # 1: SRonly, 2: SR and plant height, 3: SR and VI, 4: SR, VI, and plant height

# The Sequoia (SEQ) has reflectance bands for the following central wavelengths: 550 nm, 660 nm, 735 nm and 790 nm. The columns are named accordingly (e.g. R_550). The REM data has 475, 560, 668, 717, and 840 nm bands. 


# Observations
df_new = read_csv(paste0(path_data, "/InputData/SUSALPS-RS_UAS+Sat_samplingData_2019-2021_all_20211025_forModelling.csv"), na = "NA") # latest (24 Okt 2021)


head(df_new)
colnames(df_new)


table(df_new$plot_ID)
table(df_new$subplot_ID)



# subplot coordinates
# cbind(df_new$x_coord, df_new$y_coord)

df_new_coords = df_new 

# Remove data points without coordinates 
df_new_coords = df_new_coords[(!is.na(df_new_coords$x_coord) & !is.na(df_new_coords$y_coord)),]

new_sp = SpatialPoints(cbind(df_new_coords$x_coord, df_new_coords$y_coord), proj4string = crs(proj4.UTM32N))


# idx_cols_nonUASdata = c(1, 3:17,27:61)
# idx_cols_todelete =  c("id", "X1",  "R_475_REM", "R_560_REM", "R_668_REM", "R_717_REM", "R_840_REM", "R_550_SEQ", "R_660_SEQ", "R_735_SEQ", "R_790_SEQ")
idx_cols_todelete =  c("X1")

# drop columns 
df_new_coords = df_new_coords[,setdiff(colnames(df_new_coords), idx_cols_todelete)]


# change column names 
# colnames(df_new_coords)[colnames(df_new_coords)=="Plot"] = "plot_ID"
# colnames(df_new_coords)[colnames(df_new_coords)=="plot"] = "plot_ID"

writeOGR(SpatialPointsDataFrame(new_sp, data = df_new_coords, proj4string = crs(proj4.UTM32N)), dsn = path_data, layer = "SUSALPS-RS_UAS+Sat_samplingData_2019-2021_all_20211015", driver = "ESRI Shapefile", overwrite_layer = T)




df_new_coords$sampling_dates_corrected = str_remove_all(df_new_coords$sampling_date, pattern = "-")


# sampling_dates = sort(unique(df_new_coords$sampling_date))
# sampling_dates_simplified = sapply(str_extract_all(sampling_dates, pattern = "[0-9]*", simplify = F), FUN = function(x) paste0(x, collapse = ""))
sampling_dates_corrected_v = sort(unique(df_new_coords$sampling_dates_corrected, pattern = "-"))


#### Image reading 



# Five sites. only seq images
fendt_images = list.files(path_uav_images, pattern = "fendt[A-z0-9\ ]*.tif$")
rottenbuch_images = list.files(path_uav_images, pattern = "rottenbuch[A-z0-9\ ]*.tif$")
gubitzmoos_images =  list.files(path_uav_images, pattern = "gubitzmoos[A-z0-9\ ]*.tif$")
roedensdorf_images =  list.files(path_uav_images, pattern = "roedensdorf[A-z0-9\ ]*.tif$")
obernschreez_images =  list.files(path_uav_images, pattern = "obernschreez[A-z0-9\ ]*.tif$")

# UAV based canopy height
bestand_2019_images = list.files(paste0(path_canopy_heights, "/bestand_2019"), pattern = "[A-z0-9\ ]*.tif$")
differenzen_2019_2021_min_images = list.files(paste0(path_canopy_heights, "/differenzen_2019_2021_min"), pattern = "[A-z0-9\ ]*.tif$")



sort(unique(str_extract(gubitzmoos_images, pattern = "[0-9]{8}"))) # extract dates
sort(unique(str_extract(roedensdorf_images, pattern = "[0-9]{8}"))) # extract dates
sort(unique(str_extract(obernschreez_images, pattern = "[0-9]{8}"))) # extract dates

sort(unique(str_extract(bestand_2019_images, pattern = "[0-9]{8}"))) # extract dates
sort(unique(str_extract(differenzen_2019_2021_min_images, pattern = "[0-9]{8}"))) # extract dates


uav_dates1 = sort(unique(str_extract(fendt_images, pattern = "[0-9]{8}"))) # extract dates
uav_dates2 = sort(unique(str_extract(gubitzmoos_images, pattern = "[0-9]{8}"))) # extract dates
uav_dates3 = sort(unique(str_extract(roedensdorf_images, pattern = "[0-9]{8}"))) # extract dates
uav_dates4 = sort(unique(str_extract(obernschreez_images, pattern = "[0-9]{8}"))) # extract dates


uav_dates = sort(unique(c(uav_dates1, uav_dates2, uav_dates3, uav_dates4)))
# stopifnot(all(uav_dates == sort(unique(str_extract(rottenbuch_images, pattern = "[0-9]{8}"))))) # compare dates from RB site, and stop if not matching

# 16 dates
print(length(uav_dates))
# redundant uav images
uav_dates[!(uav_dates %in% sampling_dates_corrected_v)]
# [1] "20190329" "20190625" "20210521" 

# missing uav images (three missing)
sampling_dates_corrected_v  [!(sampling_dates_corrected_v %in% uav_dates)]
# "20190626"
canopy_dates1 = sort(unique(str_extract(bestand_2019_images, pattern = "[0-9]{8}"))) # extract dates
canopy_dates2 = sort(unique(str_extract(differenzen_2019_2021_min_images, pattern = "[0-9]{8}"))) # extract dates
canopy_dates = sort(unique(c(canopy_dates2, canopy_dates2)))


# 14 dates
print(length(canopy_dates))
# redundant canopy images
canopy_dates[!(canopy_dates %in% sampling_dates_corrected_v)]
# [1] "20190329" "20190625" 

canopy_dates[!(canopy_dates %in% uav_dates)]


# missing canopy images (three missing)
sampling_dates_corrected_v  [!(sampling_dates_corrected_v %in% canopy_dates)]
# "20190626"


# replace 
sampling_dates_corrected_v[sampling_dates_corrected_v=="20190626"] = "20190625"
df_new_coords$sampling_dates_corrected  [df_new_coords$sampling_dates_corrected=="20190626"] = "20190625"

# check again (no missing)
sampling_dates_corrected_v[!(sampling_dates_corrected_v %in% uav_dates)]
unique(df_new_coords$sampling_dates_corrected[!(df_new_coords$sampling_dates_corrected %in% uav_dates)])



# dates when sampling was done
uav_dates[uav_dates %in% sampling_dates_corrected_v] # 15 dates

canopy_dates[canopy_dates %in% sampling_dates_corrected_v] # 15 dates


# 
sampling_dates_process_v = sampling_dates_corrected_v[(sampling_dates_corrected_v %in% uav_dates)]




# see plots matching subplot ids
table(substr(df_new_coords$plot_ID, 1,3) == substr(df_new_coords$subplot_ID, start = 1, stop = 3))

# df_new_coords$plot_ID [df_new_coords$plot_ID != substr(df_new_coords$Subplot_ID, start = 1, stop = 3)]
# df_new_coords$Subplot_ID [df_new_coords$plot_ID != substr(df_new_coords$Subplot_ID, start = 1, stop = 3)]



uav_bands_small = c("green", "red", "red edge", "nir")
uav_bands_cap = c("Green", "Red", "RedEdge", "NIR")

uav_sites = c("fendt", "rottenbuch", "gubitzmoos", "obernschreez", "roedensdorf")
usv_sites_abb = c("FE", "RB", "GM", "OS", "RD")

window_sizes = c(1,3,5)

# n_newcols = length(usv_sites_abb) * length(uav_bands_cap) * length(window_sizes)

n_dates = length(sampling_dates_process_v)
n_cols_df =  length(uav_bands_cap) + 18 + 4*2 # 4 bands + 18 vis + 7*2 canopy heights

# avg. max. min, 25 and 75 percentiles, median, sd



# Container for the result
overlay_SEQ_arr = array(NA,dim =  c(nrow(df_new_coords),length(usv_sites_abb),n_dates, n_cols_df))#, length(window_sizes)))

dim(overlay_SEQ_arr)



site_idx = 2
date_idx=1


if (FALSE) { 
    beginCluster()
    
    for (site_idx in seq_along(usv_sites_abb)) { 
        
        for (date_idx in 1:n_dates) { 
            
            
            uav_date = sampling_dates_process_v[date_idx]
            uav_site = uav_sites[site_idx]
            
            
            uav_filename_tmp = paste0(path_uav_images, uav_site, "_msp_", uav_date, "_wo_sun_transparent_reflectance_", uav_bands_small, ".tif")
            
            if (!file.exists(uav_filename_tmp)) { 
                print("UAV image does not exist")
                next() # no uav image
            }
            
            uav_rs_tmp = stack(paste0(path_uav_images, uav_site, "_msp_", uav_date, "_wo_sun_transparent_reflectance_", uav_bands_small, ".tif"))
            
            names(uav_rs_tmp) = var_names_SR_seq
            # NAvalue(uav_rs_tmp) = na_mask # @TODO is it the same? -10000? they seem not to include the mask values.
            
            
            ## Band info 
            # Coastal blue 444(28)*, blue 475(32), green 531(14)*, green 560(27), red 650(16)*, red 668(14), red edge 705(10)*, red edge 717(12), red edge 740(18)*, NIR 842(57) 
            
            
            print("UAV: calculate 3x3 average values")
            
            
            # Focal images    
            focal_rs_3tmp =stack(lapply(1:length(var_names_SR_seq), FUN = function(x) focal(uav_rs_tmp[[x]],  w=matrix(1/(ncell_window^2),nrow=ncell_window,ncol=ncell_window), fun=sum, na.rm=FALSE, pad=FALSE, padValue=NA)))
            
            # ncell_window = 5
            # focal_rs_5tmp =stack(lapply(1:length(var_names_SR_seq), FUN = function(x) focal(uav_rs_tmp[[x]],  w=matrix(1/(ncell_window^2),nrow=ncell_window,ncol=ncell_window), fun=sum, na.rm=FALSE, pad=FALSE, padValue=NA)))
            
            
            # layer names 
            names(focal_rs_3tmp ) = var_names_SR_seq 
            # names(uav_rs_tmp) = names(focal_rs_5tmp) =  # SEQ
            
            # overlay    
            # uav_overlay1_tmp = raster::extract(uav_rs_tmp, new_sp)   # 1 pix
            uav_overlay3_tmp = raster::extract(focal_rs_3tmp, new_sp) # 3 pix
            # uav_overlay5_tmp = raster::extract(focal_rs_5tmp, new_sp) # 5 pix
            
            # overlay1_vi_SEQ = retrieveVI(df= data.frame(uav_overlay1_tmp), doREM = F, doSEQ = T)
            overlay3_vi_SEQ = retrieveVI(df= data.frame(uav_overlay3_tmp), doREM = F, doSEQ = T)
            # overlay5_vi_SEQ = retrieveVI(df= data.frame(uav_overlay5_tmp), doREM = F, doSEQ = T)
            
            
            
            ### Canopy heights
            # "/DATA10TB/Grassland_Multi/bestand_2019/rottenbuch_dsm_20190329_minus_2019min_dgm.tif"
            # "/DATA10TB/Grassland_Multi/differenzen_2019_2021_min/rottenbuch_dsm_20190329_minus_2019_21_min_dgm.tif"
            
            canopy_filename1_tmp = paste0(path_canopy_heights, "bestand_2019/", uav_site, "_dsm_", uav_date, "_minus_2019min_dgm.tif")
            
            if (!file.exists(canopy_filename1_tmp)) { 
                print("Canopy height image does not exist")
                print(canopy_filename1_tmp)
                canopy1_tmp = data.frame(matrix(NA, nrow = nrow(df_new_coords), ncol = 4))
            } else { 
                
                canopy1_rs_tmp = raster(canopy_filename1_tmp)
                
                names(canopy1_rs_tmp) = "bestand_2019"
                print("Canopy height1: calculate 3x3 average values")
                
                # Focal images    
                focal_canopy1_avg_tmp = focal(canopy1_rs_tmp, w=matrix(1/(ncell_window^2),nrow=ncell_window,ncol=ncell_window), fun=sum, na.rm=FALSE, pad=FALSE, padValue=NA)
                
                focal_canopy1_med_tmp = focal(canopy1_rs_tmp, w=matrix(1/(ncell_window^2),nrow=ncell_window,ncol=ncell_window), fun=median, na.rm=FALSE, pad=FALSE, padValue=NA) * (ncell_window^2)
                
                focal_canopy1_min_tmp = focal(canopy1_rs_tmp, w=matrix(1/(ncell_window^2),nrow=ncell_window,ncol=ncell_window), fun=min, na.rm=FALSE, pad=FALSE, padValue=NA) * (ncell_window^2)
                
                focal_canopy1_max_tmp = focal(canopy1_rs_tmp, w=matrix(1/(ncell_window^2),nrow=ncell_window,ncol=ncell_window), fun=max, na.rm=FALSE, pad=FALSE, padValue=NA) * (ncell_window^2)
                 
                 # overlay    
                canopy1_avg3_tmp = raster::extract(focal_canopy1_avg_tmp, new_sp) # 3 pix
                canopy1_med3_tmp = raster::extract(focal_canopy1_med_tmp, new_sp) # 3 pix
                canopy1_min3_tmp = raster::extract(focal_canopy1_min_tmp, new_sp) # 3 pix
                canopy1_max3_tmp = raster::extract(focal_canopy1_max_tmp, new_sp) # 3 pix
                 
                
                canopy1_tmp = cbind(canopy1_avg3_tmp, canopy1_med3_tmp, canopy1_min3_tmp, canopy1_max3_tmp)
            }
            
            
            canopy_filename2_tmp = paste0(path_canopy_heights, "differenzen_2019_2021_min/", uav_site, "_dsm_", uav_date, "_minus_2019min_dgm.tif")
            
            if (!file.exists(canopy_filename2_tmp)) { 
                print("Canopy height image does not exist")
                print(canopy_filename2_tmp)
                canopy2_tmp = data.frame(matrix(NA, nrow = nrow(df_new_coords), ncol = 4))
            } else { 
                
                canopy2_rs_tmp = raster(canopy_filename2_tmp)
                
                names(canopy2_rs_tmp) = "differenzen_2019_2021_min"
                print("Canopy height2: calculate 3x3 average values")
                
                # Focal images    
                focal_canopy2_avg_tmp = focal(canopy2_rs_tmp, w=matrix(1/(ncell_window^2),nrow=ncell_window,ncol=ncell_window), fun=sum, na.rm=FALSE, pad=FALSE, padValue=NA)
                
                focal_canopy2_med_tmp = focal(canopy2_rs_tmp, w=matrix(1/(ncell_window^2),nrow=ncell_window,ncol=ncell_window), fun=median, na.rm=FALSE, pad=FALSE, padValue=NA) * (ncell_window^2)
                
                focal_canopy2_min_tmp = focal(canopy2_rs_tmp, w=matrix(1/(ncell_window^2),nrow=ncell_window,ncol=ncell_window), fun=min, na.rm=FALSE, pad=FALSE, padValue=NA) * (ncell_window^2)
                
                focal_canopy2_max_tmp = focal(canopy2_rs_tmp, w=matrix(1/(ncell_window^2),nrow=ncell_window,ncol=ncell_window), fun=max, na.rm=FALSE, pad=FALSE, padValue=NA) * (ncell_window^2)
                
                # overlay    
                canopy2_avg3_tmp = raster::extract(focal_canopy2_avg_tmp, new_sp) # 3 pix
                canopy2_med3_tmp = raster::extract(focal_canopy2_med_tmp, new_sp) # 3 pix
                canopy2_min3_tmp = raster::extract(focal_canopy2_min_tmp, new_sp) # 3 pix
                canopy2_max3_tmp = raster::extract(focal_canopy2_max_tmp, new_sp) # 3 pix
                
                
                canopy2_tmp = cbind(canopy2_avg3_tmp, canopy2_med3_tmp, canopy2_min3_tmp, canopy2_max3_tmp)
            }
            
             
            
            
            # store processed values
            # overlay_SEQ_arr[, site_idx, date_idx, , 1] = as.matrix(overlay1_vi_SEQ)
            # overlay_SEQ_arr[, site_idx, date_idx, , 2] = as.matrix(overlay3_vi_SEQ)
            # overlay_SEQ_arr[, site_idx, date_idx, , 3] = as.matrix(overlay5_vi_SEQ)
            
            overlay_SEQ_arr[, site_idx, date_idx, ] = as.matrix(cbind(overlay3_vi_SEQ, canopy1_tmp, canopy2_tmp))
        }
        
    } 
    endCluster()
    
    # colnames(overlay1_vi_SEQ)
    saveRDS(overlay_SEQ_arr, "overlay_SEQ_arr_tmp.Rds")
} else { 
    overlay_SEQ_arr = readRDS("overlay_SEQ_arr_tmp.Rds")
}

gc()


# Combine different sites and dates
dimnames(overlay_SEQ_arr)[[2]] = usv_sites_abb 
dimnames(overlay_SEQ_arr)[[3]] = sampling_dates_corrected_v 
# dimnames(overlay_SEQ_arr)[[5]] = paste0("W_", window_sizes)

dimnames(overlay_SEQ_arr)

# find corresponding values
dim(df_new_coords)

df_new_coords$scene_ids = substr(df_new_coords$plot_ID, 1,2)


site_idxs = match(df_new_coords$scene_ids, usv_sites_abb)
date_idxs = match(df_new_coords$sampling_dates_corrected, sampling_dates_corrected_v)


# row_idx = 1 


overlay_tmp_df = foreach(row_idx = 1:nrow(df_new_coords), .combine = "rbind") %do% { 
    
    site_idxs[row_idx]
    if (is.na(site_idxs[row_idx])) { 
        return(rep(NA, 66))   # Other sites
    }
    
    # date_idxs[row_idx]
    
    overlay_tmp_arr = overlay_SEQ_arr[row_idx, site_idxs[row_idx], date_idxs[row_idx], ]
    dim(overlay_tmp_arr)
    
    # cbind data with different window sizes
    overlay_tmp_v = c(overlay_tmp_arr) # [,1], overlay_tmp_arr[,2], overlay_tmp_arr[,3])
    
    return(overlay_tmp_v)
}



seq_colnames= c("R_550", "R_660",  "R_735",  "R_790",  "SR" ,    "NDVI"
                ,"RDVI", "MSR1"  , "DVI",    "SAVI"  , "OSAVI" , "MSAVI" 
                ,"NDVIre", "RRI1" ,  "RRI2"  , "MCARI",  "Datts" , "aDVI"
                ,"RARSa", "MTVI"  , "MCARI2", "MCARI1")


# new_colnames = as.vector(sapply(dimnames(overlay_SEQ_arr)[[5]],  FUN = function(x) paste0(seq_colnames, "_", x)))

colnames(overlay_tmp_df) = seq_colnames


overlay_tmp_df = data.frame(overlay_tmp_df)



plot(overlay_tmp_df$R_550, overlay_tmp_df$R_550)
plot(overlay_tmp_df$R_550, overlay_tmp_df$R_550)

plot(overlay_tmp_df$R_550, overlay_tmp_df$NDVI)

table(is.na(overlay_tmp_df$R_550))

# Create datasets

# observations
df_final = cbind(df_new_coords, overlay_tmp_df)
df_final$X1 = NULL

##### save df to csv file
write.csv(df_final, file = "SUSALPS-RS_UAS+Sat_samplingData_2019-2021_all_20211015_UASdata_v2.csv", row.names = F)

writeOGR(SpatialPointsDataFrame(new_sp, data = df_final, proj4string = crs(proj4.UTM32N)), dsn = ".", layer = "SUSALPS-RS_UAS+Sat_samplingData_2019-2021_all_20211015_UASdata_v2", driver = "ESRI Shapefile", overwrite_layer = T)




stop("ends here (30 Okt 2021)")


# @todo input data csv

# 
# 
# df$plot_ID = substr(df$subplot_ID, 1, 3)
# 
# # define factor variables 
# df$site <- factor(df$site) 
# # df$plot <- factor(df$plot)
# df$LMA_PFT <- factor(df$LMA_PFT)
# 
# colnames(df)[colnames(df)=="DM_gm2"] = "DM_SI"
# 
# 
# # resp_idx = 2 
# # sensor_idx = 1
# # data_idx = 2 
# 
# 
# for (resp_idx in 1:length(resp_names)) {
#     
#     resp_name = resp_names[resp_idx]
#     
#     for (sensor_idx in 1:length(sensor_names)) { 
#         
#         sensor_name_tmp = sensor_names[sensor_idx]
#         # df_sensor = df[df$Sensor == sensor_name_tmp,]  
#         df_sensor = df
#         print(sensor_name_tmp)
#         
#         # all predictors used
#         var_names_tmp_list = list(var_names_list_SR[[sensor_idx]], var_names_plantheight, var_names_list_VI[[sensor_idx]] )
#         
#         
#         for (data_idx in 1:length(data_names)) { 
#             
#             
#             data_name_tmp = data_names[data_idx]
#             print(data_name_tmp)
#             
#             data_predictoridx_tmp = data_predictoridx_list[[data_idx]]
#             
#             # ###### N dataset does not have 
#             # if (resp_name == "N_mean") { 
#             #     # does not have VI
#             #     data_predictoridx_tmp = data_predictoridx_tmp[data_predictoridx_tmp!= 3] # drop VI 
#             # }
#             
#             # variables used
#             var_names_tmp = unlist(var_names_tmp_list[data_predictoridx_tmp])
#             
#             
#             # Formulae 
#             formula_tmp = as.formula(paste0(resp_name, " ~ ", paste0(var_names_tmp, collapse = " +")))
#             
#             
#             
#             if (sensor_name_tmp %in% c("SEQ", "REMwoBlue")) { 
#                 # restrict input to FE and RB (to be comparable to REM model)
#                 model_input <- subset(df_sensor, site == "FE" | site == "RB", select = c(resp_names[resp_idx], var_names_tmp))
#                 
#                 df_sensor_subset = subset(df_sensor,  site == "FE" | site == "RB") # for plotting
#                 df_complete = df_sensor_subset[complete.cases(model_input),] # for plotting
#                 
#                 
#                 
#             } else if (sensor_name_tmp=="REM") {
#                 # otherwise (REM) use two sites (FE, RB, EL)
#                 model_input <- subset(df_sensor, select = c(resp_names[resp_idx],  var_names_tmp))
#                 df_complete = df_sensor[complete.cases(model_input),] # for plotting
#                 
#             } 
#             # @todo ? remove NA in response 
#             
#             
#             model_input = model_input[!is.na(model_input[, resp_name]),]
#             
#             model_input[!complete.cases(model_input), ]
#             
#             model_input <- model_input[complete.cases(model_input), ]
#             stopifnot(nrow(df_complete)==nrow(model_input))
#             
#             # head(model_input)
#             nrow(model_input) 
#             # 91 for REM  
#             #   for SEQ (just FE, RB)
#             
#             
#             if (sensor_name_tmp %in% c("SEQ")){ 
#                 # ES site for validation 
#                 
#                 model_input_validation <- subset(df_sensor, site == "EL", select = c(resp_names[resp_idx], var_names_tmp))
#                 
#                 df_sensor_validation_subset = subset(df_sensor,  site == "EL") # for plotting
#                 df_complete_validation = df_sensor_validation_subset[complete.cases(model_input_validation),] # for plotting
#                 
#                 
#                 
#                 model_input_validation[!complete.cases(model_input_validation), ]
#                 
#                 model_input_validation <- model_input_validation[complete.cases(model_input_validation), ]
#                 stopifnot(nrow(df_complete_validation)==nrow(model_input_validation))
#                 
#                 # head(model_input)
#                 nrow(df_complete_validation) 
#                 write.table(model_input_validation, file = paste0(path_out, "/ModelData/InputData/Model_input_validation_",resp_name, "_", sensor_name_tmp, "_", data_name_tmp, ".csv"))
#                 
#             }
#             
#             ## save model information 
#             
#             formula_tmp_txt = paste(formula_tmp [2],formula_tmp[3], sep=' ~ ')
#             
#             model_information = data.frame(resp_name, sensor_name = sensor_name_tmp, data_name = data_name_tmp, formula = formula_tmp_txt, nsamples= nrow(model_input))
#             
#             write.csv(model_information, file = paste0(path_out, "/ModelData/ModelInformation/Model_information_",resp_name, "_", sensor_name_tmp, "_", data_name_tmp, ifelse(includingZero, "", "_woZero"),".csv"), row.names = F)
#             
#             write.table(model_input, file = paste0(path_out, "/ModelData/InputData/Model_input_",resp_name, "_", sensor_name_tmp, "_", data_name_tmp,  ifelse(includingZero, "", "_woZero"), ".csv"))
#             
#             
#         }
#     }
# }






