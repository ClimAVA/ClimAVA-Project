#########################################
# Author - Andre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to create RF models to be used in downscaling GCM data

################################################################
# 
# To make the process faster, we separated the the training of the form the downscaling. 
# I.e. models are trained through this script and exported (saved). In the downscaling script
# models are loaded and used for downscaling.
# 
# Note: I realized that this script could be a lot shorter after I finished it, 
# I will update it when i have a chance. 
# 
################################################################


#packages
library(ncdf4)
library(dplyr)
library(randomForest)
library(Metrics)
library(doParallel)
library(spam)
library(foreign)


models <- read.csv("~/Documents/scripts/cmip6/2023/models_2023.csv") #has all the other information necessary to create the HTTPs
variables <- read.csv("~/Documents/scripts/cmip6/2023/Variables.csv")
# grid has the final grid with each and every pixel of the final product
grid <- read.dbf("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/grid/master_pixels.dbf")

#########################################################################
#####Creating directories--------

# getwd()
# setwd("/scratch/general/vast/u6047395/cmip6/rf_models/")
# for (m in 1:17){
#   model = models[m,1]
#   dir.create(model)
# }
# 
# for (m in 1:17){
#   model = models[m,1]
#   for (i in 1:3){
#     v = vars[i]
#     dir = paste0(model,"/",v)
#     dir.create(dir)
#   }
# }

####################################################################

# loop through variables
for (v in 1:length(variables)){
  
  var <- paste(variables[v,3])
  print(var)
  
  # loop through models
  for(m in 1:length(models$ID)) {
    
    model = models[m,1]
    print(model)
    print(Sys.time())
    
    # guide contains the grid from each of the resampled GCMs
    guide <- read.dbf(paste0("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/grids/subsets/",model,"_guide.dbf"))
    
    guide_lat_res <- models[m,24]
    guide_lon_res <- models[m,25]
    guide_lat_length <- length(unique(guide$lat))
    guide_lon_length <- length(unique(guide$lon))
    guide_lat <- unique(guide$lat)
    
    # numbers of predictor change based on the location of the pixel in the map.
    # the following is used to find out if the pixel is in the corners of the map
    # and so help determine how many predictors it will have.
    # This part of the code will be simplified in the future. 
    
    up <- max(guide$lat)
    down <- min(guide$lat)
    left <- min(guide$lon_flip)
    right <- max(guide$lon_flip)
    
    #loading resampled PRISM
    nc_resampled <- nc_open(paste0("/scratch/general/vast/u6047395/prism/resampled/prism_",var,"_day_",model,"_resampled.nc"))
    
    #creating Array
    resampled_array <- ncvar_get(nc_resampled, var)
    rm(nc_resampled)
    
    # loading original PRISM
    nc_1 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/AOI/prism_sw_",var,"_19810101_19851231.nc"))
    nc_2 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/AOI/prism_sw_",var,"_19860101_19901231.nc"))
    nc_3 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/AOI/prism_sw_",var,"_19910101_19951231.nc"))
    nc_4 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/AOI/prism_sw_",var,"_19960101_20001231.nc"))
    nc_5 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/AOI/prism_sw_",var,"_20010101_20051231.nc"))
    nc_6 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/AOI/prism_sw_",var,"_20060101_20101231.nc"))
    nc_7 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/AOI/prism_sw_",var,"_20110101_20151231.nc"))
    nc_8 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/AOI/prism_sw_",var,"_20160101_20201231.nc"))
    
    # extracting information from original PRISM
    lon <- unique(ncvar_get(nc_1, "lon")) # all longitudes
    lon_res <- abs(lon[1] - lon[2]) # lon resolution
    lat_lenght <- length(ncvar_get(nc_1, "lat")) 
    lat <- unique(ncvar_get(nc_1, "lat"))
    lat_res <- abs(lat[1] - lat[2])
    
    # creating arrays
    array1 <- ncvar_get(nc_1, var)
    array2 <- ncvar_get(nc_2, var)
    array3 <- ncvar_get(nc_3, var)
    array4 <- ncvar_get(nc_4, var)
    array5 <- ncvar_get(nc_5, var)
    array6 <- ncvar_get(nc_6, var)
    array7 <- ncvar_get(nc_7, var)
    array8 <- ncvar_get(nc_8, var)
    
    # removing ncs to clear memory
    rm(nc_1, nc_2, nc_3, nc_4, nc_5, nc_6, nc_7, nc_8)
    
    # setting dopar parameters
    registerDoParallel(64)
    
    # Models are created for each pixel
    foreach (i = 1:158710, 
             .packages = c("spam","doParallel","foreach","ncdf4","dplyr", "randomForest", "Metrics")) %dopar% {
    
               IN_OUT <- grid[i,4] #The forth column in grid has a code to tell if what pixels are NA (out) or not. 
               
               if (IN_OUT == 0){} # if 0 (out/NA), do nothing
               
               else if (IN_OUT == 1){ #if 1 (in) create a model for it.
                 
                 ID = i
                 print(ID)
                 
                 #creating a name for the model
                 model_name <- paste0("/scratch/general/vast/u6047395/cmip6/rf_models/",model,"/",var,"/_rf_",ID,"_",model,"_",var,".rds")
                                       
                 #testing if model was already created. This is useful if you need to debug or stop/restart the job.
                 tem <- file.exists(model_name)
                 
                 if (tem == FALSE){
                   
                   #lat and long of PRISM pixel
                   grid_lon = grid[i,2]
                   grid_lat = grid[i,3]
                   
                   # finding the resampled (coarse) pixel the is closest to the selected PRSIM pixel
                   dist <- guide
                   dist$dist <- sqrt((dist$lon_flip-grid_lon)^2+(dist$lat-grid_lat)^2)
                   row <- dist[which.min(dist$dist),]
                   
                   #defining coordinates for all predictors (coarse pixels)
                   X1 <- row[1,4] #lon
                   Y1 <- row[1,2] #lat
                   X2 <- row[1,4]
                   Y2 <- row[1,2] - guide_lat_res
                   X3 <- row[1,4] + guide_lat_res
                   Y3 <- row[1,2] - guide_lat_res
                   X4 <- row[1,4] + guide_lat_res
                   Y4 <- row[1,2]
                   X5 <- row[1,4] + guide_lat_res
                   Y5 <- row[1,2] + guide_lat_res
                   X6 <- row[1,4]
                   Y6 <- row[1,2] + guide_lat_res
                   X7 <- row[1,4] - guide_lat_res
                   Y7 <- row[1,2] + guide_lat_res
                   X8 <- row[1,4] - guide_lat_res
                   Y8 <- row[1,2]
                   X9 <- row[1,4] - guide_lat_res
                   Y9 <- row[1,2] - guide_lat_res
                   
                   # all if statements that comes after this point are due to the location of the pixel in relation to
                   # the corner of the map.This is the part that will be simplified in the future.
                   # the below diagram explains how I number the predictors
                   
                   ##### 1 is the center pixels
                   #765# 2 is below the center
                   #814# 3 is the lower right pixels and so on...
                   #923#
                   #####
                   
                   if (X1 == left & Y1 == up){ # this is the left upper corner. So it will have only 4 predictors
                     
                     X_var <- round((grid_lon - lon[1]) / lon_res,0) + 1
                     Y_var <- as.double(round(lat_lenght - (grid_lat - tail(lat,1))/lat_res))
                     
                     # extracting predictant data
                     var_1 <- array1[X_var,Y_var,1:1825]
                     var_2 <- array2[X_var,Y_var,1:1825]
                     var_3 <- array3[X_var,Y_var,1:1825]
                     var_4 <- array4[X_var,Y_var,1:1825]
                     var_5 <- array5[X_var,Y_var,1:1825]
                     var_6 <- array6[X_var,Y_var,1:1825]
                     var_7 <- array7[X_var,Y_var,1:1825]
                     var_8 <- array8[X_var,Y_var,1:1825]
                     
                    VAR <- c(var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8)
                    
                    #1 Extracting center predictor
                    
                    X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                    Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                    
                    cov_1 <- resampled_array[X_1,Y_1,1:14600]
                   
                    #2
                    
                    X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                    Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                    
                    cov_2 <- resampled_array[X_2,Y_2,1:14600]
                    
                    #3
                    
                    X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                    Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                    
                    cov_3 <- resampled_array[X_3,Y_3,1:14600]
                    
                    #4
                    
                    X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                    Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                    
                    cov_4 <- resampled_array[X_4,Y_4,1:14600]
                    
                    
                    # creating a calibration data frame
                    cal <- as.data.frame(VAR) 
                    
                    cal$cov_1 <- cov_1
                    cal$cov_2 <- cov_2
                    cal$cov_3 <- cov_3
                    cal$cov_4 <- cov_4
                    
                    #removing all columns with NAs
                    cal <- cal[, colSums(is.na(cal)) == 0]
                    
                    # training and exporting RF model
                    rf <- randomForest(VAR ~ cov_1 + cov_2 + cov_3 + cov_4, data = cal, ntree =30)
                    saveRDS(rf, model_name)
                    rm(rf, grid_lon, grid_lat, dist, row, var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8,
                       VAR, cov_1, cov_2, cov_3, cov_4, cal)
                    gc()
                    
                   }
                   
                   else if (X1 == right & Y1 == up){
                     
                     X_var <- round((grid_lon - lon[1]) / lon_res,0) + 1
                     Y_var <- as.double(round(lat_lenght - (grid_lat - tail(lat,1))/lat_res))
                     
                     var_1 <- array1[X_var,Y_var,1:1825]
                     var_2 <- array2[X_var,Y_var,1:1825]
                     var_3 <- array3[X_var,Y_var,1:1825]
                     var_4 <- array4[X_var,Y_var,1:1825]
                     var_5 <- array5[X_var,Y_var,1:1825]
                     var_6 <- array6[X_var,Y_var,1:1825]
                     var_7 <- array7[X_var,Y_var,1:1825]
                     var_8 <- array8[X_var,Y_var,1:1825]
                     
                     VAR <- c(var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8)
                     
                     #1
                     
                     X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                     Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_1 <- resampled_array[X_1,Y_1,1:14600]
                     
                     #2
                     
                     X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                     Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_2 <- resampled_array[X_2,Y_2,1:14600]
                     
                     #8
                     
                     X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                     Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_8 <- resampled_array[X_8,Y_8,1:14600]
                     
                     #9
                     
                     X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                     Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_9 <- resampled_array[X_9,Y_9,1:14600]
                     
                     
                     
                     cal <- as.data.frame(VAR) 
                     
                     cal$cov_1 <- cov_1
                     cal$cov_2 <- cov_2
                     cal$cov_8 <- cov_8
                     cal$cov_9 <- cov_9
                     
                     cal <- cal[, colSums(is.na(cal)) == 0]
                     
                     rf <- randomForest(VAR ~ cov_1 + cov_2 + cov_8 + cov_9, data = cal, ntree =30)
                     saveRDS(rf, model_name)
                     rm(rf, grid_lon, grid_lat, dist, row, var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8,
                        VAR, cov_1, cov_2, cov_8, cov_9, cal)
                     gc()
                     
                   }
                   
                   else if (X1 == right & Y1 == down){
                     
                     X_var <- round((grid_lon - lon[1]) / lon_res,0) + 1
                     Y_var <- as.double(round(lat_lenght - (grid_lat - tail(lat,1))/lat_res))
                     
                     var_1 <- array1[X_var,Y_var,1:1825]
                     var_2 <- array2[X_var,Y_var,1:1825]
                     var_3 <- array3[X_var,Y_var,1:1825]
                     var_4 <- array4[X_var,Y_var,1:1825]
                     var_5 <- array5[X_var,Y_var,1:1825]
                     var_6 <- array6[X_var,Y_var,1:1825]
                     var_7 <- array7[X_var,Y_var,1:1825]
                     var_8 <- array8[X_var,Y_var,1:1825]
                     
                     VAR <- c(var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8)
                     
                     #1
                     
                     X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                     Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_1 <- resampled_array[X_1,Y_1,1:14600]
                     
                     #6
                     
                     X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                     Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_6 <- resampled_array[X_6,Y_6,1:14600]
                     
                     #7
                     
                     X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                     Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_7 <- resampled_array[X_7,Y_7,1:14600]
                     
                     #8
                     
                     X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                     Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_8 <- resampled_array[X_8,Y_8,1:14600]
                     
                     
                     
                     cal <- as.data.frame(VAR) 
                     
                     cal$cov_1 <- cov_1
                     cal$cov_6 <- cov_6
                     cal$cov_7 <- cov_7
                     cal$cov_8 <- cov_8
                     
                     cal <- cal[, colSums(is.na(cal)) == 0]
                     
                     rf <- randomForest(VAR ~ cov_1 + cov_6 + cov_7 + cov_8, data = cal, ntree =30)
                     saveRDS(rf, model_name)
                     rm(rf, grid_lon, grid_lat, dist, row, var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8,
                        VAR, cov_1, cov_6, cov_7, cov_8, cal)
                     gc()
                     
                   }
                   
                   else if (Y1 == up){
                     
                     X_var <- round((grid_lon - lon[1]) / lon_res,0) + 1
                     Y_var <- as.double(round(lat_lenght - (grid_lat - tail(lat,1))/lat_res))
                     
                     var_1 <- array1[X_var,Y_var,1:1825]
                     var_2 <- array2[X_var,Y_var,1:1825]
                     var_3 <- array3[X_var,Y_var,1:1825]
                     var_4 <- array4[X_var,Y_var,1:1825]
                     var_5 <- array5[X_var,Y_var,1:1825]
                     var_6 <- array6[X_var,Y_var,1:1825]
                     var_7 <- array7[X_var,Y_var,1:1825]
                     var_8 <- array8[X_var,Y_var,1:1825]
                     
                     VAR <- c(var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8)
                     
                     #1
                     
                     X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                     Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_1 <- resampled_array[X_1,Y_1,1:14600]
                     
                     #2
                     
                     X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                     Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_2 <- resampled_array[X_2,Y_2,1:14600]
                     
                     #3
                     
                     X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                     Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_3 <- resampled_array[X_3,Y_3,1:14600]
                     
                     #4
                     
                     X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                     Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_4 <- resampled_array[X_4,Y_4,1:14600]
                     
                     #8
                     
                     X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                     Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_8 <- resampled_array[X_8,Y_8,1:14600]
                     
                     #9
                     
                     X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                     Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_9 <- resampled_array[X_9,Y_9,1:14600]
                     
                     cal <- as.data.frame(VAR) 
                     
                     cal$cov_1 <- cov_1
                     cal$cov_2 <- cov_2
                     cal$cov_3 <- cov_3
                     cal$cov_4 <- cov_4
                     cal$cov_8 <- cov_8
                     cal$cov_9 <- cov_9
                     
                     cal <- cal[, colSums(is.na(cal)) == 0]
                     
                     rf <- randomForest(VAR ~ cov_1 + cov_2 + cov_3 + cov_4 +cov_8 + cov_9, data = cal, ntree =30)
                     saveRDS(rf, model_name)
                     rm(rf, grid_lon, grid_lat, dist, row, var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8,
                        VAR, cov_1, cov_2, cov_3, cov_4, cov_8, cov_9, cal)
                     gc()
                     
                   }
                   
                   else if (X1 == right){
                     
                     X_var <- round((grid_lon - lon[1]) / lon_res,0) + 1
                     Y_var <- as.double(round(lat_lenght - (grid_lat - tail(lat,1))/lat_res))
                     
                     var_1 <- array1[X_var,Y_var,1:1825]
                     var_2 <- array2[X_var,Y_var,1:1825]
                     var_3 <- array3[X_var,Y_var,1:1825]
                     var_4 <- array4[X_var,Y_var,1:1825]
                     var_5 <- array5[X_var,Y_var,1:1825]
                     var_6 <- array6[X_var,Y_var,1:1825]
                     var_7 <- array7[X_var,Y_var,1:1825]
                     var_8 <- array8[X_var,Y_var,1:1825]
                     
                     VAR <- c(var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8)
                     
                     #1
                     
                     X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                     Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_1 <- resampled_array[X_1,Y_1,1:14600]
                     
                     #2
                     
                     X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                     Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_2 <- resampled_array[X_2,Y_2,1:14600]
                     
                     #6
                     
                     X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                     Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_6 <- resampled_array[X_6,Y_6,1:14600]
                     
                     #7
                     
                     X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                     Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_7 <- resampled_array[X_7,Y_7,1:14600]
                     
                     #8
                     
                     X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                     Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_8 <- resampled_array[X_8,Y_8,1:14600]
                     
                     #9
                     
                     X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                     Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_9 <- resampled_array[X_9,Y_9,1:14600]
                     
                     
                     cal <- as.data.frame(VAR) 
                     
                     cal$cov_1 <- cov_1
                     cal$cov_2 <- cov_2
                     cal$cov_6 <- cov_6
                     cal$cov_7 <- cov_7
                     cal$cov_8 <- cov_8
                     cal$cov_9 <- cov_9
                     
                     cal <- cal[, colSums(is.na(cal)) == 0]
                     
                     
                     rf <- randomForest(VAR ~ cov_1 + cov_2 + cov_6 + cov_7 +cov_8 + cov_9, data = cal, ntree =30)
                     saveRDS(rf, model_name)
                     rm(rf, grid_lon, grid_lat, dist, row, var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8,
                        VAR, cov_1, cov_2, cov_6, cov_7, cov_8, cov_9, cal)
                     gc()
                     
                   }
                   
                   else if (Y1 == down){
                     
                     X_var <- round((grid_lon - lon[1]) / lon_res,0) + 1
                     Y_var <- as.double(round(lat_lenght - (grid_lat - tail(lat,1))/lat_res))
                     
                     var_1 <- array1[X_var,Y_var,1:1825]
                     var_2 <- array2[X_var,Y_var,1:1825]
                     var_3 <- array3[X_var,Y_var,1:1825]
                     var_4 <- array4[X_var,Y_var,1:1825]
                     var_5 <- array5[X_var,Y_var,1:1825]
                     var_6 <- array6[X_var,Y_var,1:1825]
                     var_7 <- array7[X_var,Y_var,1:1825]
                     var_8 <- array8[X_var,Y_var,1:1825]
                     
                     VAR <- c(var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8)
                     
                     #1
                     
                     X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                     Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_1 <- resampled_array[X_1,Y_1,1:14600]
                     
                     #4
                     
                     X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                     Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_4 <- resampled_array[X_4,Y_4,1:14600]
                     
                     #5
                     
                     X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                     Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_5 <- resampled_array[X_5,Y_5,1:14600]
                     
                     #6
                     
                     X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                     Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_6 <- resampled_array[X_6,Y_6,1:14600]
                     
                     #7
                     
                     X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                     Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_7 <- resampled_array[X_7,Y_7,1:14600]
                     
                     #8
                     
                     X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                     Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_8 <- resampled_array[X_8,Y_8,1:14600]
                     
                     
                     cal <- as.data.frame(VAR) 
                     
                     cal$cov_1 <- cov_1
                     cal$cov_4 <- cov_4
                     cal$cov_5 <- cov_5
                     cal$cov_6 <- cov_6
                     cal$cov_7 <- cov_7
                     cal$cov_8 <- cov_8
                     
                     cal <- cal[, colSums(is.na(cal)) == 0]
                    
                     
                     rf <- randomForest(VAR ~ cov_1 + cov_4 + cov_5 + cov_6 +cov_7 + cov_8, data = cal, ntree =30)
                     saveRDS(rf, model_name)
                     rm(rf, grid_lon, grid_lat, dist, row, var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8,
                        VAR, cov_1, cov_4, cov_5, cov_6, cov_7, cov_8, cal)
                     gc()
                     
                   }
                   
                   else if (X1 == left){
                     
                     X_var <- round((grid_lon - lon[1]) / lon_res,0) + 1
                     Y_var <- as.double(round(lat_lenght - (grid_lat - tail(lat,1))/lat_res))
                     
                     var_1 <- array1[X_var,Y_var,1:1825]
                     var_2 <- array2[X_var,Y_var,1:1825]
                     var_3 <- array3[X_var,Y_var,1:1825]
                     var_4 <- array4[X_var,Y_var,1:1825]
                     var_5 <- array5[X_var,Y_var,1:1825]
                     var_6 <- array6[X_var,Y_var,1:1825]
                     var_7 <- array7[X_var,Y_var,1:1825]
                     var_8 <- array8[X_var,Y_var,1:1825]
                     
                     VAR <- c(var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8)
                     
                     #1
                     
                     X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                     Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_1 <- resampled_array[X_1,Y_1,1:14600]
                     
                     #2
                     
                     X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                     Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_2 <- resampled_array[X_2,Y_2,1:14600]
                     
                     #3
                     
                     X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                     Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_3 <- resampled_array[X_3,Y_3,1:14600]
                     
                     #4
                     
                     X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                     Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_4 <- resampled_array[X_4,Y_4,1:14600]
                     
                     #5
                     
                     X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                     Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_5 <- resampled_array[X_5,Y_5,1:14600]
                     
                     #6
                     
                     X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                     Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_6 <- resampled_array[X_6,Y_6,1:14600]
                     
                     
                     cal <- as.data.frame(VAR) 
                     
                     cal$cov_1 <- cov_1
                     cal$cov_2 <- cov_2
                     cal$cov_3 <- cov_3
                     cal$cov_4 <- cov_4
                     cal$cov_5 <- cov_5
                     cal$cov_6 <- cov_6
                     
                     cal <- cal[, colSums(is.na(cal)) == 0]
                     
                     rf <- randomForest(VAR ~ cov_1 + cov_2 + cov_3 + cov_4 +cov_5 + cov_66, data = cal, ntree =30)
                     saveRDS(rf, model_name)
                     rm(rf, grid_lon, grid_lat, dist, row, var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8,
                        VAR, cov_1, cov_2, cov_3, cov_4, cov_5, cov_6, cal)
                     gc()
                     
                   }
                     
                   else{
                     
                     X_var <- round((grid_lon - lon[1]) / lon_res,0) + 1
                     Y_var <- as.double(round(lat_lenght - (grid_lat - tail(lat,1))/lat_res))
                     
                     var_1 <- array1[X_var,Y_var,1:1825]
                     var_2 <- array2[X_var,Y_var,1:1825]
                     var_3 <- array3[X_var,Y_var,1:1825]
                     var_4 <- array4[X_var,Y_var,1:1825]
                     var_5 <- array5[X_var,Y_var,1:1825]
                     var_6 <- array6[X_var,Y_var,1:1825]
                     var_7 <- array7[X_var,Y_var,1:1825]
                     var_8 <- array8[X_var,Y_var,1:1825]
                     
                     VAR <- c(var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8)
                     
                     #1
                     
                     X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                     Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_1 <- resampled_array[X_1,Y_1,1:14600]
                     
                     #2
                     
                     X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                     Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_2 <- resampled_array[X_2,Y_2,1:14600]
                     
                     #3
                     
                     X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                     Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_3 <- resampled_array[X_3,Y_3,1:14600]
                     
                     #4
                     
                     X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                     Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_4 <- resampled_array[X_4,Y_4,1:14600]
                     
                     #5
                     
                     X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                     Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_5 <- resampled_array[X_5,Y_5,1:14600]
                     
                     #6
                     
                     X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                     Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_6 <- resampled_array[X_6,Y_6,1:14600]
                     
                     #7
                     
                     X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                     Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_7 <- resampled_array[X_7,Y_7,1:14600]
                     
                     #8
                     
                     X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                     Y_8 <- as.double(round(guide_lat_length - abs((Y8 - tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_8 <- resampled_array[X_8,Y_8,1:14600]
                     
                     #9
                     
                     X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                     Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                     
                     cov_9 <- resampled_array[X_9,Y_9,1:14600]
                     
                     
                     
                     cal <- as.data.frame(VAR) 
                     
                     cal$cov_1 <- cov_1
                     cal$cov_2 <- cov_2
                     cal$cov_3 <- cov_3
                     cal$cov_4 <- cov_4
                     cal$cov_5 <- cov_5
                     cal$cov_6 <- cov_6
                     cal$cov_7 <- cov_7
                     cal$cov_8 <- cov_8
                     cal$cov_9 <- cov_9
                     
                     cal <- cal[, colSums(is.na(cal)) == 0]
                     
                     rf <- randomForest(VAR ~ cov_1 + cov_2 + cov_3 + cov_4 +cov_5 + cov_6 + cov_7 + cov_8 +cov_9, data = cal, ntree =30)
                     saveRDS(rf, model_name)
                     rm(rf, grid_lon, grid_lat, dist, row, var_1, var_2, var_3, var_4, var_5, var_6, var_7,var_8,
                        VAR, cov_1, cov_2, cov_3, cov_4, cov_5, cov_6, cov_7, cov_8, cov_9, cal)
                     gc()
                   }
                 }
               }
                        
    }
  }
}  
  
