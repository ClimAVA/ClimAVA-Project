#########################################
# Author - Andre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu
#
# This script is used to for downscaling GCM data

################################################################
# 
# To make the process faster, we separated the the training of the form the downscaling. 
# I.e. models are trained in another script (job) and exported (saved). 
# Here we load those models and do the downscaling. 
# By doing it this way the process in many times faster.
# Also note that compression and addition of metadata is done in another step,
# parallelizing the compression is much more efficient than doing it here
# 
# Note: I realized that this script could be a lot shorter after I finished it. 
# I will update it when I have a chance. 
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

#loading data frames used to guide the process
models <- read.csv("~/Documents/scripts/cmip6/2023/models_2023.csv") #has all the other information necessary to create the HTTPs
variables <- read.csv("~/Documents/scripts/cmip6/2023/Variables.csv")
grid <- read.dbf("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/grid/master_pixels.dbf")
vars <- c("pr", "tasmax", "tasmin")
ssps <- c("historical","ssp245","ssp370","ssp585")
vars <- c("pr", "tasmax", "tasmin")

#########################################################################
#####Creating directories--------

# getwd()
# setwd("/scratch/general/vast/u6047395/cmip6/POC/rf_models/")
# for (m in c(2,17)){
#   model = models[m,1]
#   dir.create(model)
# }
# 
# for (m in c(2,17)){
#   model = models[m,1]
#   for (i in 1:3){
#     v = vars[i]
#     dir = paste0(model,"/",v)
#     dir.create(dir)
#   }
# }

####################################################################
# loop trough variables
for (v in 1:3){
  
  var <- paste(variables[v,3])
  variable <-  paste(variables[v,2])
  print(var)
  
  # loop through scenarios
  for (s in 1:4){
    
    ssp = ssps[s]
    print(ssp)
    
    #loading guides with files start and end date and other information
    if(ssp == "historical") {files <- read.csv("~/Documents/scripts/cmip6/2023/files_DS_hist.csv")}# loading guide for files
    if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {files <- read.csv("~/Documents/scripts/cmip6/2023/files_DS_future.csv")}
  
    # loop trough models
    for(m in 1:17) { #2 and 17
      
      model = models[m,1]
      print(model)
      guide <- read.dbf(paste0("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/grids/subsets/",model,"_guide.dbf"))
      
      guide_lat_res <- models[m,24]
      guide_lon_res <- models[m,25]
      guide_lat_length <- length(unique(guide$lat))
      guide_lon_length <- length(unique(guide$lon))
      guide_lat <- unique(guide$lat)
      
      up <- max(guide$lat)
      down <- min(guide$lat)
      left <- min(guide$lon_flip)
      right <- max(guide$lon_flip)
      
      # loading bias corrected data
      nc_bc <- nc_open(paste0("/scratch/general/vast/u6047395/cmip6/bc/",ssp,"_",var,"_day_",model,"_BC.nc"))
      bc_array <- ncvar_get(nc_bc, var)
      rm(nc_bc) 
      
      # loop through each file to be created
      for (f in 1: length(files[,1])){
        
        dates <- files[f,5]
        start <- files[f,3]
        finish <- files[f,4]
        length_1 <- files[f,2]
        SD <- files[f,6]
        print(Sys.time())
        print(dates)
        
        # defining the name of the file
        nc_name <- paste0("/scratch/general/vast/u6047395/cmip6/DS/",model,"/",ssp,"/",var,"/SWCP4K",model,"_",ssp,"_",var,"_",dates,".nc")
        
        # testing if file was already created
        tem <- file.exists(nc_name)
        print(tem)
        
        if (tem == FALSE){
        
        ############################################################################################################
        
        # defining dopar paramenters  
        registerDoParallel(20)
        
        # downscaling is done for each pixel
        data <- foreach (i = 1:158710, 
                         .packages = c("spam","doParallel","foreach","ncdf4","dplyr", "randomForest", "Metrics")) %dopar% {
                           
                           IN_OUT <- grid[i,4]
                           
                           if (IN_OUT == 0){
                             
                             pixel <- rep(-9999, length_1)
                             
                           }
                           
                           if (IN_OUT == 1){
                             
                             ID = i
                             # RF model name for that specific pixel
                             model_name <- paste0("/scratch/general/vast/u6047395/cmip6/rf_models/",model,"/",var,"/_rf_",ID,"_",model,"_",var,".rds")
                             
                             
                             exists <- file.exists(model_name)
                             
                             if (exists == TRUE){ # only continue if RF model exists
                               
                               grid_lon = grid[i,2]
                               grid_lat = grid[i,3]
                               
                               # finding the resampled (coarse) pixel the is closest to the selected (fine) pixel to be downcaled
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
                               
                               if (X1 == left & Y1 == up){
                                 
                                 
                                 #1 Extracting center predictor
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- bc_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- bc_array[X_2,Y_2,start:finish]
                                 
                                 #3
                                 
                                 X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                                 Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_3 <- bc_array[X_3,Y_3,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- bc_array[X_4,Y_4,start:finish]
                                 
                                 
                                 # creating a data frame with all predictors
                                 cal <- as.data.frame(cov_1) 
                                 
                                 cal$cov_1 <- cov_1
                                 cal$cov_2 <- cov_2
                                 cal$cov_3 <- cov_3
                                 cal$cov_4 <- cov_4
                                 
                                 #removing colmns with NA
                                 cal <- cal[, colSums(is.na(cal)) == 0]
                                 
                                 rf <- readRDS(model_name) # loading RF model
                                 pred <- predict(rf,cal) # running the RF model
                                 if(var == "pr") {pred[pred < 0] <- 0} # removing any negative precipitation (for safety)
                                 pixel <- as.vector(round(pred,1)) # creting the vector with downcaled data for the specific pixel.
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_3, cov_4, cal, pred)
                               }
                               
                               else if (X1 == right & Y1 == up){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- bc_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- bc_array[X_2,Y_2,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- bc_array[X_8,Y_8,start:finish]
                                 
                                 #9
                                 
                                 X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                                 Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_9 <- bc_array[X_9,Y_9,start:finish]
                                 
                                 
                                 
                                 cal <- as.data.frame(cov_1) # this cal is the oserved data
                                 
                                 cal$cov_1 <- cov_1
                                 cal$cov_2 <- cov_2
                                 cal$cov_8 <- cov_8
                                 cal$cov_9 <- cov_9
                                 
                                 cal <- cal[, colSums(is.na(cal)) == 0]
                                 
                                 rf <- readRDS(model_name)
                                 pred <- predict(rf,cal)
                                 if(var == "pr") {pred[pred < 0] <- 0}
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_8, cov_9, cal, pred)
                               }
                               
                               else if (X1 == right & Y1 == down){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- bc_array[X_1,Y_1,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- bc_array[X_6,Y_6,start:finish]
                                 
                                 #7
                                 
                                 X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                                 Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_7 <- bc_array[X_7,Y_7,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- bc_array[X_8,Y_8,start:finish]
                                 
                                 
                                 
                                 cal <- as.data.frame(cov_1) # this cal is the oserved data
                                 
                                 cal$cov_1 <- cov_1
                                 cal$cov_6 <- cov_6
                                 cal$cov_7 <- cov_7
                                 cal$cov_8 <- cov_8
                                 
                                 cal <- cal[, colSums(is.na(cal)) == 0]
                                 
                                 rf <- readRDS(model_name)
                                 pred <- predict(rf,cal)
                                 if(var == "pr") {pred[pred < 0] <- 0}
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_6, cov_7, cov_8, cal, pred)
                               }
                               
                               else if (Y1 == up){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- bc_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- bc_array[X_2,Y_2,start:finish]
                                 
                                 #3
                                 
                                 X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                                 Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_3 <- bc_array[X_3,Y_3,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- bc_array[X_4,Y_4,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- bc_array[X_8,Y_8,start:finish]
                                 
                                 #9
                                 
                                 X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                                 Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_9 <- bc_array[X_9,Y_9,start:finish]
                                 
                                 cal <- as.data.frame(cov_1) # this cal is the oserved data
                                 
                                 cal$cov_1 <- cov_1
                                 cal$cov_2 <- cov_2
                                 cal$cov_3 <- cov_3
                                 cal$cov_4 <- cov_4
                                 cal$cov_8 <- cov_8
                                 cal$cov_9 <- cov_9
                                 
                                 cal <- cal[, colSums(is.na(cal)) == 0]
                                 
                                 
                                 rf <- readRDS(model_name)
                                 pred <- predict(rf,cal)
                                 if(var == "pr") {pred[pred < 0] <- 0}
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_3, cov_4, cov_8, cov_9, cal, pred)
                               }
                               
                               else if (X1 == right){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- bc_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- bc_array[X_2,Y_2,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- bc_array[X_6,Y_6,start:finish]
                                 
                                 #7
                                 
                                 X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                                 Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_7 <- bc_array[X_7,Y_7,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- bc_array[X_8,Y_8,start:finish]
                                 
                                 #9
                                 
                                 X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                                 Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_9 <- bc_array[X_9,Y_9,start:finish]
                                 
                                 
                                 cal <- as.data.frame(cov_1) # this cal is the oserved data
                                 
                                 cal$cov_1 <- cov_1
                                 cal$cov_2 <- cov_2
                                 cal$cov_6 <- cov_6
                                 cal$cov_7 <- cov_7
                                 cal$cov_8 <- cov_8
                                 cal$cov_9 <- cov_9
                                 
                                 cal <- cal[, colSums(is.na(cal)) == 0]
                                 
                                 rf <- readRDS(model_name)
                                 pred <- predict(rf,cal)
                                 if(var == "pr") {pred[pred < 0] <- 0}
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_6, cov_7, cov_8, cov_9, cal, pred)
                               }
                               
                               else if (Y1 == down){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- bc_array[X_1,Y_1,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- bc_array[X_4,Y_4,start:finish]
                                 
                                 #5
                                 
                                 X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                                 Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_5 <- bc_array[X_5,Y_5,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- bc_array[X_6,Y_6,start:finish]
                                 
                                 #7
                                 
                                 X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                                 Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_7 <- bc_array[X_7,Y_7,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- bc_array[X_8,Y_8,start:finish]
                                 
                                 
                                 cal <- as.data.frame(cov_1) # this cal is the oserved data
                                 
                                 cal$cov_1 <- cov_1
                                 cal$cov_4 <- cov_4
                                 cal$cov_5 <- cov_5
                                 cal$cov_6 <- cov_6
                                 cal$cov_7 <- cov_7
                                 cal$cov_8 <- cov_8
                                 
                                 cal <- cal[, colSums(is.na(cal)) == 0]
                                 
                                 rf <- readRDS(model_name)
                                 pred <- predict(rf,cal)
                                 if(var == "pr") {pred[pred < 0] <- 0}
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_4, cov_5, cov_6, cov_7, cov_8, cal, pred)
                               }
                               
                               else if (X1 == left){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- bc_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- bc_array[X_2,Y_2,start:finish]
                                 
                                 #3
                                 
                                 X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                                 Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_3 <- bc_array[X_3,Y_3,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- bc_array[X_4,Y_4,start:finish]
                                 
                                 #5
                                 
                                 X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                                 Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_5 <- bc_array[X_5,Y_5,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- bc_array[X_6,Y_6,start:finish]
                                 
                                 
                                 cal <- as.data.frame(cov_1) # this cal is the oserved data
                                 
                                 cal$cov_1 <- cov_1
                                 cal$cov_2 <- cov_2
                                 cal$cov_3 <- cov_3
                                 cal$cov_4 <- cov_4
                                 cal$cov_5 <- cov_5
                                 cal$cov_6 <- cov_6
                                 
                                 cal <- cal[, colSums(is.na(cal)) == 0]
                                 
                                 rf <- readRDS(model_name)
                                 pred <- predict(rf,cal)
                                 if(var == "pr") {pred[pred < 0] <- 0}
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_3, cov_4, cov_5, cov_6, cal, pred)
                               }
                               
                               else{
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- bc_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- bc_array[X_2,Y_2,start:finish]
                                 
                                 #3
                                 
                                 X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                                 Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_3 <- bc_array[X_3,Y_3,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- bc_array[X_4,Y_4,start:finish]
                                 
                                 #5
                                 
                                 X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                                 Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_5 <- bc_array[X_5,Y_5,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- bc_array[X_6,Y_6,start:finish]
                                 
                                 #7
                                 
                                 X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                                 Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_7 <- bc_array[X_7,Y_7,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- bc_array[X_8,Y_8,start:finish]
                                 
                                 #9
                                 
                                 X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                                 Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_9 <- bc_array[X_9,Y_9,start:finish]
                                 
                                 
                                 
                                 cal <- as.data.frame(cov_1) # this cal is the oserved data
                                 
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
                                 
                                 
                                 rf <- readRDS(model_name)
                                 pred <- predict(rf,cal)
                                 if(var == "pr") {pred[pred < 0] <- 0}
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_3, cov_4, cov_5, cov_6, cov_7, cov_8, cov_9, cal, pred)
                               }
                             }
                           }
                           cbind(pixel) # binding all pixels
                           #gc()
                         }
        
        #Creating the NetCDF
        data <- as.data.frame(data)
        rownames(data) <- as.character(1:length(data[,1]))
        colnames(data) <- as.character(1:length(data))
        data <- t(data)
        
        # defining dimension
        LON_n <- length(unique(grid$lon)) 
        LAT_n <- length(unique(grid$lat))
        TIME_n <- length_1 
        
        # creating array
        data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
        
        rm(data) # cleaning memory
        
        # anming dimensions
        dim_name <- variables[v,3]
        dim_long_name <- variables[v,5]
        dim_units <- variables[v,6]
        
        ##defining dimensions
        lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(grid$lon))
        lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(grid$lat))
        time_dim <- ncdim_def("time", units = "days", longname = "days since start", vals = seq(1,length_1,1))
        
        # defining variable
        variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                  missval =  -9999,longname = dim_long_name, prec = "double") # compression is done in another step
        
        # creating empty NetCDF
        nc_out <- nc_create(nc_name,variable_dim)
        
        # adding variables to the NetCDF
        ncvar_put(nc_out, variable_dim, data_array)
        
        ############ additional Meta data is added in a another step #########
        
        #closing NetCDF
        nc_close(nc_out)
        
        # Cleaning memory
        rm(data_array, LON_n, LAT_n, TIME_n, dim_name, dim_long_name, dim_units, lon_dim, lat_dim,time_dim, variable_dim, nc_out)
        gc()
        
        }
      }
    }
  }  
}
