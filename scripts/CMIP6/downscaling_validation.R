#########################################
# Author - Andre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to downscaling GCM data.
# this is specific for the validation process, it downscale 10-year external data.

################################################################
# 
# To make the process faster, we separated the the training of the from the downscaling. 
# I.e. models are trained in previous stsep and exported (saved). Here, 
# models are loaded and used for downscaling.
# 
# Note: I realized that this script could be a lot shorter after I finished it, 
# I will update it when i have a chance.
#
#
################################################################


#packages
library(ncdf4)
#library(cmsaf)
library(dplyr)
library(randomForest)
library(Metrics)
library(doParallel)
library(spam)
library(foreign)


models <- read.csv("~/Documents/scripts/cmip6/2023/models_2023.csv") #has all the other information necessary to create the HTTPs
variables <- read.csv("~/Documents/scripts/cmip6/2023/Variables.csv")
grid <- read.dbf("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/grid/master_pixels.dbf")
vars <- c("pr", "tasmax", "tasmin")
files <- read.csv("~/Documents/scripts/cmip6/2023/files_POC.csv")

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

for (v in 2){
  
  var <- paste(variables[v,3])
  variable <-  paste(variables[v,2])
  print(var)
  
  
  for(m in 17) { #2 and 17
    
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
    
    nc_resampled <- nc_open(paste0("/scratch/general/vast/u6047395/prism/resampled/prism_",var,"_day_",model,"_resampled.nc"))
    resampled_array <- ncvar_get(nc_resampled, var)
    rm(nc_resampled) 
    
    f=1
    for (f in 1:length(files[,1])){
      
      dates <- files[f,5]
      start <- files[f,3]
      finish <- files[f,4]
      length_1 <- files[f,2]
      SD <- files[f,6]
      print(Sys.time())
      print(dates)
      
      nc_name <- paste0("/scratch/general/vast/u6047395/cmip6/POC/dc_models/CLIMAVA_validation_",var,"_day_",model,"_",dates,".nc")
      
      tem <- file.exists(nc_name)
      print(tem)
      
      if (tem == FALSE){
        
        ############################################################################################################
        
        registerDoParallel(20)
        
        i=20748
        data <- foreach (i = 1:158710, 
                         .packages = c("spam","doParallel","foreach","ncdf4","dplyr", "randomForest", "Metrics")) %dopar% {
                           
                           IN_OUT <- grid[i,4]
                           
                           if (IN_OUT == 0){
                             
                             pixel <- rep(-9999, length_1)
                             
                           }
                           
                           if (IN_OUT == 1){
                             
                             ID = i
                             print(ID)
                             model_name <- paste0("/scratch/general/vast/u6047395/cmip6/POC/rf_models/",model,"/",var,"/rf_",ID,"_",model,"_",var,".rds")
                             
                             
                             tem <- file.exists(model_name)
                             print(tem)
                             
                             if (tem == TRUE){
                               
                               grid_lon = grid[i,2]
                               grid_lat = grid[i,3]
                               
                               
                               dist <- guide
                               dist$dist <- sqrt((dist$lon_flip-grid_lon)^2+(dist$lat-grid_lat)^2)
                               row <- dist[which.min(dist$dist),]
                               
                               
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
                               
                               if (X1 == left & Y1 == up){
                                 
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- resampled_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- resampled_array[X_2,Y_2,start:finish]
                                 
                                 #3
                                 
                                 X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                                 Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_3 <- resampled_array[X_3,Y_3,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- resampled_array[X_4,Y_4,start:finish]
                                 
                                 
                                 
                                 cal <- as.data.frame(cov_1) # this cal is the oserved data
                                 
                                 cal$cov_1 <- cov_1
                                 cal$cov_2 <- cov_2
                                 cal$cov_3 <- cov_3
                                 cal$cov_4 <- cov_4
                                 
                                 cal <- cal[, colSums(is.na(cal)) == 0]
                                 
                                 rf <- readRDS(model_name)
                                 pred <- predict(rf,cal)
                                 pred[pred <= 0] <- 0
                                 pixel <- as.vector(round(pred,1))
                                 
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_3, cov_4, cal)
                               }
                               
                               else if (X1 == right & Y1 == up){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- resampled_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- resampled_array[X_2,Y_2,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- resampled_array[X_8,Y_8,start:finish]
                                 
                                 #9
                                 
                                 X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                                 Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_9 <- resampled_array[X_9,Y_9,start:finish]
                                 
                                 
                                 
                                 cal <- as.data.frame(cov_1) # this cal is the oserved data
                                 
                                 cal$cov_1 <- cov_1
                                 cal$cov_2 <- cov_2
                                 cal$cov_8 <- cov_8
                                 cal$cov_9 <- cov_9
                                 
                                 cal <- cal[, colSums(is.na(cal)) == 0]
                                 
                                 rf <- readRDS(model_name)
                                 pred <- predict(rf,cal)
                                 pred[pred <= 0] <- 0
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_8, cov_9, cal)
                               }
                               
                               else if (X1 == right & Y1 == down){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- resampled_array[X_1,Y_1,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- resampled_array[X_6,Y_6,start:finish]
                                 
                                 #7
                                 
                                 X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                                 Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_7 <- resampled_array[X_7,Y_7,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- resampled_array[X_8,Y_8,start:finish]
                                 
                                 
                                 
                                 cal <- as.data.frame(cov_1) # this cal is the oserved data
                                 
                                 cal$cov_1 <- cov_1
                                 cal$cov_6 <- cov_6
                                 cal$cov_7 <- cov_7
                                 cal$cov_8 <- cov_8
                                 
                                 cal <- cal[, colSums(is.na(cal)) == 0]
                                 
                                 rf <- readRDS(model_name)
                                 pred <- predict(rf,cal)
                                 pred[pred <= 0] <- 0
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_6, cov_7, cov_8, cal)
                               }
                               
                               else if (Y1 == up){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- resampled_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- resampled_array[X_2,Y_2,start:finish]
                                 
                                 #3
                                 
                                 X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                                 Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_3 <- resampled_array[X_3,Y_3,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- resampled_array[X_4,Y_4,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- resampled_array[X_8,Y_8,start:finish]
                                 
                                 #9
                                 
                                 X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                                 Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_9 <- resampled_array[X_9,Y_9,start:finish]
                                 
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
                                 pred[pred <= 0] <- 0
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_3, cov_4, cov_8, cov_9, cal)
                               }
                               
                               else if (X1 == right){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- resampled_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- resampled_array[X_2,Y_2,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- resampled_array[X_6,Y_6,start:finish]
                                 
                                 #7
                                 
                                 X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                                 Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_7 <- resampled_array[X_7,Y_7,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- resampled_array[X_8,Y_8,start:finish]
                                 
                                 #9
                                 
                                 X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                                 Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_9 <- resampled_array[X_9,Y_9,start:finish]
                                 
                                 
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
                                 pred[pred <= 0] <- 0
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_6, cov_7, cov_8, cov_9, cal)
                               }
                               
                               else if (Y1 == down){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- resampled_array[X_1,Y_1,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- resampled_array[X_4,Y_4,start:finish]
                                 
                                 #5
                                 
                                 X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                                 Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_5 <- resampled_array[X_5,Y_5,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- resampled_array[X_6,Y_6,start:finish]
                                 
                                 #7
                                 
                                 X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                                 Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_7 <- resampled_array[X_7,Y_7,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- resampled_array[X_8,Y_8,start:finish]
                                 
                                 
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
                                 pred[pred <= 0] <- 0
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_4, cov_5, cov_6, cov_7, cov_8, cal)
                               }
                               
                               else if (X1 == left){
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- resampled_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- resampled_array[X_2,Y_2,start:finish]
                                 
                                 #3
                                 
                                 X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                                 Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_3 <- resampled_array[X_3,Y_3,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- resampled_array[X_4,Y_4,start:finish]
                                 
                                 #5
                                 
                                 X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                                 Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_5 <- resampled_array[X_5,Y_5,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- resampled_array[X_6,Y_6,start:finish]
                                 
                                 
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
                                 pred[pred <= 0] <- 0
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_3, cov_4, cov_5, cov_6, cal)
                               }
                               
                               else{
                                 
                                 #1
                                 
                                 X_1 <- round((X1 - left) / guide_lon_res,0) + 1 
                                 Y_1 <- as.double(round(guide_lat_length - abs((Y1 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_1 <- resampled_array[X_1,Y_1,start:finish]
                                 
                                 #2
                                 
                                 X_2 <- round((X2 - left) / guide_lon_res,0) + 1 
                                 Y_2 <- as.double(round(guide_lat_length - abs((Y2 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_2 <- resampled_array[X_2,Y_2,start:finish]
                                 
                                 #3
                                 
                                 X_3 <- round((X3 - left) / guide_lon_res,0) + 1 
                                 Y_3 <- as.double(round(guide_lat_length - abs((Y3 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_3 <- resampled_array[X_3,Y_3,start:finish]
                                 
                                 #4
                                 
                                 X_4 <- round((X4 - left) / guide_lon_res,0) + 1 
                                 Y_4 <- as.double(round(guide_lat_length - abs((Y4 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_4 <- resampled_array[X_4,Y_4,start:finish]
                                 
                                 #5
                                 
                                 X_5 <- round((X5 - left) / guide_lon_res,0) + 1 
                                 Y_5 <- as.double(round(guide_lat_length - abs((Y5 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_5 <- resampled_array[X_5,Y_5,start:finish]
                                 
                                 #6
                                 
                                 X_6 <- round((X6 - left) / guide_lon_res,0) + 1 
                                 Y_6 <- as.double(round(guide_lat_length - abs((Y6 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_6 <- resampled_array[X_6,Y_6,start:finish]
                                 
                                 #7
                                 
                                 X_7 <- round((X7 - left) / guide_lon_res,0) + 1 
                                 Y_7 <- as.double(round(guide_lat_length - abs((Y7 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_7 <- resampled_array[X_7,Y_7,start:finish]
                                 
                                 #8
                                 
                                 X_8 <- round((X8 - left) / guide_lon_res,0) + 1 
                                 Y_8 <- as.double(round(guide_lat_length - abs((Y8 - tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_8 <- resampled_array[X_8,Y_8,start:finish]
                                 
                                 #9
                                 
                                 X_9 <- round((X9 - left) / guide_lon_res,0) + 1 
                                 Y_9 <- as.double(round(guide_lat_length - abs((Y9- tail(guide_lat,1))/guide_lat_res)))
                                 
                                 cov_9 <- resampled_array[X_9,Y_9,start:finish]
                                 
                                 
                                 
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
                                 pred[pred <= 0] <- 0
                                 pixel <- as.vector(round(pred,1))
                                 
                                 rm(rf, grid_lon, grid_lat, dist, row, cov_1, cov_2, cov_3, cov_4, cov_5, cov_6, cov_7, cov_8, cov_9, cal)
                               }
                             }
                           }
                           cbind(pixel)
                         }
        
        # creating NetCDF
        data <- as.data.frame(data)
        rownames(data) <- as.character(1:length(data[,1]))
        colnames(data) <- as.character(1:length(data))
        data <- t(data)
        
        # defining dimensions
        LON_n <- length(unique(grid$lon)) 
        LAT_n <- length(unique(grid$lat))
        TIME_n <- length_1 
        
        # creating array
        data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
        
        # naming dimensions
        dim_name <- variables[v,3]
        dim_long_name <- variables[v,5]
        dim_units <- variables[v,7]
        
        # defining dimensions
        lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(grid$lon))
        lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(grid$lat))
        time_dim <- ncdim_def("time", units = "days", longname = "days since start", vals = seq(1,length_1,1))
        
        # variable dimensions
        variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                  missval =  -9999,longname = dim_long_name, prec = "double", compression = 9)
        
        # creating empty NetCDF 
        nc_out <- nc_create(nc_name,variable_dim)
        
        # adding variable to NetCDF
        ncvar_put(nc_out, variable_dim, data_array)
        
        #adding global metadata to NetCDF
        ncatt_put(nc_out, 0, "Name", paste0("CLIMAVA validation at ",model," model resolution"),"character")
        ncatt_put(nc_out, 0, "Version", "1.0")
        ncatt_put(nc_out, 0, "Author", "Andre Geraldo de Lima Moraes","character")
        ncatt_put(nc_out, 0, "Institution", "Utah State University, Watershed Sciences Department","character")
        ncatt_put(nc_out, 0, "Adress", "5210 Old Main Hill, NR 210, Logan, UT 84322","character")
        ncatt_put(nc_out, 0, "email", "andre.moraes@usu.edu","character")
        ncatt_put(nc_out, 0, "Description", paste("This is the validation data set for CLIMAVA-SW from ",model," model resolution"),"character")
        ncatt_put(nc_out, 0, "lineage", "Parameter-elevation Relationships on Independent The Slopes Model (PRISM 4K) project (https://prism.oregonstate.edu/)", "character")
        ncatt_put(nc_out, 0, "License", "CCO 1.0 Universal","character")
        ncatt_put(nc_out, 0, "fees", "This data set is free","character")
        ncatt_put(nc_out, 0, "Disclaimer", "While every effort has been made to ensure the accuracy and completeness of the data, no guarantee is given that the information provided is error-free or that the dataset will be suitable for any particular purpose. Users are advised to use this dataset with caution and to independently verify the data before making any decisions based on it. The creators of this dataset make no warranties, express or implied, regarding the dataset's accuracy, reliability, or fitness for a particular purpose. In no event shall the creators be liable for any damages, including but not limited to direct, indirect, incidental, special, or consequential damages, arising out of the use or inability to use the dataset. Users of this dataset are encouraged to properly cite the dataset in any publications or works that make use of the data. By using this dataset, you agree to these terms and conditions. If you do not agree with these terms, please do not use the dataset.","character")
        
        # closing NetCDF
        nc_close(nc_out)
        
        rm(data, lon_dim, lat_dim,time_dim, variable_dim, nc_out)
        
        
        
      }
    }
  }  
}
