#########################################
# Author - Andre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to bias corrected GCM precipitation data by Quantile mapping 

################################################################

setwd("/scratch/general/vast/u6047395/cmip6/bc/")
getwd()

#packages
library(sp)
library(ncdf4)
library(dplyr)
library(randomForest)
library(Metrics)
library(doParallel)
library(spam)
library(foreign)

# dopar parameters
registerDoParallel(50)

#loading guides
models <- read.csv("~/Documents/scripts/cmip6/2023/models_2023.csv") #has all the other information necessary to create the HTTPs
variables <- read.csv("~/Documents/scripts/cmip6/2023/Variables.csv")
ssps <- c("historical","ssp245","ssp370","ssp585")
vars <- c("pr", "tasmax", "tasmin")

#################################

for (v in 1){ # only pr
  
  var <- paste(variables[v,3])
  print(var)
  
  for(m in 1:17) {
    
    model = models[m,1]
    print(model)
    guide <- read.dbf(paste0("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/grids/subsets/",model,"_guide.dbf"))
    realization <- models[m,4]
    
    
    nc1 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/resampled/prism_",var,"_day_",model,"_resampled.nc"))
    
    lon <- unique(ncvar_get(nc1, "lon")) # all longitudes
    lon_res <- abs(lon[1] - lon[2]) # lon resolution
    lat_lenght <- length(ncvar_get(nc1, "lat")) 
    lat <- unique(ncvar_get(nc1, "lat"))
    lat_res <- abs(lat[1] - lat[2])
    
    prism_array <- ncvar_get(nc1, var)
    rm(nc1)
    
    nc_hist <- nc_open(paste0("/scratch/general/vast/u6047395/cmip6/cmip6_subset/",model,"/historical/",var,"/",var,"_day_",model,"_",realization,"_historical_subset.nc"))
    hist_array <- ncvar_get(nc_hist, var)
    rm(nc_hist)
    
    for (s in 1){
      
      ssp = ssps[s]
      print(ssp)
      
      nc <- nc_open(paste0("/scratch/general/vast/u6047395/cmip6/cmip6_subset/",model,"/", ssp,"/",var,"/",var,"_day_",model,"_",realization,"_", ssp,"_subset.nc"))
      to_bc_array <- ncvar_get(nc, var)
      
      
      if (ssp == "historical"){
        
        data <- foreach (i = 1: length(guide[,1]), .combine=cbind, .multicombine = T,
                         .packages = c("doParallel","foreach","ncdf4","dplyr", "Metrics")) %dopar% {
                           
                           guide_lon <- guide[i,4] # get central coordinates (lon flip)
                           guide_lat <- guide[i,2] # get central coordinates
                           
                           X <- round((guide_lon - lon[1]) / lon_res,0) + 1 
                           Y <- as.double(round(lat_lenght - abs((guide_lat - tail(lat,1))/lat_res))) 
                           
                           prism_vector <- prism_array[X,Y,1:12410] #1981 - 2014
                           
                           hist_vector <-  hist_array[X,Y,366:12775] #1981 - 2014
                           hist_vector <- (round((hist_vector *86400),1))
                           
                           if (is.na(prism_vector[1]) == TRUE) {
                             
                             pixel = rep(-9999, 12410)
                             
                           }
                           else{
                             
                             to_bc_vector <-  to_bc_array[X,Y,366:12775] #1981 - 2014 - vector to be bias corrected
                             to_bc_vector <- (round((to_bc_vector *86400),1)) # converting units
                             
                             
                             prism_cdf <- ecdf(prism_vector) # calculating Prism CDF
                             hist_cdf <- ecdf(hist_vector) #calculating hist CDF
                             
                             probability <- hist_cdf(to_bc_vector) # getting probabilities for variables to be corrected  based on the historical CDF
                             pixel <- quantile(prism_cdf, probability) # using probabilities to get bias corrected values from the Prism cdf.
                             
                           }
                           
                           cbind(pixel)
                           
                         }
        
        # building the NetCDF
        data <- as.data.frame(data)
        rownames(data) <- as.character(1:length(data[,1]))
        colnames(data) <- as.character(1:length(data))
        data <- t(data)
        
        # defining dimensions
        LON_n <- length(unique(guide$lon_flip)) 
        LAT_n <- length(unique(guide$lat))
        TIME_n <- 12410 
        
        # creating array
        data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
        
        # naming
        nc_name <- paste0(ssp,"_",var,"_day_",model,"_BC.nc")
        dim_name <- variables[v,3]
        dim_long_name <- variables[v,5]
        dim_units <- variables[v,7]
        
        ##defining dimensions
        lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon_flip))
        lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
        time_dim <- ncdim_def("time", units = "days", longname = "days since sstart", vals = seq(1,12410,1))
        
        # variable dimensions
        variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                  missval =  -9999,longname = dim_long_name, prec = "double")
        
        #creating empty NetCDF
        nc_out <- nc_create(nc_name,variable_dim)
        
        #adding variable to NetCDF
        ncvar_put(nc_out, variable_dim, data_array)
        
        # closing NetCDF file
        nc_close(nc_out)
        
      }
      
      if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585"){
        
        data <- foreach (i = 1: length(guide[,1]), .combine=cbind, .multicombine = T,
                         .packages = c("spam","doParallel","foreach","ncdf4","dplyr", "Metrics")) %dopar% {
                           
                           guide_lon <- guide[i,4] # get central coordinates (lon flip)
                           guide_lat <- guide[i,2] # get central coordinates
                           
                           X <- round((guide_lon - lon[1]) / lon_res,0) + 1 
                           Y <- as.double(round(lat_lenght - abs((guide_lat - tail(lat,1))/lat_res))) 
                           
                           prism_vector <- prism_array[X,Y,1:12410] #1981 - 2014
                           
                           hist_vector <-  hist_array[X,Y,366:12775] #1981 - 2014
                           hist_vector <- (round((hist_vector *86400),1))
                           
                           if (is.na(prism_vector[1]) == TRUE) {
                             
                             pixel = rep(-9999, 31390)
                             
                           }
                           else{
                             
                             to_bc_vector <-  to_bc_array[X,Y,1:31390] #2015 - 2100
                             to_bc_vector <- (round((to_bc_vector *86400),1)) # converting units
                             
                             
                             prism_cdf <- ecdf(prism_vector) # calculating Prism CDF
                             hist_cdf <- ecdf(hist_vector) #calculating Prism CDF
                             
                             probability <- hist_cdf(to_bc_vector) # getting probabilities for variables to be corrected  based on the historical CDF
                             pixel <- quantile(prism_cdf, probability) # using probabilities to get bias corrected values from the Prism CDF.
                             
                           }
                           
                           cbind(pixel)
                           
                         }
        
        # creating NetCDF
        data <- as.data.frame(data)
        rownames(data) <- as.character(1:length(data[,1]))
        colnames(data) <- as.character(1:length(data))
        data <- t(data)
        
        # NetCDF dimensions
        LON_n <- length(unique(guide$lon_flip)) 
        LAT_n <- length(unique(guide$lat))
        TIME_n <- 31390 
        
        # creating array
        data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
        
        # naming
        nc_name <- paste0(ssp,"_",var,"_day_",model,"_BC.nc")
        dim_name <- variables[v,3]
        dim_long_name <- variables[v,5]
        dim_units <- variables[v,7]
        
        # defining dimensions
        lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon_flip))
        lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
        time_dim <- ncdim_def("time", units = "days", longname = "days since sstart", vals = seq(1,31390,1))
        
        # variable dimensions
        variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                  missval =  -9999,longname = dim_long_name, prec = "double")
        
        # creating empty NetCDF
        nc_out <- nc_create(nc_name,variable_dim)
        
        # adding variable to NEtCDF
        ncvar_put(nc_out, variable_dim, data_array)
        
        # closing NetCDF file
        nc_close(nc_out)
        
      }
    }
  }
}  






