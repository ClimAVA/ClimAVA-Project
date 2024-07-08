########################################
# Author - Andre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to subset cmip6 models.

############ EC-Earth3 ####################

future_dates <- read.csv("~/Documents/scripts/cmip6/2023/future_dates_2023.csv") # all dates for ssp245
historical_dates <- read.csv("~/Documents/scripts/cmip6/2023/historical_dates_2023.csv") # all dates for historical
models <- read.csv("~/Documents/scripts/cmip6/2023/models_2023.csv") #has all the other information necessary to create the HTTPs
variables <- read.csv("~/Documents/scripts/cmip6/2023/Variables.csv")
ssps <- c("historical","ssp245","ssp370","ssp585")

historical_days <- seq(1,12775,1)
ssp_days <- seq(12776,44165,1)

library(ncdf4)
library(dplyr)
library(foreign)


#########################################################################
#####Creating directories--------
# getwd()
# setwd("/scratch/general/vast/u6047395/cmip6/cmip6_subset")
# 
# for (m in 1:17){
#   model = models[m,1]
#   dir.create(model)
# }
# 
# for (m in 1:17){
#   model = models[m,1]
#   for (s in 1:4){
#     ssp = ssps[s]
#     dir = paste0(model,"/",ssp)
#     dir.create(dir)
#   }
# 
# }
# 
# 
# for (m in 1:17){
#   model = models[m,1]
#   for (s in 1:4){
#     ssp = ssps[s]
#     for (i in 1:3){
#       v = vars[i]
#       dir = paste0(model,"/",ssp,"/",v)
#       dir.create(dir)
#     }
#   }
# }
####################################################################
####### SUBSETING m = 2 ###################################----
####################################################################

#Model specific information

lon_res <- 360 / 512 ###### CHANGES FOR EVERY MODEL
lat_res <- 180 / 256 

##############################################################

m=2 

model = models[m,1]
realization = models[m,4]
grid = models[m,5]
guide <- read.dbf(paste0("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/grids/subsets/",model,"_guide.dbf"))

s=1
for (s in 1:4){
  ssp = ssps[s]
  print(ssp)
  
  if(ssp == "historical") {dates_n = models[m,6]} # number of files
  if(ssp == "ssp245") {dates_n = models[m,7]}
  if(ssp == "ssp245") {dates_n = models[m,7]}
  if(ssp == "ssp585") {dates_n = models[m,7]}
  
  if(ssp == "historical") {dates = historical_dates[1:dates_n,m]}# creating a vector of the dates
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates = future_dates[1:dates_n,m]}  
  
  
  v=2
  for (v in 1:3){
    var = variables[v,3]
    print(var)
    
    if(ssp == "historical") {
      
      d=1
      for (d in 1:length(dates)){
        
        date = dates[d]
        print(date)
        nc_name <- paste0("/scratch/general/vast/u6047395/cmip6/",var,"/",model,"/",ssp,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")
        nc <- nc_open(nc_name)
          
          
        ####collecting model information####################
        #time <- ncvar_get(nc, "time") #no leap years!!!!!
          
          
        array <- ncvar_get(nc, var)
          
        pixels = rep(-999999, 365)
        p=1
        for (p in 1:length(guide$lon)){
            
          Y <- ((guide[p,2] + 90)/lat_res) +1 # X is the pixel position in the netcdf. takes the coordinates of the pixel divide by resolution and sums 90 becouse it is in the north hemisphere 
          X <- (guide[p,3]/ lon_res)+1
            
          pixel <- array[X,Y, 1:365] # change every model
            
          pixels <- cbind(pixels,pixel)
            
          if (d == 1) { pixels_d1 <- pixels[,-1] }
          if (d == 2) { pixels_d2 <- pixels[,-1] }
          if (d == 3) { pixels_d3 <- pixels[,-1] }
          if (d == 4) { pixels_d4 <- pixels[,-1] }
          if (d == 5) { pixels_d5 <- pixels[,-1] }
          if (d == 6) { pixels_d6 <- pixels[,-1] }
          if (d == 7) { pixels_d7 <- pixels[,-1] }
          if (d == 8) { pixels_d8 <- pixels[,-1] }
          if (d == 9) { pixels_d9 <- pixels[,-1] }
          if (d == 10) { pixels_d10 <- pixels[,-1] }
          if (d == 11) { pixels_d11 <- pixels[,-1] }
          if (d == 12) { pixels_d12 <- pixels[,-1] }
          if (d == 13) { pixels_d13 <- pixels[,-1] }
          if (d == 14) { pixels_d14 <- pixels[,-1] }
          if (d == 15) { pixels_d15 <- pixels[,-1] }
          if (d == 16) { pixels_d16 <- pixels[,-1] }
          if (d == 17) { pixels_d17 <- pixels[,-1] }
          if (d == 18) { pixels_d18 <- pixels[,-1] }
          if (d == 19) { pixels_d19 <- pixels[,-1] }
          if (d == 20) { pixels_d20 <- pixels[,-1] }
          if (d == 21) { pixels_d21 <- pixels[,-1] }
          if (d == 22) { pixels_d22 <- pixels[,-1] }
          if (d == 23) { pixels_d23 <- pixels[,-1] }
          if (d == 24) { pixels_d24 <- pixels[,-1] }
          if (d == 25) { pixels_d25 <- pixels[,-1] }
          if (d == 26) { pixels_d26 <- pixels[,-1] }
          if (d == 27) { pixels_d27 <- pixels[,-1] }
          if (d == 28) { pixels_d28 <- pixels[,-1] }
          if (d == 29) { pixels_d29 <- pixels[,-1] }
          if (d == 30) { pixels_d30 <- pixels[,-1] }
          if (d == 31) { pixels_d31 <- pixels[,-1] }
          if (d == 32) { pixels_d32 <- pixels[,-1] }
          if (d == 33) { pixels_d33 <- pixels[,-1] }
          if (d == 34) { pixels_d34 <- pixels[,-1] }
          if (d == 35) { pixels_d35 <- pixels[,-1] }
        }
      }
        
      data <- rbind(pixels_d1, pixels_d2, pixels_d3, pixels_d4, pixels_d5, pixels_d6,
                    pixels_d7, pixels_d8, pixels_d9, pixels_d10, pixels_d11, pixels_d12,
                    pixels_d13, pixels_d14, pixels_d15, pixels_d16, pixels_d17, pixels_d18,
                    pixels_d19, pixels_d20, pixels_d21, pixels_d22, pixels_d23, pixels_d24,
                    pixels_d25, pixels_d26, pixels_d27, pixels_d28, pixels_d29, pixels_d30,
                    pixels_d31, pixels_d32, pixels_d33, pixels_d34, pixels_d35)
          
      #Creating the nectdf----
          
      getwd()
      setwd(paste0("/scratch/general/vast/u6047395/cmip6/cmip6_subset/",model,"/",ssp,"/",var))
          
      data <- as.data.frame(data)
      rownames(data) <- as.character(1:length(data$pixel))
      colnames(data) <- as.character(1:length(data))
      data <- t(data)
          
      LON_n <- length(unique(guide$lon)) 
      LAT_n <- length(unique(guide$lat))
      TIME_n <- 12775 
          
      data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
          
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- variables[v,3]
      dim_long_name <- variables[v,5]
      dim_units <- variables[v,7]
          
      ##defining dimensions
          
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 19800101", vals = historical_days)
          
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval =  -9999,longname = dim_long_name, prec = "double")
          
      nc_out <- nc_create(nc_name,variable_dim)
          
      ncvar_put(nc_out, variable_dim, data_array)
          
      nc_close(nc_out)
    }
    
    
    ##########################################################################
    
    if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {
      
      d=1
      for (d in 1:length(dates)){
        
        date = dates[d]
        print(date)
        nc_name <- paste0("/scratch/general/vast/u6047395/cmip6/",var,"/",model,"/",ssp,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")
        nc <- nc_open(nc_name)
          
        ####collecting model information####################
        #time <- ncvar_get(nc, "time") #no leap years!!!!!
          
        array <- ncvar_get(nc, var)
          
        pixels = rep(-999999, 365)
          p=1
          for (p in 1:length(guide$lon)){
            
            Y <- ((guide[p,2] + 90)/lat_res)+1 # X is the pixel position in the netcdf. takes the coordinates of the pixel divide by resolution and sums 90 becouse it is in the north hemisphere 
            X <- (guide[p,3]/ lon_res)+1
            
            pixel <- array[X,Y, 1:365] # change every model
            
            pixels <- cbind(pixels,pixel)
            
            if (d == 1) { pixels_d1 <- pixels[,-1] }
            if (d == 2) { pixels_d2 <- pixels[,-1] }
            if (d == 3) { pixels_d3 <- pixels[,-1] }
            if (d == 4) { pixels_d4 <- pixels[,-1] }
            if (d == 5) { pixels_d5 <- pixels[,-1] }
            if (d == 6) { pixels_d6 <- pixels[,-1] }
            if (d == 7) { pixels_d7 <- pixels[,-1] }
            if (d == 8) { pixels_d8 <- pixels[,-1] }
            if (d == 9) { pixels_d9 <- pixels[,-1] }
            if (d == 10) { pixels_d10 <- pixels[,-1] }
            if (d == 11) { pixels_d11 <- pixels[,-1] }
            if (d == 12) { pixels_d12 <- pixels[,-1] }
            if (d == 13) { pixels_d13 <- pixels[,-1] }
            if (d == 14) { pixels_d14 <- pixels[,-1] }
            if (d == 15) { pixels_d15 <- pixels[,-1] }
            if (d == 16) { pixels_d16 <- pixels[,-1] }
            if (d == 17) { pixels_d17 <- pixels[,-1] }
            if (d == 18) { pixels_d18 <- pixels[,-1] }
            if (d == 19) { pixels_d19 <- pixels[,-1] }
            if (d == 20) { pixels_d20 <- pixels[,-1] }
            if (d == 21) { pixels_d21 <- pixels[,-1] }
            if (d == 22) { pixels_d22 <- pixels[,-1] }
            if (d == 23) { pixels_d23 <- pixels[,-1] }
            if (d == 24) { pixels_d24 <- pixels[,-1] }
            if (d == 25) { pixels_d25 <- pixels[,-1] }
            if (d == 26) { pixels_d26 <- pixels[,-1] }
            if (d == 27) { pixels_d27 <- pixels[,-1] }
            if (d == 28) { pixels_d28 <- pixels[,-1] }
            if (d == 29) { pixels_d29 <- pixels[,-1] }
            if (d == 30) { pixels_d30 <- pixels[,-1] }
            if (d == 31) { pixels_d31 <- pixels[,-1] }
            if (d == 32) { pixels_d32 <- pixels[,-1] }
            if (d == 33) { pixels_d33 <- pixels[,-1] }
            if (d == 34) { pixels_d34 <- pixels[,-1] }
            if (d == 35) { pixels_d35 <- pixels[,-1] }
            if (d == 36) { pixels_d36 <- pixels[,-1] }
            if (d == 37) { pixels_d37 <- pixels[,-1] }
            if (d == 38) { pixels_d38 <- pixels[,-1] }
            if (d == 39) { pixels_d39 <- pixels[,-1] }
            if (d == 40) { pixels_d40 <- pixels[,-1] }
            if (d == 41) { pixels_d41 <- pixels[,-1] }
            if (d == 42) { pixels_d42 <- pixels[,-1] }
            if (d == 43) { pixels_d43 <- pixels[,-1] }
            if (d == 44) { pixels_d44 <- pixels[,-1] }
            if (d == 45) { pixels_d45 <- pixels[,-1] }
            if (d == 46) { pixels_d46 <- pixels[,-1] }
            if (d == 47) { pixels_d47 <- pixels[,-1] }
            if (d == 48) { pixels_d48 <- pixels[,-1] }
            if (d == 49) { pixels_d49 <- pixels[,-1] }
            if (d == 50) { pixels_d50 <- pixels[,-1] }
            if (d == 51) { pixels_d51 <- pixels[,-1] }
            if (d == 52) { pixels_d52 <- pixels[,-1] }
            if (d == 53) { pixels_d53 <- pixels[,-1] }
            if (d == 54) { pixels_d54 <- pixels[,-1] }
            if (d == 55) { pixels_d55 <- pixels[,-1] }
            if (d == 56) { pixels_d56 <- pixels[,-1] }
            if (d == 57) { pixels_d57 <- pixels[,-1] }
            if (d == 58) { pixels_d58 <- pixels[,-1] }
            if (d == 59) { pixels_d59 <- pixels[,-1] }
            if (d == 60) { pixels_d60 <- pixels[,-1] }
            if (d == 61) { pixels_d61 <- pixels[,-1] }
            if (d == 62) { pixels_d62 <- pixels[,-1] }
            if (d == 63) { pixels_d63 <- pixels[,-1] }
            if (d == 64) { pixels_d64 <- pixels[,-1] }
            if (d == 65) { pixels_d65 <- pixels[,-1] }
            if (d == 66) { pixels_d66 <- pixels[,-1] }
            if (d == 67) { pixels_d67 <- pixels[,-1] }
            if (d == 68) { pixels_d68 <- pixels[,-1] }
            if (d == 69) { pixels_d69 <- pixels[,-1] }
            if (d == 70) { pixels_d70 <- pixels[,-1] }
            if (d == 71) { pixels_d71 <- pixels[,-1] }
            if (d == 72) { pixels_d72 <- pixels[,-1] }
            if (d == 73) { pixels_d73 <- pixels[,-1] }
            if (d == 74) { pixels_d74 <- pixels[,-1] }
            if (d == 75) { pixels_d75 <- pixels[,-1] }
            if (d == 76) { pixels_d76 <- pixels[,-1] }
            if (d == 77) { pixels_d77 <- pixels[,-1] }
            if (d == 78) { pixels_d78 <- pixels[,-1] }
            if (d == 79) { pixels_d79 <- pixels[,-1] }
            if (d == 80) { pixels_d80 <- pixels[,-1] }
            if (d == 81) { pixels_d81 <- pixels[,-1] }
            if (d == 82) { pixels_d82 <- pixels[,-1] }
            if (d == 83) { pixels_d83 <- pixels[,-1] }
            if (d == 84) { pixels_d84 <- pixels[,-1] }
            if (d == 85) { pixels_d85 <- pixels[,-1] }
            if (d == 86) { pixels_d86 <- pixels[,-1] }
          
          }
        }
        
        
      data <- rbind(pixels_d1, pixels_d2, pixels_d3, pixels_d4, pixels_d5, pixels_d6,
                    pixels_d7, pixels_d8, pixels_d9, pixels_d10, pixels_d11, pixels_d12,
                    pixels_d13, pixels_d14, pixels_d15, pixels_d16, pixels_d17, pixels_d18,
                    pixels_d19, pixels_d20, pixels_d21, pixels_d22, pixels_d23, pixels_d24,
                    pixels_d25, pixels_d26, pixels_d27, pixels_d28, pixels_d29, pixels_d30,
                    pixels_d31, pixels_d32, pixels_d33, pixels_d34, pixels_d35,
                    pixels_d36, pixels_d37, pixels_d38, pixels_d39, pixels_d40,
                    pixels_d41, pixels_d42, pixels_d43, pixels_d44, pixels_d45,
                    pixels_d46, pixels_d47, pixels_d48, pixels_d49, pixels_d50,
                    pixels_d51, pixels_d52, pixels_d53, pixels_d54, pixels_d55,
                    pixels_d56, pixels_d57, pixels_d58, pixels_d59, pixels_d60,
                    pixels_d61, pixels_d62, pixels_d63, pixels_d64, pixels_d65,
                    pixels_d66, pixels_d67, pixels_d68, pixels_d69, pixels_d70,
                    pixels_d71, pixels_d72, pixels_d73, pixels_d74, pixels_d75,
                    pixels_d76, pixels_d77, pixels_d78, pixels_d79, pixels_d80,
                    pixels_d81, pixels_d82, pixels_d83, pixels_d84, pixels_d85,
                    pixels_d86)
        
      #Creating the nectdf----
        
      getwd()
      setwd(paste0("/scratch/general/vast/u6047395/cmip6/cmip6_subset/",model,"/",ssp,"/",var))
      
      data <- as.data.frame(data)
      rownames(data) <- as.character(1:length(data$pixel))
      colnames(data) <- as.character(1:length(data))
      data <- t(data)
        
      LON_n <- length(unique(guide$lon)) 
      LAT_n <- length(unique(guide$lat))
      TIME_n <- 31390 
        
      data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
        
      nc_name <- paste0(var,"_day_",model,"_",realization,"_",ssp,"_subset.nc")
      dim_name <- variables[v,3]
      dim_long_name <- variables[v,5]
      dim_units <- variables[v,7]
        
      ##defining dimensions
        
      lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon))
      lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
      time_dim <- ncdim_def("time", units = "days", longname = "days since 19800101", vals = ssp_days)
        
      variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                missval =  -9999,longname = dim_long_name, prec = "double")
        
      nc_out <- nc_create(nc_name,variable_dim)
      
      ncvar_put(nc_out, variable_dim, data_array)
        
      nc_close(nc_out)
        
    }
  }
}



# Fim


