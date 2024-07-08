########################################
# Author - Andre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to subset cmip6 models.

############ CMCC-ESM2 ####################

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
####### SUBSETING m = 1 ###################################----
####################################################################

#Model specific information

##############################################################

m=1

model = models[m,1]
realization = models[m,4]
grid = models[m,5]
guide <- read.dbf(paste0("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/grids/subsets/",model,"_guide.dbf"))
lon_res <- models[m,25] ###### pixel resolutions
lat_res <- models[m,24] 

for (s in 1:4){
  ssp = ssps[s]
  print(ssp)
  
  if(ssp == "historical") {dates_n = models[m,6]} # number of files
  if(ssp == "ssp245") {dates_n = models[m,7]}
  if(ssp == "ssp245") {dates_n = models[m,7]}
  if(ssp == "ssp585") {dates_n = models[m,7]}
  
  if(ssp == "historical") {dates = historical_dates[1:dates_n,m]}# creating a vector of the dates
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates = future_dates[1:dates_n,m]}  
  
  for (v in 1:3){
    var = variables[v,3]
    print(var)
  
    if(ssp == "historical") {
      
      for (d in 1:length(dates)){
        
        if (d == 1) {
        
          date = dates[d]
          print(date)
          nc_name <- paste0("/scratch/general/vast/u6047395/cmip6/",var,"/",model,"/",ssp,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")
          nc <- nc_open(nc_name)
      
      
          ####collecting model information####################
          #time <- ncvar_get(nc, "time") #no leap years!!!!!
      
          array <- ncvar_get(nc, var)
      
          pixels1 = rep(-999999, 7300)
          
          for (p in 1:length(guide$lon)){
                 
            Y <- ((guide[p,2] + 90)/lat_res)+1 # X is the pixel position in the netcdf. takes the coordinates of the pixel divide by resolution and sums 90 becouse it is in the north hemisphere 
            X <- (guide[p,3]/ lon_res)+1
        
            pixel <- array[X,Y, 1826:9125] # change every model
        
            pixels1 <- cbind(pixels1,pixel)
        
        }
      }
      d=2
      if (d == 2) {
        
        date = dates[d]
        print(date)
        nc_name <- paste0("/scratch/general/vast/u6047395/cmip6/",var,"/",model,"/",ssp,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")
        nc <- nc_open("/scratch/general/vast/u6047395/cmip6/tasmax/CMCC-ESM2/historical/tasmax_day_CMCC-ESM2_historical_r1i1p1f1_gn_19750101-19991231.nc")
        nc <- nc_open(nc_name)
        
        
        ####collecting model information####################
        #time <- ncvar_get(nc, "time") #no leap years!!!!!
        
        
        array <- ncvar_get(nc, var)
        
        pixels2 = rep(-999999, 5475)# change every model
        
        for (p in 1:length(guide$lon)){
          
          Y <- ((guide[p,2] + 90)/lat_res)+1 # X is the pixel position in the netcdf. takes the coordinates of the pixel divide by resolution and sums 90 becouse it is in the north hemisphere 
          X <- (guide[p,3]/ lon_res)+1
          
          pixel <- array[X,Y, 1:5475]
          
          pixels2 <- cbind(pixels2,pixel)
        }
      
      
        data <- rbind(pixels1[,-1], pixels2[,-1])
  
        #Creating the nectdf----
      
        getwd()
        setwd(paste0("/scratch/general/vast/u6047395/cmip6/cmip6_subset/",model,"/",ssp,"/",var))
      
        data <- as.data.frame(data)
        rownames(data) <- as.character(1:length(data$pixel))
        colnames(data) <- as.character(1:length(data))
        data <- t(data)
        
        LON_n <- length(unique(guide$lon)) 
        LAT_n <- length(unique(guide$lat))
        TIME_n <- 12775 #Changes for every model ###############################################!!!!!!!!!!!!!
      
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
    }
  }
    
    ##########################################################################
    
  if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {

    for (d in 1:length(dates)) {
        
      if (d == 1 |d == 2| d ==3) {
          
        date = dates[d]
        print(date)
        nc_name <- paste0("/scratch/general/vast/u6047395/cmip6/",var,"/",model,"/",ssp,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")
        nc <- nc_open(nc_name)
  
        ####collecting model information####################
        #time <- ncvar_get(nc, "time") #no leap years!!!!!
  
        array <- ncvar_get(nc, var)
          
        pixels1 = rep(-999999, 9125)
        p=1
        for (p in 1:length(guide$lon)){
            
          Y <- ((guide[p,2] + 90)/lat_res)+1 # X is the pixel position in the netcdf. takes the coordinates of the pixel divide by resolution and sums 90 becouse it is in the north hemisphere 
          X <- (guide[p,3]/ lon_res)+1
            
          pixel <- array[X,Y, 1:9125] # change every model
            
          pixels1 <- cbind(pixels1,pixel)
            
          if (d == 1) { pixel_d1 <- pixels1[,-1] }
          if (d == 2) { pixel_d2 <- pixels1[,-1] }
          if (d == 3) { pixel_d3 <- pixels1[,-1] }
          
          }
        }
    
      d=4
      if (d == 4) {
          
        date = dates[d]
        print(date)
        nc_name <- paste0("/scratch/general/vast/u6047395/cmip6/",var,"/",model,"/",ssp,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")
        #nc <- nc_open("/scratch/general/vast/u6047395/cmip6/tasmax/CMCC-ESM2/historical/tasmax_day_CMCC-ESM2_historical_r1i1p1f1_gn_19750101-19991231.nc")
        nc <- nc_open(nc_name)
          
        ####collecting model information####################
        #time <- ncvar_get(nc, "time") #no leap years!!!!!
          
        array <- ncvar_get(nc, var)
          
        pixels2 = rep(-999999, 4015)# change every model
          
        for (p in 1:length(guide$lon)){
            
          Y <- ((guide[p,2] + 90)/lat_res)+1 # X is the pixel position in the netcdf. takes the coordinates of the pixel divide by resolution and sums 90 becouse it is in the north hemisphere 
          X <- (guide[p,3]/ lon_res)+1
            
          pixel <- array[X,Y, 1:4015]
          
          pixels2 <- cbind(pixels2,pixel)
            
          if (d == 4) { pixel_d4 <- pixels2[,-1] }
            
          }
        }
      }
        
      data <- rbind(pixel_d1, pixel_d2, pixel_d3, pixel_d4)
        
      #Creating the nectdf----
        
      getwd()
      setwd(paste0("/scratch/general/vast/u6047395/cmip6/cmip6_subset/",model,"/",ssp,"/",var))
        
      data <- as.data.frame(data)
      rownames(data) <- as.character(1:length(data$pixel))
      colnames(data) <- as.character(1:length(data))
      data <- t(data)
        
      LON_n <- length(unique(guide$lon)) 
      LAT_n <- length(unique(guide$lat))
      TIME_n <- 31390 #Changes for every model ###############################################!!!!!!!!!!!!!
        
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

  
  
