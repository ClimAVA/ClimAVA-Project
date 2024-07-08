########################################
# Author - Aandre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to create netcdf files from prism files for the area of interest.

#
setwd("/scratch/general/vast/u6047395/prism/AOI")

library(raster)
library(lubridate)
library(ncdf4)

# loading shape file of the state of Utah
aoi <- shapefile("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/AOI/AOI_2023_06.shp") # loading shape file of the state of Utah

#objects for loop
year_start <- as.character(seq(1981,2019,5))
year_end <- as.character(seq(1985,2020,5))

#dopar parameters
library(doParallel)
registerDoParallel(2) #setting number of cores



################## creating tmax netcdf ###################----

print("tmax")
print(Sys.time())
i=8
foreach(i = 1:8, .packages = c("raster","lubridate","ncdf4")) %dopar% {
  s = year_start[i]
  S = paste0(s,"0102") # to start the seq of days
  S1 = paste0(s,"0101") # to load the first raster for the stack
  e = year_end[i]
  E = paste0(e,"1231") #to end the seq of days
  
  days <- gsub("-", "", as.character(seq(ymd(S), ymd(E), by = "days")), "-") # sequence of of years with starting days
  
  #cropping the first day of each 5-year daily sequence and starting the stack
  daily_tmax <- raster(paste0("/scratch/general/vast/u6047395/prism/raw_1981-2020_tmax/PRISM_tmax_stable_4kmD2_",S1,"_bil/PRISM_tmax_stable_4kmD2_",S1,"_bil.bil"))
  daily_tmax <- mask(daily_tmax, aoi)
  daily_tmax <- crop(daily_tmax, aoi)
  daily_tmax <- round(daily_tmax, digits = 1)
  daily_tmax <- stack(daily_tmax) 
  
  #cropping and staking all remaining days of the 5-year daily sequence 
  for(d in 1:length(days)){
    
    D = days[d]
    print(D)
    next_map <- raster(paste0("/scratch/general/vast/u6047395/prism/raw_1981-2020_tmax/PRISM_tmax_stable_4kmD2_",D,"_bil/PRISM_tmax_stable_4kmD2_",D,"_bil.bil")) 
    next_map <- mask(next_map, aoi)
    next_map <- crop(next_map, aoi)
    next_map <- round(next_map, digits = 1)
    daily_tmax <- stack(daily_tmax, next_map)
  }
  
  #exporting/saving netcdf file  
  file_name <- paste0("prism_sw_tasmax_",S1,"_",E,".nc")
  
  writeRaster(daily_tmax, file_name, overwrite = T, format ="CDF",
              varname = "tasmax",varunit = "C", longname = "daily maximum air temperature",
              xname = "lon", yname = "lat", zname = "time")
  rm(daily_tmax)
}

####################### creating tmin netcdfs ######################-------

print("tmin")
print(Sys.time())
foreach(i = 1:8, .packages = c("raster","lubridate")) %dopar% {

  s = year_start[i]
  S = paste0(s,"0102") 
  S1 = paste0(s,"0101") 
  e = year_end[i]
  E = paste0(e,"1231") 
  
  days <- gsub("-", "", as.character(seq(ymd(S), ymd(E), by = "days")), "-") 
  
  daily_tmin <- raster(paste0("/scratch/general/vast/u6047395/prism/raw_1981-2020_tmin/PRISM_tmin_stable_4kmD2_",S1,"_bil/PRISM_tmin_stable_4kmD2_",S1,"_bil.bil"))
  daily_tmin <- mask(daily_tmin, aoi)
  daily_tmin <- crop(daily_tmin, aoi)
  daily_tmin <- round(daily_tmin, digits = 1)
  daily_tmin <- stack(daily_tmin) 
  
  for(d in 1:length(days)){
    
    D = days[d]
    print(D)
    next_map <- raster(paste0("/scratch/general/vast/u6047395/prism/raw_1981-2020_tmin/PRISM_tmin_stable_4kmD2_",D,"_bil/PRISM_tmin_stable_4kmD2_",D,"_bil.bil")) 
    next_map <- mask(next_map, aoi)
    next_map <- crop(next_map, aoi)
    next_map <- round(next_map, digits = 1)
    daily_tmin <- stack(daily_tmin, next_map)
  
  }
  
  file_name <- paste0("prism_SW_tasmin_",S1,"_",E,".nc")
  
  writeRaster(daily_tmin, file_name, overwrite = T, format ="CDF",
              varname = "tasmin",varunit = "C", longname = "daily minimum air temperature",
              xname = "lon", yname = "lat", zname = "time")
  rm(daily_tmin)
}


###################### building PRISM pr netcdfs ###############################--------

print("ppt")
print(Sys.time())

foreach(i = 1:8, .packages = c("raster","lubridate")) %dopar% {

  s = year_start[i]
  S = paste0(s,"0102") 
  S1 = paste0(s,"0101") 
  e = year_end[i]
  E = paste0(e,"1231") 
  
  days <- gsub("-", "", as.character(seq(ymd(S), ymd(E), by = "days")), "-") 
  
  daily_ppt <- raster(paste0("/scratch/general/vast/u6047395/prism/raw_1981-2020_ppt/PRISM_ppt_stable_4kmD2_",S1,"_bil/PRISM_ppt_stable_4kmD2_",S1,"_bil.bil"))
  daily_ppt <- mask(daily_ppt, aoi)
  daily_ppt <- crop(daily_ppt, aoi)
  daily_ppt <- round(daily_ppt, digits = 1)
  daily_ppt <- stack(daily_ppt) 
  
  for(d in 1:length(days)){
    
    D = days[d]
    print(D)
    next_map <- raster(paste0("/scratch/general/vast/u6047395/prism/raw_1981-2020_ppt/PRISM_ppt_stable_4kmD2_",D,"_bil/PRISM_ppt_stable_4kmD2_",D,"_bil.bil")) 
    next_map <- mask(next_map, aoi)
    next_map <- crop(next_map, aoi)
    next_map <- round(next_map, digits = 1)
    daily_ppt <- stack(daily_ppt, next_map)
    
  }

  file_name <- paste0("prism_SW_pr_",S1,"_",E,".nc")
  
  writeRaster(daily_ppt, file_name, overwrite = T, format ="CDF",
              varname = "pr",varunit = "mm", longname = "daily precipittaion",
              xname = "lon", yname = "lat", zname = "time")
  rm(daily_ppt)
}

#Fim
          
          
