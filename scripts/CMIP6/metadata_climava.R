#SPRDS Dowmscaling for temperature
#Andre Moraes
#andre.mortaes@usu.edu
# Utha State University
#Watershed Sciences Department

#this script has the porpuse of fixing file names and thier metadata

#packages

library(ncdf4)
library(dplyr)
library(randomForest)
library(Metrics)
library(doParallel)
library(spam)
library(foreign)
library(RNetCDF)
#library(profvis)



models <- read.csv("~/Documents/scripts/cmip6/2023/models_2023.csv") #has all the other information necessary to create the HTTPs
variables <- read.csv("~/Documents/scripts/cmip6/2023/Variables.csv")
grid <- read.dbf("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/grid/master_pixels.dbf")
vars <- c("pr", "tasmax", "tasmin")
#files <- read.csv("~/Documents/scripts/cmip6/2023/files_DS_hist.csv")
ssps <- c("historical","ssp245","ssp370","ssp585")
vars <- c("pr", "tasmax", "tasmin")


#########################################################################
##### Creating directories--------

# getwd()
# setwd("/scratch/general/vast/u6047395/cmip6/")
# list.files()
# 
# dir.create("ClimAVA-SW")
# setwd("/scratch/general/vast/u6047395/cmip6/ClimAVA-SW/")
# list.files()
# 
# for (m in 1:17){
#  model = models[m,1]
#  dir.create(model)
# }
# 
# for (m in 1:17){
#  model = models[m,1]
#  for (s in 1:4){
#    ssp =ssps[s]
#    dir = paste0(model,"/",ssp)
#    dir.create(dir)
#  }
# }
# 
# for (m in 1:17){
#  model = models[m,1]
#  for (s in 1:4){
#    ssp =ssps[s]
#    for(v in 1:3){
#      var = vars[v]
#      dir = paste0(model,"/",ssp,"/",var)
#      dir.create(dir)
#    }
#  }
# }

####################################################################

#v=1
for (v in 1:3){ #loop through tasmax and tasmin
  
  var <- paste(variables[v,3])
  variable <-  paste(variables[v,2])
  print(var)
  
#  s=3
  for (s in 1:4){ #loop through historical and 3 scenarios
    
    ssp = ssps[s]
    print(ssp)
    
    if(ssp == "historical") {files <- read.csv("~/Documents/scripts/cmip6/2023/files_DS_hist.csv")}# loading guide for files
    if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {files <- read.csv("~/Documents/scripts/cmip6/2023/files_DS_future_corrected.csv")}
    if(ssp == "historical") {files_out <- read.csv("~/Documents/scripts/cmip6/2023/files_DS_hist.csv")}# loading guide for files
    if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {files_out <- read.csv("~/Documents/scripts/cmip6/2023/files_DS_future_corrected.csv")}
    
#    m=15
    for(m in 1:17) { 
      
      model = models[m,1]
      realization <- paste(models[m,4])
      if(var == "pr" & ssp == "historical") {version = models[v,8]}
      if(var == "pr" & ssp == "ssp245") {version = models[v,9]}
      if(var == "pr" & ssp == "ssp370") {version = models[v,10]}
      if(var == "pr" & ssp == "ssp485") {version = models[v,11]}
      
      if(var == "tasmax" & ssp == "historical") {version = models[v,12]}
      if(var == "tasmax" & ssp == "ssp245") {version = models[v,13]}
      if(var == "tasmax" & ssp == "ssp370") {version = models[v,14]}
      if(var == "tasmax" & ssp == "ssp485") {version = models[v,15]}
      
      if(var == "tasmin" & ssp == "historical") {version = models[v,16]}
      if(var == "tasmin" & ssp == "ssp245") {version = models[v,17]}
      if(var == "tasmin" & ssp == "ssp370") {version = models[v,18]}
      if(var == "tasmin" & ssp == "ssp485") {version = models[v,19]}
      
      
      print(model)

      registerDoParallel(18)
      
      foreach (f = 1:length(files[,1]), 
                         .packages = c("spam","doParallel","foreach","ncdf4","dplyr", "randomForest", "Metrics")) %dopar% {
                           
        
        
        dates <- files[f,5]
        start <- files[f,3]
        finish <- files[f,4]
        length_1 <- files[f,2]
        SD <- files[f,6]
        print(Sys.time())
        #print(dates)
        
        dates_out <- files_out[f,5]
        start_date <- files_out[f,6]
        
        #fixing starts here ----
        
        nc_in_name <- paste0("/scratch/general/vast/u6047395/cmip6/SPRDS-SW/",model,"/",ssp,"/",var,"/SPRDS_SW_",model,"_",ssp,"_",var,"_",dates,".nc")
       
        #if(var == "pr") {nc_in_name <- paste0("/scratch/general/vast/u6047395/cmip6/SPRDS-SW/",model,"/",ssp,"/",var,"/SPRDS_SW_",model,"_",ssp,"_",var,"_",dates,".nc")} # name pf the bad file
        #if(var == "tasmax" |var == "tasmin") {nc_in_name <- paste0("/scratch/general/vast/u6047395/cmip6/SPRDS-SW/",model,"/",ssp,"/",var,"/SPRDS_SW_",model,"_",ssp,"_",var,"_",dates,".nc")} # name pf the bad file
        
        nc_out_name <- paste0("/scratch/general/vast/u6047395/cmip6/ClimAVA-SW/",model,"/",ssp,"/",var,"/ClimAVA-SW_",model,"_",ssp,"_",var,"_",realization,"_",dates_out,".nc")
        
        
        print(paste0("/ClimAVA-SW_",model,"_",ssp,"_",var,"_",realization,"_",dates_out,".nc"))
        
       
        #################################################################
        
        input_file <- nc_open(nc_in_name) 
       
        lon <- ncvar_get(input_file, "lon")
        lat <- ncvar_get(input_file, "lat")
        time <- ncvar_get(input_file, "time")
          
        array <- ncvar_get(input_file, var)   
        
        dim_name <- variables[v,3]
        dim_long_name <- variables[v,5]
        dim_units <- variables[v,6]
        
        ##defining dimensions
        
        lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = lon)
        lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = lat)
        time_dim <- ncdim_def("time", units = "days", longname = paste0("days since ", start_date), vals = time)
        
        variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                                  missval =  -9999,longname = dim_long_name, prec = "double", compression = 9)
        
        nc_out <- nc_create(nc_out_name,variable_dim)
        
        ncvar_put(nc_out, variable_dim, array)
        
        ncatt_put(nc_out, 0, "Name", "Clinate data for Adaptation and Vulnerability Assesments - SouthWest (ClimAVA-SW)","character")
        ncatt_put(nc_out, 0, "Version", "01","character")
        ncatt_put(nc_out, 0, "Author", "Andre Geraldo de Lima Moraes","character")
        ncatt_put(nc_out, 0, "Institution", "Utah State University, Watershed Sciences Department","character")
        ncatt_put(nc_out, 0, "Adress", "5210 Old Main Hill, NR 210, Logan, UT 84322","character")
        ncatt_put(nc_out, 0, "email", "andre.moraes@usu.edu","character")
        ncatt_put(nc_out, 0, "Description", "The ClimAVA-SW dataset provides a high-resolution (4km) bias-corrected, downscaled future climate projection based on seventeen CMIP6 GCMs. The dataset includes three variables (pr, tasmin, tasmax) and three Shared Socio-economic Pathways (SSP245, SSP370, SSP585) for the entire U.S.Southwest region. ClimAVA-SW employs the Spatial Pattern Interaction Downscaling (SPID) method which applies Random Forest model to translate the relationship and intercations between spatial patterns at a coarse resolution and fine-resolution pixel values. A random forest model is trained for each pixel, with the finer pixel of the reference data as the predictor and nine pixels from the spatially resampled (coarser) version of the reference data (at the GCMs spatial resolution) as predictors. Models are then used to downscale the GCM data. See the pepper describing the method and data set for more information.","character")
        ncatt_put(nc_out, 0, "lineage", paste0("ClimaAVA uses the Parameter-elevation Relationships on Independent The Slopes Model (PRISM 4K) project (https://prism.oregonstate.edu/) as reference data for both bias correction and the training of downscaling models. This file contain data downscaled from model",model," SSP ",ssp," variant label ",realization,", version ",version),"character")
        ncatt_put(nc_out, 0, "License", "CCO 1.0 Universal","character")
        ncatt_put(nc_out, 0, "fees", "This data set is free","character")
        ncatt_put(nc_out, 0, "Disclaimer", "While every effort has been made to ensure the accuracy and completeness of the data, no guarantee is given that the information provided is error-free or that the dataset will be suitable for any particular purpose. Users are advised to use this dataset with caution and to independently verify the data before making any decisions based on it. The creators of this dataset make no warranties, express or implied, regarding the dataset's accuracy, reliability, or fitness for a particular purpose. In no event shall the creators be liable for any damages, including but not limited to direct, indirect, incidental, special, or consequential damages, arising out of the use or inability to use the dataset. Users of this dataset are encouraged to properly cite the dataset in any publications or works that make use of the data. By using this dataset, you agree to these terms and conditions. If you do not agree with these terms, please do not use the dataset.","character")
        
        nc_close(nc_out)
        
      }
    }
  }
}

