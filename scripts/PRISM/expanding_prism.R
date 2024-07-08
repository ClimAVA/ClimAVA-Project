########################################
# Author - Aandre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to create to expand the prism NetCDFs. why? for the resampling step to work we need a bunch of NA values
# outside our area of interest. This script adds all of those NAs.

#setting directory to save expanded prism netcdfs
setwd("/scratch/general/vast/u6047395/prism/expanded_compresed/")
getwd()

#packages
library(doParallel)
library(foreign)
library(ncdf4)

#dopar parameters
registerDoParallel(50) #setting number of cores

# files to guide loops
variables <- read.csv("~/Documents/scripts/cmip6/2023/Variables.csv")
#the exp_guide has colum with zeros and ones to determine where NAs should be filled
exp_guide <- read.csv("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/prism_guide_expand.csv")
year_start <- as.character(seq(1981,2019,5))
year_end <- as.character(seq(1985,2020,5))

v=1
for (v in 1:length(variables$ID)){
  var <- variables[v,3]
  print(var)
  
  i=1
  for (i in 1:8 ) {
    s = year_start[i]
    S = paste0(s,"0101") # to load the first raster for the stack
    e = year_end[i]
    E = paste0(e,"1231") #to end the seq of days
    print(S)
    print(Sys.time())
    
    nc <- nc_open(paste0("/scratch/general/vast/u6047395/prism/AOI_compressed/prism_sw_",var,"_",S,"_",E,".nc"))
    
    #extracting dimensions and resolutions
    lon <- unique(ncvar_get(nc, "lon")) 
    lon_res <- abs(lon[1] - lon[2])
    lat_lenght <- length(ncvar_get(nc, "lat"))
    lat <- unique(ncvar_get(nc, "lat"))
    lat_res <- abs(lat[1] - lat[2])
    
    #extracting variable
    array <- ncvar_get(nc, var)
    
    #loop trhough every pixel of the exp_guide
    data <- foreach (p = 1:length(exp_guide$lon), .combine=cbind, .multicombine = T, .packages = c("ncdf4")) %dopar% {  
                       print(p)
                       guide_lon <- exp_guide[p,2] # get central coordinates (lon flip)
                       guide_lat <- exp_guide[p,3] # get central coordinates
                       
                       X <- round((guide_lon - -125) / lon_res,0)+1 # finding central coordinate in prism nc coordinates
                       Y <- as.double(round(lat_lenght - (guide_lat - tail(lat,1))/lat_res)) # finding central coordinate in prism nc coordinates
                       
                       # Determining if pixel should be NA (-9999) or a value
                       not_ap = exp_guide[p,4]
                       
                       if (not_ap == 0) {pixel = rep(-9999, 1825)}
                       
                       if (not_ap == 1) {
                         
                         pixel <- round(array[X,Y,1:1825],1)
                         
                         if (is.na(pixel[1]) == TRUE){ # this is safety feature (redundancy) to guaranty that NA in the original file are NA in the final file
                           
                           pixel <- rep(-9999, 1825)
                         }
                       }
                       
                       cbind(pixel)
                       
                     }  
    
    print("creating NetCDF")
    print(Sys.time())
    data <- as.data.frame(data)
    rownames(data) <- as.character(1:length(data$pixel))
    colnames(data) <- as.character(1:length(data))
    data <- t(data)
    
    LON_n <- length(unique(exp_guide$lon)) 
    LAT_n <- length(unique(exp_guide$lat))
    TIME_n <- 1825 
    
    data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
    
    nc_name <- paste0("prism_",var,"_day_",S,"-",E,".nc")
    dim_name <- variables[v,3]
    dim_long_name <- variables[v,5]
    dim_units <- variables[v,7]
    
    ##defining dimensions
    lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(exp_guide$lon))
    lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(exp_guide$lat))
    time_dim <- ncdim_def("time", units = "days", longname = paste0("days since",S) , vals = seq(1,1825,1))
    
    #defining variable
    variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                              missval =  -9999,longname = dim_long_name, prec = "double", compression = 9)
    #creating empty NetCDF
    nc_out <- nc_create(nc_name,variable_dim)
    
    #adding variable to NetCDF
    ncvar_put(nc_out, variable_dim, data_array)
    
    #adding global metadata to NetCDF
    ncatt_put(nc_out, 0, "Name", "prism NetCDF","character")
    ncatt_put(nc_out, 0, "Version", "NA")
    ncatt_put(nc_out, 0, "Author", "Andre Geraldo de Lima Moraes","character")
    ncatt_put(nc_out, 0, "Institution", "Utah State University, Watershed Sciences Department","character")
    ncatt_put(nc_out, 0, "Adress", "5210 Old Main Hill, NR 210, Logan, UT 84322","character")
    ncatt_put(nc_out, 0, "email", "andre.moraes@usu.edu","character")
    ncatt_put(nc_out, 0, "Description", "This is the same as the prism data, but in NetCDF format and expanded NA data","character")
    ncatt_put(nc_out, 0, "lineage", "Parameter-elevation Relationships on Independent The Slopes Model (PRISM 4K) project (https://prism.oregonstate.edu/)", "character")
    ncatt_put(nc_out, 0, "License", "Same as PRISM","character")
    ncatt_put(nc_out, 0, "fees", "This data set is free","character")
    ncatt_put(nc_out, 0, "Disclaimer", "While every effort has been made to ensure the accuracy and completeness of the data, no guarantee is given that the information provided is error-free or that the dataset will be suitable for any particular purpose. Users are advised to use this dataset with caution and to independently verify the data before making any decisions based on it. The creators of this dataset make no warranties, express or implied, regarding the dataset's accuracy, reliability, or fitness for a particular purpose. In no event shall the creators be liable for any damages, including but not limited to direct, indirect, incidental, special, or consequential damages, arising out of the use or inability to use the dataset. Users of this dataset are encouraged to properly cite the dataset in any publications or works that make use of the data. By using this dataset, you agree to these terms and conditions. If you do not agree with these terms, please do not use the dataset.","character")
    
    #closing NetCDF
    nc_close(nc_out)
    
  }
  
}


# Fim