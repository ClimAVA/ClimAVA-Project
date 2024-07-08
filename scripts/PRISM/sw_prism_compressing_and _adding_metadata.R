########################################
# Author - Aandre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to compress and add metadata to PRISM NetCDFs.

library(ncdf4)

#objects for loop
year_start <- as.character(seq(1981,2019,5))
year_end <- as.character(seq(1985,2020,5))
variables <- read.csv("~/Documents/scripts/cmip6/2023/Variables.csv")

#dopar parameters
library(doParallel)
registerDoParallel(8) #setting number of cores

#loop though variables
for(v in 1: length(variables$ID)){
  var = variables[v,3]
  print(var)
  print(Sys.time())

  #loop though 5-year chunks
  foreach(i = 1:length(year_start), .packages = c("raster","lubridate","ncdf4")) %dopar% {
    s = year_start[i]
    S = paste0(s,"0102") # to start the seq of days
    S1 = paste0(s,"0101") # to load the first raster for the stack
    e = year_end[i]
    E = paste0(e,"1231") #to end the seq of days
    
    #opneing file to be modified
    input_file_name <- paste0("/scratch/general/vast/u6047395/prism/AOI/prism_sw_",var,"_",S1,"_",E,".nc")
    input_file <- nc_open(input_file_name)
    
    #extracting variables from form input files
    lon <- ncvar_get(input_file, "lon")
    lat <- ncvar_get(input_file, "lat")
    time <- ncvar_get(input_file, "time")
    
    #Correcting issues with variable names
    if(var == "pr") {var = "precipitation"} # name pf the bad file
    if(var == "tasmax") {var = "tmax"} # name pf the bad file
    if(var == "tasmin") {var = "tmin"} # name pf the bad file
    
    #Extracting array of data
    array <- ncvar_get(input_file, var)
    
    #going back to right name of variables - it will be used when building the new NetCDF
    var = variables[v,3]
    
    #naming dimensins
    dim_name <- variables[v,3]
    dim_long_name <- variables[v,5]
    dim_units <- variables[v,6]
    
    ##defining dimensions
    lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = lon)
    lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = lat)
    time_dim <- ncdim_def("time", units = "days", longname = paste0("days since ", E), vals = time)
    
    #defining variable dimensions and compressing data
    variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                              missval =  -9999,longname = dim_long_name, prec = "double", compression = 9)
    
    #defining name of output
    nc_out_name <- paste0("/scratch/general/vast/u6047395/prism/AOI_compressed/prism_sw_",var,"_",S1,"_",E,".nc")
    
    #creating empty Netcdf
    nc_out <- nc_create(nc_out_name,variable_dim)
    
    #adding variables to NetCDF
    ncvar_put(nc_out, variable_dim, array)
    
    #adding global metadata to NetCDF
    ncatt_put(nc_out, 0, "Name", "prism NetCDF","character")
    ncatt_put(nc_out, 0, "Version", "NA")
    ncatt_put(nc_out, 0, "Author", "Andre Geraldo de Lima Moraes","character")
    ncatt_put(nc_out, 0, "Institution", "Utah State University, Watershed Sciences Department","character")
    ncatt_put(nc_out, 0, "Adress", "5210 Old Main Hill, NR 210, Logan, UT 84322","character")
    ncatt_put(nc_out, 0, "email", "andre.moraes@usu.edu","character")
    ncatt_put(nc_out, 0, "Description", "This is the same as the prism data, but in NetCDF format","character")
    ncatt_put(nc_out, 0, "lineage", "Parameter-elevation Relationships on Independent The Slopes Model (PRISM 4K) project (https://prism.oregonstate.edu/)", "character")
    ncatt_put(nc_out, 0, "License", "Same as PRISM","character")
    ncatt_put(nc_out, 0, "fees", "This data set is free","character")
    ncatt_put(nc_out, 0, "Disclaimer", "While every effort has been made to ensure the accuracy and completeness of the data, no guarantee is given that the information provided is error-free or that the dataset will be suitable for any particular purpose. Users are advised to use this dataset with caution and to independently verify the data before making any decisions based on it. The creators of this dataset make no warranties, express or implied, regarding the dataset's accuracy, reliability, or fitness for a particular purpose. In no event shall the creators be liable for any damages, including but not limited to direct, indirect, incidental, special, or consequential damages, arising out of the use or inability to use the dataset. Users of this dataset are encouraged to properly cite the dataset in any publications or works that make use of the data. By using this dataset, you agree to these terms and conditions. If you do not agree with these terms, please do not use the dataset.","character")
    
    #closing NetCDF
    nc_close(nc_out)
  }  
}

#Fim