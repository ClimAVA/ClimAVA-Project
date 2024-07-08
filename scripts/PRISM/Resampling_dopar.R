######################################################################
# Author - Aandre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to create NetCDF files of resampled PRSM data. Data is resampled to match pixel location and 
# spatial resolution of selected GCMs (17 GCMs)

#######################################################################



#setimg the dorectory where files will be saved
setwd("/scratch/general/vast/u6047395/prism/resampled_compresed/")
getwd()

#loading packages
library(doParallel)
library(foreign)
library(ncdf4)

#loading files necessary to guide the loops
models <- read.csv("~/Documents/scripts/cmip6/2023/models_2023.csv") #has all the other information necessary to create the HTTPs
variables <- read.csv("~/Documents/scripts/cmip6/2023/Variables.csv")

#do par parameters
registerDoParallel(50)

#loop through variables
for (v in 1:length(variables$ID)){
  
  var <- variables[v,3]
  print(var)
  
  # load all ncs per variable
  nc1 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/expanded_compresed/prism_",var,"_day_19810101-19851231.nc"))
  nc2 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/expanded_compresed/prism_",var,"_day_19860101-19901231.nc"))
  nc3 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/expanded_compresed/prism_",var,"_day_19910101-19951231.nc"))
  nc4 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/expanded_compresed/prism_",var,"_day_19960101-20001231.nc"))
  nc5 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/expanded_compresed/prism_",var,"_day_20010101-20051231.nc"))
  nc6 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/expanded_compresed/prism_",var,"_day_20060101-20101231.nc"))
  nc7 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/expanded_compresed/prism_",var,"_day_20110101-20151231.nc"))
  nc8 <- nc_open(paste0("/scratch/general/vast/u6047395/prism/expanded_compresed/prism_",var,"_day_20160101-20201231.nc"))
  
  #extracting lat, lon dimensions and resulution from ncs
  lon <- unique(ncvar_get(nc1, "lon")) # all longitudes
  lon_res <- abs(lon[1] - lon[2]) # lon resolution
  lat_lenght <- length(ncvar_get(nc1, "lat")) 
  lat <- unique(ncvar_get(nc1, "lat"))
  lat_res <- abs(lat[1] - lat[2])
  
  #Creating arrays
  array1 <- ncvar_get(nc1, var)
  array2 <- ncvar_get(nc2, var)
  array3 <- ncvar_get(nc3, var)
  array4 <- ncvar_get(nc4, var)
  array5 <- ncvar_get(nc5, var)
  array6 <- ncvar_get(nc6, var)
  array7 <- ncvar_get(nc7, var)
  array8 <- ncvar_get(nc8, var)
  
  #removing unnecessary NCs
  rm(nc1, nc2, nc3, nc4, nc5, nc6, nc7, nc8)
  
  for (m in 1:17){
    
    model = models[m,1]
    print(model)
    print(Sys.time())
    
    #loading the guide with the coordinates for each pixel of respective GCM
    guide <- read.dbf(paste0("/uufs/chpc.utah.edu/common/home/u6047395/Documents/scripts/cmip6/2023/grids/subsets/",model,"_guide.dbf"))
    guide_lat_res <- models[m,24] #lat resolution of GCM
    guide_lon_res <- models[m,25] #lon resolution of GCM
    lat_n_pixels <- round(guide_lat_res / lat_res, 0) #number of pixels in lat
    lon_n_pixels <- round(guide_lon_res / lon_res, 0) #number of pixels in lon
    
    corse_pixel <- rep(-9999, 14600) # dummy object to be filled inside the loop
    
    #p=28
    for (p in 1:length(guide[,1])){ #for every pixel from the coarse file #####
      #print(p)
      guide_lon <- guide[p,4] # get central coordinates (lon flip)
      guide_lat <- guide[p,2] # get central coordinates
      
      X <- round((guide_lon - lon[1]) / lon_res,0) + 1 # finding central coordinate in prism nc coordinates
      Y <- as.double(round(lat_lenght - (guide_lat - tail(lat,1))/lat_res)) # finding central coordinate in prism nc coordinates
      
      X_list <- seq(guide_lon -(0.5 * models[m,25]), guide_lon + (0.5 * models[m,25]),lon_res) # Creating a grid of all prism pixels inside the course pixel
      Y_list <- seq(guide_lat -(0.5 * models[m,24]), guide_lat + (0.5 * models[m,24]),lat_res)
      
      X_Y_list <- expand.grid(X_list, Y_list)
      
      # Getting all pixels from prism (fine) that are inside each GCM pixel (coarse)
      pixel <- foreach(c = 1:length(X_Y_list[,1]), .combine=cbind, .multicombine = T, ##
                      .packages = c("ncdf4")) %dopar% {
                          
        X_ <- round((X_Y_list[c,1] - lon[1]) / lon_res,0) + 1 
        Y_ <- as.double(round(lat_lenght - (X_Y_list[c,2] - tail(lat,1))/lat_res))
        
        
        pixel_1 <- array1[X_,Y_,1:1825]
        pixel_2 <- array2[X_,Y_,1:1825]
        pixel_3 <- array3[X_,Y_,1:1825]
        pixel_4 <- array4[X_,Y_,1:1825]
        pixel_5 <- array5[X_,Y_,1:1825]
        pixel_6 <- array6[X_,Y_,1:1825]
        pixel_7 <- array7[X_,Y_,1:1825]
        pixel_8 <- array8[X_,Y_,1:1825]
        
        pixel_ <- c(pixel_1, pixel_2, pixel_3, pixel_4, pixel_5, pixel_6, pixel_7,pixel_8)
        
        cbind(pixel_)
        
      }
      
      # averaging values
      pixel <- as.data.frame(pixel)
      pixel <- rowMeans(pixel, na.rm = TRUE)
      corse_pixel <- cbind(corse_pixel, pixel)
      
      }
    
    print("creating NetCDF")
    print(Sys.time())
    #Organizing "data" to fit it in array
    data <- as.data.frame(corse_pixel[,-1])
    rownames(data) <- as.character(1:length(data[,1])) #naming rows
    colnames(data) <- as.character(1:length(data)) #naming columns
    data <- t(data) # transposing data
    
    #defining dimensions for the array
    LON_n <- length(unique(guide$lon_flip)) 
    LAT_n <- length(unique(guide$lat))
    TIME_n <- 14600
    
    #creating the Array
    data_array <-  array(data, dim = c(LON_n, LAT_n, TIME_n))
    
    #naming dimension
    nc_name <- paste0("prism_",var,"_day_",model,"_resampled.nc")
    dim_name <- variables[v,3]
    dim_long_name <- variables[v,5]
    dim_units <- variables[v,7]
    
    ##defining dimensions
    lon_dim <- ncdim_def("lon", units = "degrees_east", longname = "Longitude", vals = unique(guide$lon_flip))
    lat_dim <- ncdim_def("lat", units = "degrees_north", longname = "Latitude", vals = unique(guide$lat))
    time_dim <- ncdim_def("time", units = "days", longname = "days since 19810101", vals = seq(1,14600,1))
    
    #defining variable and compression
    variable_dim <- ncvar_def(name = dim_name, units = dim_units, list(lon_dim, lat_dim, time_dim),
                              missval =  -9999,longname = dim_long_name, prec = "double", compression = 9)
    
    #creating empty NetCDF
    nc_out <- nc_create(nc_name,variable_dim)
    
    #adding variable to NetCDF
    ncvar_put(nc_out, variable_dim, data_array)
    
    #adding global metadata to NetCDF
    ncatt_put(nc_out, 0, "Name", paste0("prism NetCDF resampled to ",model," model resolution"),"character")
    ncatt_put(nc_out, 0, "Version", "NA")
    ncatt_put(nc_out, 0, "Author", "Andre Geraldo de Lima Moraes","character")
    ncatt_put(nc_out, 0, "Institution", "Utah State University, Watershed Sciences Department","character")
    ncatt_put(nc_out, 0, "Adress", "5210 Old Main Hill, NR 210, Logan, UT 84322","character")
    ncatt_put(nc_out, 0, "email", "andre.moraes@usu.edu","character")
    ncatt_put(nc_out, 0, "Description", paste("This is the same as the prism data, but in NetCDF format and resampled to ",model," model resolution"),"character")
    ncatt_put(nc_out, 0, "lineage", "Parameter-elevation Relationships on Independent The Slopes Model (PRISM 4K) project (https://prism.oregonstate.edu/)", "character")
    ncatt_put(nc_out, 0, "License", "Same as PRISM","character")
    ncatt_put(nc_out, 0, "fees", "This data set is free","character")
    ncatt_put(nc_out, 0, "Disclaimer", "While every effort has been made to ensure the accuracy and completeness of the data, no guarantee is given that the information provided is error-free or that the dataset will be suitable for any particular purpose. Users are advised to use this dataset with caution and to independently verify the data before making any decisions based on it. The creators of this dataset make no warranties, express or implied, regarding the dataset's accuracy, reliability, or fitness for a particular purpose. In no event shall the creators be liable for any damages, including but not limited to direct, indirect, incidental, special, or consequential damages, arising out of the use or inability to use the dataset. Users of this dataset are encouraged to properly cite the dataset in any publications or works that make use of the data. By using this dataset, you agree to these terms and conditions. If you do not agree with these terms, please do not use the dataset.","character")
    
    #closing NetCDF
    nc_close(nc_out)
    
  }
  
}

#Fim