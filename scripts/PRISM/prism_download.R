########################################
# Author - Aandre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to download 4km daily PRISM data.

############################################ ----------------

# loading PRISM package ----
library(prism) 

#do par parameters
library(doParallel) # loading doParallel package
registerDoParallel(64) #setting number of cores

#creating files for loops----

library(lubridate) # Loading lubridate package to handle dates

dates <- as.character(seq(ymd("1981-01-01"), ymd("2020-12-31"), by = "day")) # sequence of of years with starting days
L = length(dates)

variables <- c("ppt", "tmax", "tmin") # vector with variables names


#Creating directories ----

#for (d in 1:length(variables)){
#  dir.create(paste0("/scratch/general/vast/u6047395/prism/raw_1981-2020_",variables[d]))
#}

# downloading ----

for (d in 1:length(variables)){
  
  # setting download directory
  prism_set_dl_dir(paste0("/scratch/general/vast/u6047395/prism/raw_1981-2020_",variables[d])) 
  
foreach(i = 1:L, .packages = c("prism")) %dopar% {

#for (i in 1:L) {

  S = dates[i] #start date
  s = gsub("-","", S) # date without dash
  print(s) 
  
  #file_name is used to test if file was already downloaded. This avoids trying to download the same file twice. See if statement below.
  file_name <- paste0("/scratch/general/vast/u6047395/prism/raw_1981-2020_",variables[d],"/PRISM_",variables[d],"_stable_4kmD2_",s,"_bil/PRISM_",variables[d],"_stable_4kmD2_",s,"_bil.bil")
  file_exists <- file.exists(file_name)
  print(file_exists)
  #Download files if they were not donwloaded yet.
  if(file.exists(file_name) == FALSE){
  
    get_prism_dailys(type = variables[d], minDate = S, maxDate = S,
                   keepZip = F) # downloading files
    }
  }
}

# Checking if all files were downloaded. Result should be = 14610
length(list.files("/scratch/general/vast/u6047395/prism/raw_1981-2020_ppt"))
length(list.files("/scratch/general/vast/u6047395/prism/raw_1981-2020_tmax"))
length(list.files("/scratch/general/vast/u6047395/prism/raw_1981-2020_tmin"))

#Fim
