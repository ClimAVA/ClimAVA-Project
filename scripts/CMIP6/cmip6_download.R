########################################
# Author - Andre Moraes
# Utah State University
# QCNR - Watershed Sciences Department
# andre.moraes@usu.edu

# This script is used to download cmip6 models.

################################################################
# 
# Tree data frames were built in preparation for this script to run.
# - the models data frame contains almost all information necessary to build the
# URLs to download the files through HTTP download
# - the historical and future data frames have the initial and final dates for 
# each file for each model
# 
################################################################

#loading guide data frames
future_dates <- read.csv("~/Documents/scripts/cmip6/2023/future_dates_2023.csv") # all dates for ssp245
historical_dates <- read.csv("~/Documents/scripts/cmip6/2023/historical_dates_2023.csv") # all dates for historical
models <- read.csv("~/Documents/scripts/cmip6/2023/models_2023.csv") #has all the other information necessary to create the HTTPs
ssps <- c("historical","ssp245","ssp370","ssp585")
vars <- c("pr", "tasmax", "tasmin")

# changing timeout for  downloads
getOption("timeout")
options(timeout = 10000)

#########################################################################
#####Creating directories--------
#getwd()
# setwd("/scratch/general/vast/u6047395/cmip6")
# for (i in 1:3){
#    v = vars[i]
#    dir.create(v)  
# }
#  for (i in 1:3){
#    v = vars[i]
#    for (m in 1:25){
#      model = models[m,1]
#      dir = paste0(v,"/",model)
#      dir.create(dir)
#   }
# }
# # 
# for (i in 1:3){
#   v = vars[i]
#   for (m in 1:25){
#     model = models[m,1]
#     for (s in 1:4){
#       ssp = ssps[s]
#       dir = paste0(v,"/",model,"/",ssp)
#       dir.create(dir)
#     }
#   }
# }
####################################################################

for (i in 1:3){
  var = vars[i]
  
  for (m in 17){ #1:22 #### just first model first
    inst_id = models[m,3]
    model = models[m,1]
    realization = models[m,4]
    grid = models[m,5]
    node = models[m,20]
    dataroot = models[m,21]
    path_hist = models[m,22]
    path_ssp = models[m,23]
    
    for (s in 1:4){
      ssp = ssps[s]
      
      if(ssp == "historical") {dates_n = models[m,6]} # number of files
      if(ssp == "ssp245") {dates_n = models[m,7]}
      if(ssp == "ssp245") {dates_n = models[m,7]}
      if(ssp == "ssp585") {dates_n = models[m,7]}
      
      if(ssp == "historical") {dates = historical_dates[1:dates_n,m]}# creating a vector of the dates
      if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {dates = future_dates[1:dates_n,m]}
      
      
      if(ssp == "historical") {vv = c(8,12,16)} # columns whee the versions are 
      if(ssp == "ssp245") {vv = c(9,13,17)}
      if(ssp == "ssp370") {vv = c(10,14,18)}
      if(ssp == "ssp585") {vv = c(11,15,19)}
      
      if(var == "pr") {vvv = vv[1]}
      if(var == "tasmax") {vvv = vv[2]}
      if(var == "tasmin") {vvv = vv[3]}
      
      version = models[m,vvv]
      version <- paste0("v",version)
      
      for (d in 1:length(dates)){
        date = dates[d]
        
        if(ssp == "historical"){
          
          url <- paste0("http://",node,"/thredds/fileServer/",dataroot,"/",path_hist,"/",inst_id,"/",model,"/",ssp,"/",realization,"/day/",var,"/",grid,"/",version,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")
          destfile <- paste0("/scratch/general/vast/u6047395/cmip6/",var,"/",model,"/",ssp,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")
          tem <- file.exists(destfile)
          print(destfile)
          print(tem)
          
          if(tem == FALSE){
            download.file(url, destfile, mode = "wb")
          }
        }
        if(ssp == "ssp245" |ssp == "ssp370" |ssp == "ssp585"){
          
          url <- paste0("http://",node,"/thredds/fileServer/",dataroot,"/",path_ssp,"/",inst_id,"/",model,"/",ssp,"/",realization,"/day/",var,"/",grid,"/",version,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")
          #url <- paste0("http://",node,"/thredds/fileServer/",dataroot,"/",path_ssp,"/DKRZ/",model,"/",ssp,"/",realization,"/day/",var,"/",grid,"/",version,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")# for MPI-ESM1-2-HR
          destfile <- paste0("/scratch/general/vast/u6047395/cmip6/",var,"/",model,"/",ssp,"/",var,"_day_",model,"_",ssp,"_",realization,"_",grid,"_",date,".nc")
          tem <- file.exists(destfile)
          print(destfile)
          print(tem)
          
          if(tem == FALSE){
            download.file(url, destfile, mode = "wb")
          }
        }        
      }               
    }
  }
}

#example <- "http://esgf.rcec.sinica.edu.tw/thredds/fileServer/my_cmip6_dataroot/ScenarioMIP/AS-RCEC/TaiESM1/ssp245/r1i1p1f1/day/pr/gn/v20210222/pr_day_TaiESM1_ssp245_r1i1p1f1_gn_20150101-20241231.nc"

# Fim