# ClimAVA
# Andre Moraes
# andre.mortaes@usu.edu
# Utha State University
# Watershed Sciences Department

#this script has the porpuse of help users dowload ClimAVA data trhough HTTP download

#library(doParallel) #load this package if you are paralelizing this job

# the following files are used to facilitate the downloading of the data, we will loop trhough them
# all of them all available in the guides folder in climava website under "/ClimAVA_sw_scripts/guides."

models <- read.csv("~/Documents/scripts/cmip6/2023/models_2023.csv") #has all the other information necessary to create the HTTPs
variables <- read.csv("~/Documents/scripts/cmip6/2023/Variables.csv")
ssps <- c("historical","ssp245","ssp370","ssp585")

##### Creating directories--------

# chose where you want to save your data

my_directory <- "/scratch/general/vast/u6047395/ClimAVA/" # put your path here

getwd() # checking where am I
setwd(my_directory) # seting the directory to whre I wnat to save my data
getwd() # Am I there?
list.files() # are there files in this directory

# the following loops will create the folder where you are going to save your data

for (m in 1:17){
 model = models[m,1]
 dir.create(model)
}

for (m in 1:17){
 model = models[m,1]
 for (s in 1:4){
   ssp =ssps[s]
   dir = paste0(model,"/",ssp)
   dir.create(dir)
 }
}

for (m in 1:17){
 model = models[m,1]
 for (s in 1:4){
   ssp =ssps[s]
   for(v in 1:3){
     var = variables[v,3]
     dir = paste0(model,"/",ssp,"/",var)
     dir.create(dir)
   }
 }
}

# Doloading the data

# basically, this a series of nested loops that will create the links to download the data and save them on the right folder in your machine


#Changing downloading time out - this will avoid crashes in case your internet is slow
getOption("timeout")
options(timeout = 1000000)


for (v in 1:3){ #loop through variables
  
  var <- paste(variables[v,3])
  print(var)
  
  for (s in 1:4){ #loop through historical and 3 scenarios
    
    ssp = ssps[s]
    print(ssp)
    
    
    if(ssp == "historical") {files <- read.csv("~/Documents/scripts/2024_clean/guides/files_DS_hist.csv")}# loading guide for files
    if(ssp == "ssp245"|ssp == "ssp370"|ssp == "ssp585") {files <- read.csv("~/Documents/scripts/2024_clean/guides/files_DS_future.csv")}
    
    
    for(m in 1:17) { #loop trhough models
      
      model = models[m,1]
      v_label <- paste(models[m,4])
      print(model)
      
      #registerDoParallel(50) # feel free to paralelize the code if you can/want
      
      #foreach (f = 1:length(files[,1])) %dopar% { 
      
      for ( f in 1:length(files[,1])){           
       
       dates_out <- files[f,5]
       
       #Creating the HTTP link
       
       HTTP <- paste0("https://home.chpc.utah.edu/~u6047395/ClimAVA_sw_data/",model,"/",ssp,"/",var,"/ClimAVA-SW_",model,"_",ssp,"_",var,"_",v_label,"_",dates_out,".nc")
       
       # creating path to save the data in your machine
       out_file_name <- paste0(my_directory,"/",model,"/",ssp,"/",var,"/ClimAVA-SW_",model,"_",ssp,"_",var,"_",v_label,"_",dates_out,".nc")
       
       #downloading the data if you dindt dowload it yet
       
       test <- file.exists(out_file_name) # checking if file already exists
       
       if (test == FALSE) {download.file(HTTP, out_file_name, mode = "wb")}
       
      }
    }  
  }               
}

# Fim

HTTP <- "https://home.chpc.utah.edu/~u6047395/ClimAVA_sw_data/ACCESS-CM2/historical/pr/ClimAVA-sw_ACCESS-CM2_historical_pr_r1i1p1f1_19810101-19851231.nc"







https://home.chpc.utah.edu/~u6047395/ClimAVA_sw_data/ACCESS-CM2/historical/pr/ClimAVA-SW_ACCESS-CM2_historical_pr_r1i1p1f1_19810101-19851231.nc



for (s in 1:length(stations_logan$ID)){#header of a for loop that will have "i" running from 1 to the length of Stations_logan
  
  S <- stations_logan[s,1]
  
  HTTP <- paste0("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/",S,".csv.gz")
  out_file_name <- paste0("C:/Users/andre/Box/usu_teaching/base_r/baser_f_2023/data/class_data/station_data/",S,".csv.gz") #path for file
  
  download.file(HTTP, out_file_name, mode = "wb")# downloading data
  
}









