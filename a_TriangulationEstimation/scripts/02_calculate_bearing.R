#========================================================================#
# calculate bearings based on a custom made cosinus function             #
# (Bearing_calc_cos.R)                                                   #
# import: calibrated, filtered data                                      #
# export: list of data including bearings                                #
#                                                                        #
# Markus Milchram                                                        #
#                                                                        #
# markus.milchram@boku.ac.at                                             #
#========================================================================#

#load necessary packages and source scripts
library(data.table)
library(RollingWindow)
library(tidyverse)
library(parallel)
library(doParallel)
library(foreach)

source("./scripts/fun/time_match_logger_tRackIT.R")
source("./scripts/fun/Bearing_calc_cos.R")
source("./scripts/fun/hampel_time_tRackIT.R")

#set working directory
setwd("./01_TriangulationEstimation")

##read filtered data and stations data with information about stations (orientation of antennas, coordinates)
data_filtered<-fread("./results/data_calibrated_filtered.csv",data.table = FALSE)
stations<-read.csv("./data/Antennas.csv",sep=";",dec=",")

##replace receiver values with 0,1,2,3 (north, east, south, west) in both data_filtered and stations
data_filtered$receiver[grepl("r000", data_filtered$receiver)]<-0
data_filtered$receiver[grepl("r090", data_filtered$receiver)]<-1
data_filtered$receiver[grepl("r180", data_filtered$receiver)]<-2
data_filtered$receiver[grepl("r270", data_filtered$receiver)]<-3


stations$Name[grepl("r000", stations$Name)]<-0
stations$Name[grepl("r090", stations$Name)]<-1
stations$Name[grepl("r180", stations$Name)]<-2
stations$Name[grepl("r270", stations$Name)]<-3


##split data into list based on individuals
data_list<-split(data_filtered,f=data_filtered$freq_tag)

##calculate time matches with a function authored by Jannis Gottwald (tRackIT)----
data_timematch<-mclapply(data_list, function(x) time_match_logger_tRackIT(data=x),mc.cores=6)
##save the results
#saveRDS(data_timematch,"./results/timematched.rds")
#data_timematch<-readRDS("./results/timematched.rds")


##loop through station names, calculate bearings and store into a final list----
#this takes 5.25 minutes on a 2023 Macbook Pro with 32 GB RAM, 6 performance cores and an Apple M2 Pro chip,
#so read the saved output instead

bearings_raw<-mclapply(data_timematch,function(x)bearing_calc_cos(x),mc.cores=6)
saveRDS(bearings_raw,"./results/bearings_raw.rds")
bearings_raw<-readRDS("./results/bearings_raw.rds")
names(bearings_raw)

##change time stamp to POSIXct UTC
bearings_raw_utc<-mclapply(bearings_raw,function(x){
  x<-x %>% mutate(timestamp=as.POSIXct(timestamp,tz="UTC"))
},mc.cores=6)

##in 2022, Station "paur" has its bearings shifted by 17° - add this value and modulo
add_17<-function(x){
  x$resultant_angle[x$Name=="paur"&as.Date(x$timestamp)<"2023-01-01"]<-(x$resultant_angle[x$Name=="paur"&as.Date(x$timestamp)<"2023-01-01"]+17) %% 360
  return(x)
}

bearings_raw_corr<-lapply(bearings_raw_utc,function(x)add_17(x))

##loop through bearings_raw_corr and save station specific .rds in results
list_stations<-unique(stations$Station)
for(i in list_stations){
  bearings_name<-i
  bearing_station<-lapply(bearings_raw_corr,function(x){
    x<- x %>% dplyr::filter(Name==bearings_name)
  })
  saveRDS(bearing_station,paste("./results/bearings_stations/","bearings_",bearings_name,".rds",sep=""))
}

##loop through names and stations, apply Hampel filter to mitigate outliers----

##sliding window of 10, set t0 (threshold for median filter) to 0.5
list_stations<-unique(stations$Station)

for(i in unique(list_stations)) {
  #input name
  bearings_name<-paste("./results/bearings_stations/","bearings_",i,".rds",sep="")
  #output name
  bearings_hampel_name<-paste("./results/bearings_hampel/","bearings_hampel","_",i,".rds",sep="")
  bearings<-readRDS(bearings_name)
  #apply hampel filter
  bearings_hampel<-mclapply(bearings, function(x) hampel_time(data=x, col = "resultant_angle", k = 10,t0=0.5),mc.cores=6)
  #save output as .rds
  saveRDS(bearings_hampel, bearings_hampel_name)
  print(bearings_hampel_name)
}


##aggregate bearings to time intervals of 2 seconds, to allow time difference between stations----
#and add number of antennas with received signal as column

#function for adding column with number of antennas
calc_numberant<-function(x){
  x[,3:6]<-x[,3:6] %>% mutate_if(is.numeric,function(x)fifelse(x>0,1,0))
  x$number_antennas<-rowSums(x[,c("N","E","S","W")])
  return(x)
}

#function for aggregating data to 2 second intervals
calc_aggregate <- function(x){
  x<-na.omit(x)
  if(nrow(x)>0){
    y<-setNames(aggregate(cbind(bearings_filtered,number_antennas)~cut(timestamp,"2 secs"),data=x,median),c("timestamp","bearings_filtered","number_antennas"))
    return(y)
  } else{
    y<-x[,c("timestamp","bearings_filtered","number_antennas")]
    return(y)
  }
}


#load single lists, add number of antennas, aggregate to 2 second intervals
names<-c("BOKU_01","BOKU_02","BOKU_03","BOKU_04","BOKU_05","BOKU_06","BOKU_07","BOKU_09","BOKU_10","paur","BOKU_04_2023")

registerDoParallel(cores=6)
foreach(i = names[5:11],.inorder=FALSE) %dopar% {
  inputname<-paste("./results/bearings_hampel/bearings_hampel_",i,".rds",sep="")
  input<-readRDS(inputname)
  #calculate the number of antennas at each signal detection event
  output<-lapply(input,function(x)calc_numberant(x))
  print(inputname)
  #aggregate to 2 seconds intervals
  output2<-lapply(output,function(x)calc_aggregate(x))
  print(inputname)
  #convert factor timestamp to as.POSIXct
  output2<-lapply(output2,function(x)mutate(x,Name=i,timestampposix=as.POSIXct(timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC")))
  print(inputname)
  #omit NAs
  output2<-lapply(output2,function(x)na.omit(x))
  saveRDS(output2,paste("./results/bearings_final/bearings_final_",i,".rds",sep=""))
  print(inputname)
}

###combine lists to single list with IDs of individuals as names----
#load single lists
BOKU_01<-readRDS("./results/bearings_final/bearings_final_BOKU_01.rds")
BOKU_02<-readRDS("./results/bearings_final/bearings_final_BOKU_02.rds")
BOKU_03<-readRDS("./results/bearings_final/bearings_final_BOKU_03.rds")
BOKU_04<-readRDS("./results/bearings_final/bearings_final_BOKU_04.rds")
BOKU_05<-readRDS("./results/bearings_final/bearings_final_BOKU_05.rds")
BOKU_06<-readRDS("./results/bearings_final/bearings_final_BOKU_06.rds")
BOKU_07<-readRDS("./results/bearings_final/bearings_final_BOKU_07.rds")
paur<-readRDS("./results/bearings_final/bearings_final_paur.rds")
BOKU_09<-readRDS("./results/bearings_final/bearings_final_BOKU_09.rds")
BOKU_10<-readRDS("./results/bearings_final/bearings_final_BOKU_10.rds")
BOKU_04_2023<-readRDS("./results/bearings_final/bearings_final_BOKU_04_2023.rds")


##combine lists to single lists
stations_combined<-list()
#loop through station names and bind them according to the individual IDs
for (i in names(BOKU_01)){
  stations_combined[[i]]<-rbind(BOKU_01[[i]],BOKU_02[[i]],BOKU_03[[i]],BOKU_04[[i]],BOKU_05[[i]],BOKU_06[[i]],BOKU_07[[i]],paur[[i]],BOKU_04_2023[[i]])
}

#save output as .rds
saveRDS(stations_combined,"./results/bearings_final.rds")

