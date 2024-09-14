#========================================================================#
# Calculate triangulations with an azimuthal telemetry model             #
# import: bearings from 02_calculate_bearing.R                           #
# export: final triangulations                                           #
#                                                                        #
# Markus Milchram                                                        #
#                                                                        #
# markus.milchram@boku.ac.at                                             #
#========================================================================#

##load libraries----
library(razimuth)
library(parallel)
library(sp)
library(terra)
library(tidyverse)
library(lubridate)
library(data.table)
library(foreach)
library(fasttime)
library(stringr)

#set working directory
setwd("./a_TriangulationEstimation")

#load data
bearings<-readRDS("./results/bearings_final.rds")
#remove column "timestamp"
bearings<-mclapply(bearings,function(x){
  x$timestamp<-NULL
  return(x)
},mc.cores=6)


#add stations data and project them to UTM
antennas2022<-fread("./daten/2022_Antennas.csv",data.table=FALSE,dec=",")
antennas2023<-fread("./daten/2023_Antennas.csv",data.table=FALSE,dec=",")
antennas<-rbind(antennas2022,antennas2023)

#group by Station ID to get a dataframe with station ID and coordinates 
antennas<-antennas %>% group_by(Station) %>% summarize(Lat_stat=unique(Latitude),Lon_stat=unique(Longitude))

#delete Individual "Kunigunde" because it has no data
bearings$K_Kunigunde<-NULL

#bind bearings to make calculations easier
bearings_bind<-bind_rows(bearings,.id="indiv")

#ceiling of the number of antennas and convert them to integer
bearings_bind$number_antennas<-as.integer(ceiling(bearings_bind$number_antennas))

#create Spatial points from coordinate information
cord.dec<-SpatialPoints(cbind(antennas$Lon_stat,antennas$Lat_stat),proj4string=CRS("+proj=longlat"))
#transform coordinates to UTM 33N
cord.UTM<-spTransform(cord.dec,CRS("epsg:25833"))
#bind UTM coordinates with original stations data frame
antennas_utm<-cbind(antennas,cord.UTM)
#rename columns with coordinate information to X and Y coordinates
antennas_utm<-dplyr::rename(antennas_utm,utm_x=coords.x1,utm_y=coords.x2)
#remove columns not needed in further analyses
antennas_utm<-antennas_utm %>% dplyr::select(-c(Lat_stat,Lon_stat,optional))

#merge antennas and bearings
bearings_ant<-merge(bearings_bind,antennas_utm,by.x="Name",by.y="Station")

###add prior (estimates from field tests)----
priors_antenna<-fread("./daten/priors_antennas.csv",data.table=FALSE)
#the prior depends on the number of antennas. One antenna receives the signal farther than
#two antennas - thus, we differentiate between one and two antennas
bearings_ant$number_antennas[bearings_ant$number_antennas>1]<-2

#merge bearings and prior data frame
bearing_grouped_prior<-merge(bearings_ant,priors_antenna,by=c("Name","number_antennas"),all.x=TRUE,all.y=FALSE)


##add column with respective recording night----
#function to add recording night
add_night<-function(x){
  x$night<-as.Date(x$timestampposix)
  x$night<-fifelse(strftime(x$timestampposix,format="%H:%M:%S",tz="UTC")>="00:00:00"&strftime(x$timestampposix,format="%H:%M:%S",tz="UTC")<"12:00:00",x$night-1,x$night)
  return(x)
}

bearing_grouped_prior_night<-add_night(bearing_grouped_prior)

##split data frame into a list where each element accords to an individual night
#add column with individual night
bearing_grouped_prior_night$indiv<-paste(bearing_grouped_prior_night$night,bearing_grouped_prior_night$indiv,sep="_")
#split data frame into list
bearing_grouped_prior<-split(bearing_grouped_prior_night,f=bearing_grouped_prior_night$indiv)
#save as .rds
saveRDS(bearing_grouped_prior,"./results/bearing_grouped_prior.rds")

#calculate observation ID for triangulation based on timestamps----
calc_group_id<-function(x){
  x<-setDT(x)
  x$obs_id<-as.factor(x$timestampposix)
  levels(x$obs_id)<-1:length(levels(x$obs_id))
  x<-x[order(x$obs_id),]
  x$obs_id<-as.numeric(x$obs_id)
  #rename columns required for the razimuth package
  x<- x %>% rename(azimuth=bearings_filtered,prior_r=prior,date=timestampposix)
  #remove "Name" column
  x<-x %>% dplyr::select(-(Name))
  return(as.data.frame(x))
}

bearing_grouped_prior_te<-mclapply(bearing_grouped_prior,function(x)calc_group_id(x),mc.cores = 6)

#save .rds of bearing_grouped_prior
saveRDS(bearing_grouped_prior_te,"./results/bearing_grouped_prior_list.rds")
bearing_grouped_prior_te<-readRDS("./results/bearing_grouped_prior_list.rds")

##convert data to atm object----
#CAVE: this takes 2.6 hours on a Macbook Pro 2023 with 32 GB RAM and Apple M2 Pro Chip
bearing_atm<- mclapply(bearing_grouped_prior_te,function(x)convert_atm(x),mc.cores = 6)

#save atm object as rds
saveRDS(bearing_atm,"./results/bearing_atm.rds")
bearing_atm<-readRDS("./results/bearing_atm.rds")

###fit azimuthal telemetry model----
#CAVE: this command took 6 days on the Vienna Scientific Cluster 5 with 128 cores and 2 TB RAM,
#so use the .rds outputs to proceed

#registerDoParallel(cores=128)

###fit azimuthal telemetry model
# foreach(i=names(bearing_atm),.packages = "razimuth",.inorder=FALSE) %dopar% {
#   result<-atm_mcmc(bearing_atm[[i]],n_mcmc=10000,n_burn=1000)
#   outputname<-sprintf("./results/atm_fit_nights/data_fit_night_%s.rds",i)
#   saveRDS(result,outputname)
#   print(i)
# }

#create traceplots, and running mean plots of the atm results to inspect the model fit----
#list files in the folder containing the atm fits
atm_fit_names<-list.files("./results/atm_fit_nights/",pattern=".rds",full.names = FALSE)
#remove "data_fit_night_" and "rds"
remove_string<-c("data_fit_night_",".rds")
atm_fit_names<-str_remove_all(atm_fit_names,pattern=paste(remove_string,collapse = "|"))

registerDoParallel(cores=6)

foreach(i=atm_fit_names,.inorder=FALSE) %dopar% {
  #define input name
  inputname<-sprintf("./results/atm_fit_nights/data_fit_night_%s.rds",i)
  #define .pdf name
  mypath<-sprintf("./results/traceplots/diagnostic_%s.pdf",i)
  #read fitted atm
  input<-readRDS(inputname)
  #create .pdf
  pdf(mypath)
  #plot traceplot
  plot_kappa(atm_obj=input$kappa_ls,item="traceplot")
  mtext(names(input),line=0.5)
  #plot density
  plot_kappa(atm_obj=input$kappa_ls,item="density")
  mtext(names(input),line=0.5)
  #plot progress of tuning
  plot_kappa(atm_obj=input$kappa_ls,item="tuning")
  mtext(names(input),line=0.5)
  #plot running mean
  plot_kappa(atm_obj=input$kappa_ls,item="run_mean")
  #name each plot
  mtext(names(input),line=0.5) 
  invisible(dev.off())
}


##check traceplots manually and save good model fits in folder "traceplots_select"

###save location estimates as csv----
#list files in the folder containing the good model fits
atm_traceplot_names<-list.files("./results/traceplots_select/",pattern=".pdf",full.names = FALSE)
#remove "data_fit_night_" and "rds"
remove_string<-c("diagnostic_",".pdf")
atm_traceplot_names<-str_remove_all(atm_traceplot_names,pattern=paste(remove_string,collapse = "|"))

#read atm_fit_names and save location estimates and covariance information. This loop is based
#on a code of Matteo Rizzuto ("Snowshoe hare Triangulation with razimuth", 2021)

foreach(i=atm_traceplot_names,.packages = "data.table",.inorder=FALSE) %dopar% {
  #define input name
  inputname<-sprintf("./results/atm_fit_nights/data_fit_night_%s.rds",i)
  #define output name
  outputname<-sprintf("./results/triangulations/data_triang_%s.csv",i)
  #read fitted atm
  input<-readRDS(inputname)
  #loop through elements of atm list and save posterior location estimates and covariance information
  atm_vars<-
    rbindlist(lapply(input$mu_ls,function(y){
      xy<-y[["pdraws"]]
      data.table(COV.x.y=var(x=xy[,1],y=xy[,2]),pid=y[["pid"]],timestamp=y[["date"]],
                 COV.x.x=var(xy[,1]),COV.y.y=var(xy[,2]))
    }))[,id:=i]
  test_vars<-(atm_vars)
  atm_reloc<-
    rbindlist(lapply(input$mu_ls,function(y){
      xy<-as.matrix(y[["pmode"]])
      data.table(utm_x=xy[1,],utm_y=xy[2,],pid=y[["pid"]])
    }))[,id:=i]
  test_relocs<-(atm_reloc)
  ##join vars and relocations based on the date
  output<-merge(test_vars,test_relocs,by=c("id","pid"))
  fwrite(output,outputname)
}


