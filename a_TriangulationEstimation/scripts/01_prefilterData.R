#========================================================================#
# inspect raw data to filter them by intervals between timestamps        #
# import: raw but calibrated data from telemetry stations                #
# export: data filtered by interval between signals                      #
#                                                                        #
# Markus Milchram                                                        #
#                                                                        #
# markus.milchram@boku.ac.at                                             #
#========================================================================#

#required packages
library(data.table)
library(tidyverse)
library(ggplot2)

#set working directory
setwd("./a_TriangulationEstimation")

#read calibrated data
data_cal<-fread("./data/data_calibrated.csv")

##all intervals between two consecutive signals
##have to be between 0.8 and 2 s according to the transmitters' manufacturer,
##so roughly filter data accordingly

data_cal<-data_cal %>% dplyr::filter(td>0.8 & td<2)

#create plots with subset of timestamps, look at noise and specify time interval
#between signals (true signals should be aligned in a horizontal line)
#then manually enter time intervals into excel table (2022_timedifference.csv
#and 2023_timedifference.csv)
ggplot(head(data_cal[data_cal$freq_tag=="S_BEATE",],100),aes(x=timestamp,y=td))+
  geom_point(size=2)+
  scale_y_continuous(limits=c(1,1.5))


###filter data with wrong time difference between signals----

#import 2022_timedifference.csv and 2023_timedifference.csv and bind them
td2022<-fread("./data/2022_timedifference.csv",sep=";",dec=",")
td2023<-fread("./data/2023_timedifference.csv",sep=";",dec=",")
td<-rbind(td2022,td2023)

#merge data_cal and td for filtering
data_td<-merge(data_cal,td,by.x="freq_tag",by.y="Name",all.x=TRUE)
#exclude noise according to time difference information
data_td_filtered<-data_td %>% filter(td>td_start&td<td_stop)

##write finally filtered data
fwrite(data_td_filtered,"./results/data_calibrated_filtered.csv")



