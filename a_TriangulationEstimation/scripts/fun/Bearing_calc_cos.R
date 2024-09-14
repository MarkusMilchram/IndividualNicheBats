#to speed it up I reduced the options

#Script including the correction using the acos function
#load libraries
library(tidyverse) # just used for the read_csv underneath
library(circular)

####helper function
##### ##########################
#for test: x=0.2; y=-0.8
helper_bcos_speedy<- function(x, y, dbLoss_fact) {
  x_ <- sign(x);y_ <- sign(y);x <- abs(x);y <- abs(y)

  if (x >= y) {
    a <- x - (x - y) * dbLoss_fact
    b <- y
  } else {
    a <- x
    b <- y - (y - x) * dbLoss_fact
  }

  ab_ <- a + b
  a <- a / ab_
  b <- b / ab_

  x_corr <- a * x_
  y_corr <- b * y_

  return(c(x_corr,y_corr))
}


###helper function for inner loop
speedy_bearing_2ant<-function(v_l,rad_ant_a=rad_ant_a,dbLoss=dbLoss) {
  #error_count <<- error_count + 1
  #print(error_count)

  df<-data.frame(v_l,rad_ant_a)
  df<-df[order(-df$v_l),]
  v_l<-df$v_l[1:2]
  rad_ant_a2<-df$rad_ant_a[1:2]

  x_comp<- v_l * cos(as.numeric(rad_ant_a2))
  y_comp<- v_l * sin(as.numeric(rad_ant_a2))

  # Sum the components for each axis
  sum_x <- round(sum(x_comp),digits=5)
  sum_y <- round(sum(y_comp),digits=5)

  if (sum_x + sum_y == 0) {return(NA)}
  else {

  ######################
  #here add a correction
  ######################
  #standardize the lengths
  totv<-abs(sum_x)+abs(sum_y)+abs(dbLoss) # I try to write the dBloss in here, because after the helper function they will added up to 1 again

  #get the correction factor
  sum_x_fact<-sum_x/totv
  sum_y_fact<-sum_y/totv
  dbLoss_fact<-dbLoss/totv

  ####insert correction
  corr.<-helper_bcos_speedy(sum_x_fact,sum_y_fact,dbLoss_fact)
  sum_x_fact<-corr.[1];  sum_y_fact<-corr.[2]

  Deltaalph<-abs(sum_x_fact)-abs(sum_y_fact)

  if (is.na(sum_x_fact) | is.na(sum_y_fact)) {
    warning("Warning: Either sum_x_fact or sum_y_fact is NA. Setting Deltaalph to zero.")
    Deltaalph <- 0  # or any other default value you want
  } else {
    Deltaalph <- abs(sum_x_fact) - abs(sum_y_fact)
    if (is.na(Deltaalph) | Deltaalph == 1) {
      warning("Warning: Deltaalph is NA or one.")
      Deltaalph <- 0  # or any other default value you want
    }
  }

  #get a correction angle


  #Deltaalph<-((sum_y_fact)-(sum_x_fact))/dbLoss
  # Deltaalph<-abs((abs(sum_x_fact)-abs(sum_y_fact))/dbLoss)


  if(sum_x_fact==0 | sum_y_fact==0) {Deltaalph<-(abs(sum_x_fact)-abs(sum_y_fact))}


  acos_res_temp<-acos(Deltaalph) * 90 / pi



  if(acos_res_temp==0 | acos_res_temp==90) {acos_res <- deg(atan2(sum_y_fact, sum_x_fact))} else {


    if(sum_y_fact>0  & sum_x_fact>0) {acos_res<-acos_res_temp}

    if(sum_y_fact>0  & sum_x_fact<0) {acos_res<-180-acos_res_temp}

    if(sum_y_fact<0  & sum_x_fact<0) {acos_res<-180+acos_res_temp}

    if(sum_y_fact<0  & sum_x_fact>0) {acos_res<-360-acos_res_temp}}


  # corr.fact<-(abs((abs(sum_x/totv)-abs(sum_y/totv))))/dbLoss
  # corr.fact.cos2<-deg(acos(corr.fact)) # this is twice the number below
  #
  # corr.fact.cos<-acos(corr.fact)* 90 / pi #+ L #l means orientation to the left

  # Length of the resultant vector
  #a_dat$resultant_length[i] <- sqrt(sum_x^2 + sum_y^2)

  #angle in degree
  return(round(acos_res%%360,digits = 4))}## reduces to modulo 360 and saves the outoput

}


#####################################
#here is the new function
# all antennas are 90° to the next in the order N, E, S, W inout strength needs to be placed in columns 3:6.
# the column names are hard coded and need to be :  c("timestamp","Name","0","1","2","3") (column order can be changed)
#######################
bearing_calc_cos<-function(a_dat,ref_angle=0,dbLoss=14) {

  require(circular) # require the package cirular

    a_dat<-data.frame(a_dat$timestamp,a_dat$Name,a_dat$"0",a_dat$"1",a_dat$"2",a_dat$"3")
    colnames(a_dat)<-c("timestamp","Name","N","E","S","W")
  #rename the 0, 1, 2, 3 in N, E , S, W
  print(colnames(a_dat))


  #replace NAs with 0 in antenna columns
  a_dat[,3:6][is.na(a_dat[,3:6])] <- 0


  #define the orientation of the antennas
  r_a<-ref_angle # this is the reference angle, all other are adjusted accordingly

  r_a<-as.circular(r_a,units="degrees",type = c("angles"),
                   template = c("none"),
                   modulo = c("asis"),
                   zero = 0, rotation = c("counter"))

  ant_a<-c(r_a,r_a+90,r_a+180,r_a+270) #antenna angles - it is assumed that the columns 0, 1, 2, 3 are antenna N, E , S, W (or a bit off but in this order)

  #calculate mean vector out of the 4 vectors
  rad_ant_a<-as.numeric(rad(ant_a))

  #this loops through each line 

  a_dat<-data.frame(a_dat,rep("NA",nrow(a_dat))) # this is just because in the loop below I add another column with the results to this data frame

  colnames(a_dat)[7]<-c("resultant_angle")

  tadat<-t(a_dat[,3:6])

  a_dat$resultant_angle<-apply(tadat,2, function(x) speedy_bearing_2ant(x,rad_ant_a=rad_ant_a,dbLoss=dbLoss))

  return(a_dat)
}#end of function


