#------------------------------------------------------------------------------#
# Preprocess memory and computational data for behavioral sample
#
#------------------------------------------------------------------------------#

categ<-levels(as.factor(RLdata_beh$obj_category))


RLdata_beh$prob_obs<-NA
RLdata_beh$prob_obs_post<-NA
RLdata_beh$expV<-NA
RLdata_beh$expVpre<-NA
RLdata_beh$Delta<-NA
RLdata_beh$Delta_chosen<-NA
RLdata_beh$update<-NA
RLdata_beh$updateV<-NA
RLdata_beh$expVpost<-NA
RLdata_beh$updateAll<-NA#
RLdata_beh$uncertainty<-NA
RLdata_beh$SI_V<-NA

for (n in 1:nrow(RLdata_beh)){
  if (!is.na(RLdata_beh$obj_category[n])){
    RLdata_beh$updateAll[n]<-sum(abs(RLdata_beh[n,c("V1", "V2", "V3", "V4")] -
                                       RLdata_beh[n,c("Vpre1", "Vpre2", "Vpre3", "Vpre4")] 
    ))
    
    RLdata_beh$prob_obs[n]<-RLdata_beh[[paste0("P", which(categ == RLdata_beh$obj_category[n]))]][n] 
    RLdata_beh$expV[n]<-RLdata_beh[[paste0("V", which(categ == RLdata_beh$obj_category[n]))]][n] 
    RLdata_beh$expVpre[n]<-RLdata_beh[[paste0("Vpre", which(categ == RLdata_beh$obj_category[n]))]][n] 
    
    # prediction error chosen
    if(!is.na(RLdata_beh$respCat[n])){
      RLdata_beh$Delta_chosen[n]<-RLdata_beh$accuracy[n] -RLdata_beh[[paste0("Vpre", which(categ == RLdata_beh$respCat[n]))]][n] 
    }
    RLdata_beh$Delta[n]<-1-RLdata_beh[[paste0("Vpre", which(categ == RLdata_beh$obj_category[n]))]][n] 
    RLdata_beh$prob_obs_post[n]<-RLdata_beh[[paste0("Post", which(categ == RLdata_beh$obj_category[n]))]][n] 
    RLdata_beh$updateV[n]<-abs(RLdata_beh$expV[n]-RLdata_beh$expVpre[n])
    RLdata_beh$update[n]<-abs(RLdata_beh$prob_obs_post[n]-RLdata_beh$prob_obs[n])
    #RLdata_beh$updateAll[n]<-sum(RLdata_beh[n,c('V1', 'V2', 'V3', 'V4')] - 
    #   RLdata_beh[n,c('Vpre1', 'Vpre2', 'Vpre3', 'Vpre4')])
    RLdata_beh$uncertainty[n]<-1/(var(as.numeric(RLdata_beh[n,c('Vpre1', 'Vpre2', 'Vpre3', 'Vpre4')]))+0.1)
    
    
    
  }
}


# shannon information (surprise)
RLdata_beh$SI<-NA
RLdata_beh$SI_V<-NA

for (n in 1:nrow(RLdata_beh)){
  # we are taking the prior of the category
  # that is shown
  #1  "Electronic device & accessory"  2  "Hand labour tool & accessory" 
  # 3 "Kitchen & utensil"     4  "Outdoor activity & sport item"
  #Data$obj_category<-as.numeric(as.factor(Data$obj_category))
  if (RLdata_beh$obj_category[n]=="Electronic device & accessory"){obj<-1
  }else if(RLdata_beh$obj_category[n]== "Hand labour tool & accessory"){obj <-2
  }else if (RLdata_beh$obj_category[n]== "Kitchen & utensil" ){obj<-3
  }else if (RLdata_beh$obj_category[n]== "Outdoor activity & sport item"){obj<-4}
  RLdata_beh$SI[n]<- -(log(RLdata_beh[[paste0("P", obj)]][n]))
  RLdata_beh$SI_V[n]<- -(log(RLdata_beh[[paste0("V", obj)]][n]+1))
  
  
}

recogData_beh$participant<-recogData_beh$SubNum
recogData_beh$recog_acc<-recogData_beh$recogAcc

RLdata_beh$image<-substr(RLdata_beh$image, 17, nchar(RLdata_beh$image))

#attaching memory
RLdata_beh$recog_acc<-NA
RLdata_beh$confidence<-NA

for (n in 1:nrow(RLdata_beh)){
  
  if (length(recogData_beh$recog_acc[recogData_beh$image == RLdata_beh$image[n]
                                     & recogData_beh$participant == RLdata_beh$participant[n]])>0){
    
    RLdata_beh$recog_acc[n]<-recogData_beh$recog_acc[recogData_beh$image == RLdata_beh$image[n]
                                                     & recogData_beh$participant == RLdata_beh$participant[n]]
    
    RLdata_beh$confidence[n]<-recogData_beh$confidence[recogData_beh$image == RLdata_beh$image[n]
                                                       & recogData_beh$participant == RLdata_beh$participant[n]]
    
  } else{
    
    RLdata_beh$recog_acc[n]<-NA
    RLdata_beh$confidence[n]<-NA
  }
  
}

# here I have to count the number of NA
is_NA<-RLdata_beh %>%
  group_by(participant) %>%
  dplyr::summarize(num_NA = sum(!is.na(recog_acc)))

# confidence weighted accuracy
# let's create confidence - weighted accuracy
# create confidence weighted
RLdata_beh$con_weighted_acc =
  ifelse(RLdata_beh$recog_acc == 1, RLdata_beh$confidence + 4,
         ifelse(RLdata_beh$recog_acc == 0,
                ifelse(RLdata_beh$confidence == 1, 4,
                       ifelse(RLdata_beh$confidence == 2, 3,
                              ifelse(RLdata_beh$confidence == 3 , 2, 1))), "NA"))


RLdata_beh$con_weighted_acc <-as.numeric(RLdata_beh$con_weighted_acc )

# create a second variable to check if it coincides
RLdata_beh$con_weighted_acc2<-NA
for (i in 1:nrow(RLdata_beh)) {
  
  if (!is.na(RLdata_beh$recog_acc[i])){
    
    if (RLdata_beh$recog_acc[i] == 1) {
      RLdata_beh$con_weighted_acc2[i] <- RLdata_beh$confidence[i] + 4
      
    } else if (RLdata_beh$recog_acc[i] == 0) {
      
      if (RLdata_beh$confidence[i] == 1) {
        RLdata_beh$con_weighted_acc2[i] <- 4
        
      } else if (RLdata_beh$confidence[i] == 2) {
        RLdata_beh$con_weighted_acc2[i] <- 3
        
      } else if (RLdata_beh$confidence[i] == 3) {
        RLdata_beh$con_weighted_acc2[i] <- 2
        
      } else if (RLdata_beh$confidence[i] == 4) {
        RLdata_beh$con_weighted_acc2[i] <- 1
      }
      
    }
  } else {
    RLdata_beh$con_weighted_acc2[i] <- NA
  }
}

# correlate the two
#cor.test(RLdata_beh$con_weighted_acc, RLdata_beh$con_weighted_acc2)

#plot(RLdata_beh$con_weighted_acc, RLdata_beh$con_weighted_acc2)
# it is the same

# how many NAs per participant?
# is_NA_conf<-RLdata_beh %>%
#   group_by(participant) %>%
#   dplyr::summarize(num_NA = sum(!is.na(con_weighted_acc)))

# 38 participants - participant 41 we have no data


RL_data_b<-RLdata_beh

rm(RLdata_beh)

dat_summary_uncertainty <- summarySEwithin(RL_data_b,
                                           measurevar = c("uncertainty"),
                                           withinvars = c("new_trial_n") ,
                                           idvar = "participant",
                                           na.rm = T)

dat_summary_uncertainty$new_trial_n<-as.numeric(as.character(dat_summary_uncertainty$new_trial_n))