#------------------------------------------------------------------------------#
# generate quantities and memory for fmri sample---#
#------------------------------------------------------------------------------#


categ<-levels(as.factor(RLdata_fmri$obj_category))


RLdata_fmri$prob_obs<-NA
RLdata_fmri$prob_obs_post<-NA
RLdata_fmri$expV<-NA
RLdata_fmri$expVpre<-NA
RLdata_fmri$Delta<-NA
RLdata_fmri$update<-NA
RLdata_fmri$updateV<-NA
RLdata_fmri$expVpost<-NA
RLdata_fmri$updateAll<-NA#
RLdata_fmri$uncertainty<-NA
RLdata_fmri$SI_V<-NA

for (n in 1:nrow(RLdata_fmri)){
  if (!is.na(RLdata_fmri$obj_category[n])){
    RLdata_fmri$updateAll[n]<-sum(abs(RLdata_fmri[n,c("V1", "V2", "V3", "V4")] -
                                        RLdata_fmri[n,c("Vpre1", "Vpre2", "Vpre3", "Vpre4")] 
    ))
    
    RLdata_fmri$prob_obs[n]<-RLdata_fmri[[paste0("P", which(categ == RLdata_fmri$obj_category[n]))]][n] 
    RLdata_fmri$expV[n]<-RLdata_fmri[[paste0("V", which(categ == RLdata_fmri$obj_category[n]))]][n] 
    RLdata_fmri$expVpre[n]<-RLdata_fmri[[paste0("Vpre", which(categ == RLdata_fmri$obj_category[n]))]][n] 
    RLdata_fmri$Delta[n]<-1-RLdata_fmri[[paste0("Vpre", which(categ == RLdata_fmri$obj_category[n]))]][n] 
    # prediction error chosen
    if(!is.na(RLdata_fmri$respCat[n])){
      RLdata_fmri$Delta_chosen[n]<-RLdata_fmri$accuracy[n] -RLdata_fmri[[paste0("Vpre", which(categ == RLdata_fmri$respCat[n]))]][n] 
    }
    RLdata_fmri$prob_obs_post[n]<-RLdata_fmri[[paste0("Post", which(categ == RLdata_fmri$obj_category[n]))]][n] 
    RLdata_fmri$updateV[n]<-abs(RLdata_fmri$expV[n]-RLdata_fmri$expVpre[n])
    RLdata_fmri$update[n]<-abs(RLdata_fmri$prob_obs_post[n]-RLdata_fmri$prob_obs[n])
    
    #RLdata_fmri$updateAll[n]<-sum(RLdata_fmri[n,c('V1', 'V2', 'V3', 'V4')] - 
    #   RLdata_fmri[n,c('Vpre1', 'Vpre2', 'Vpre3', 'Vpre4')])
    RLdata_fmri$uncertainty[n]<-1/(var(as.numeric(RLdata_fmri[n,c('Vpre1', 'Vpre2', 'Vpre3', 'Vpre4')]))+0.1)
    
    
    
  }
}


# shannon information (surprise)
RLdata_fmri$SI<-NA
RLdata_fmri$SI_V<-NA

for (n in 1:nrow(RLdata_fmri)){
  # we are taking the prior of the category
  # that is shown
  #1  "Electronic device & accessory"  2  "Hand labour tool & accessory" 
  # 3 "Kitchen & utensil"     4  "Outdoor activity & sport item"
  #Data$obj_category<-as.numeric(as.factor(Data$obj_category))
  if (RLdata_fmri$obj_category[n]=="Electronic device & accessory"){obj<-1
  }else if(RLdata_fmri$obj_category[n]== "Hand labour tool & accessory"){obj <-2
  }else if (RLdata_fmri$obj_category[n]== "Kitchen & utensil" ){obj<-3
  }else if (RLdata_fmri$obj_category[n]== "Outdoor activity & sport item"){obj<-4}
  RLdata_fmri$SI[n]<- -(log(RLdata_fmri[[paste0("P", obj)]][n]))
  RLdata_fmri$SI_V[n]<- -(log(RLdata_fmri[[paste0("V", obj)]][n]+1))
  
  
}


# attach memory

# ggplot(RLdata_fmri[RLdata_fmri$participant==29,], aes(x=trialN, y=uncertainty,  group = 1))+
# stat_summary(fun = mean,
#              geom = "line", color = "red")+
# geom_vline(xintercept = c(97, 143,193, 243))+
# 
# theme_bw()

################# attachiong memory
recogData_fmri$con_weighted_acc <-
  ifelse(recogData_fmri$recog_acc == 1, recogData_fmri$confidence + 4,
         ifelse(recogData_fmri$recog_acc == 0,
                ifelse(recogData_fmri$confidence == 1, 4,
                       ifelse(recogData_fmri$confidence == 2, 3,
                              ifelse(recogData_fmri$confidence == 3 , 2, 1))), "NA"))



recogData_fmri$con_weighted_acc <-as.numeric(recogData_fmri$con_weighted_acc )

#attaching memory
RLdata_fmri$con_weighted_acc<-NA
RLdata_fmri$recog_acc<-NA


for (n in 1:nrow(RLdata_fmri)){
  
  if (length(recogData_fmri$recog_acc[recogData_fmri$image == RLdata_fmri$image[n]
                                      & recogData_fmri$participant == RLdata_fmri$participant[n]])>0){
    
    
    RLdata_fmri$recog_acc[n]<-recogData_fmri$recog_acc[recogData_fmri$image == RLdata_fmri$image[n]
                                                       & recogData_fmri$participant == RLdata_fmri$participant[n]]
    
    RLdata_fmri$con_weighted_acc[n]<-recogData_fmri$con_weighted_acc[recogData_fmri$image == RLdata_fmri$image[n]
                                                                     & recogData_fmri$participant == RLdata_fmri$participant[n]]
    
  } else{
    
    RLdata_fmri$recog_acc[n]<-NA
    RLdata_fmri$con_weighted_acc[n]<-NA
  }
  
}


is_NA_conf_fmri<-RLdata_fmri %>%
  group_by(participant) %>%
  dplyr::summarize(num_NA = sum(!is.na(con_weighted_acc)))

# delete recog data beh
rm(recogData_fmri)

RL_data_f<-RLdata_fmri

#RL_data_f$con_weighted_acc<-  (RL_data_f$con_weighted_acc - 1) / (4 - 1)


rm(RLdata_fmri)
# exclude the 9 and the 19
RL_data_f<-RL_data_f[!RL_data_f$participant %in% c(9, 19),]