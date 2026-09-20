#-----------------------------------------------------------------------------#
# Preprocess behavioral data
# 
# this scripts run some preprocessing steps on behavioral data
#
#-----------------------------------------------------------------------------#

RLdata_beh<-read.csv( "output_files/beh/RL_data_RW_IC_2alpha_beta_beh_mcmc.csv")

# get the accuracy of the previous trials

RLdata_beh$character<-substr(RLdata_beh$cuedCharacter, 9, 10)

#delete switch number 1 (RLdata_beh)

# count trials for setting the before-after change point
count_trials<- RLdata_beh %>%
  group_by(participant, switch_cond) %>%
  tally()

RLdata_beh<-RLdata_beh[RLdata_beh$switch_cond!=1,]

# get a new trial number
# divide in before and after
RLdata_beh<-RLdata_beh  %>%
  group_by(participant)  %>%
  dplyr::mutate(new_trial_n = c(1:n()))

# divide in before and after
RLdata_beh<-RLdata_beh  %>%
  group_by(participant, switch_cond)  %>%
  dplyr::mutate(befAft=c(rep("afterCP", times=24), rep("beforeCP", times=24)))%>%
  dplyr::mutate(trialToSwitch=c(0:23 , rev(-1:-24)))


RLdata_beh<-RLdata_beh[order(RLdata_beh$participant),]


# in RL data, cut all the trial before the 16th and trial cond==1
#RLdata<-RLdata[RLdata$trialNum>16 & RLdata$trial_cond==1,]
RLdata_beh$image<-as.character(RLdata_beh$image)

# now we need the recognition data
recogData_beh<-read.csv("recog_data/recognitionData_beh.csv")

recogData_beh$image<-as.character(recogData_beh$image)



