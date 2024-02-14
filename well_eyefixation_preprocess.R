#### This eye movement data pre-processing script includes following steps:
# Step1: Load files and attach the task info file to the eye gaze data
# Step2: Define eye gaze location (Top Left,Top Right,Low Left,Low Right)
# Step3: Identify looking objects (Target Animal,Target Instrument,Distractor Animal,Distractor Instrument)
# Step4: Define time windows (1=Verb, 2=N1, 3=N2)
# Step5: Clean data and check data loss (face convergence value below 0.5 & at least 2/3 frames left in each trial)
# Step6: Save eye gaze data for verb bias task and semantic verb task (for stats/plotting purpose)
# Step7: Identify first fixation in each trials for each participant and save the result
################################################################################################################
library(dplyr)
library(tidyverse)
library(ggplot2)
# Step1: Load files and attach the task info file to the eye gaze file
data <- read.csv("well_eyefixation_rawdata.csv")
task_info <- read.csv("task_info.csv")
task_info$verb_onset_new <- 200
task_info$n1_onset_new <- task_info$n1_onset+200
task_info$n2_onset_new <- task_info$n2_onset+200
data <- left_join(data,task_info)


# Step2: Define eye gaze location (Top Left,Top Right,Low Left,Low Right)
#### [0.5,0.5] is the center of screen
lengt <- length(data$trial)
for (i in 1:lengt) {
         if (data$x_pred_normalised[i] < 0.5 && data$y_pred_normalised[i] > 0.5) {
    data$eyeloc[i] = "TL" #top left
  } else if (data$x_pred_normalised[i] > 0.5 && data$y_pred_normalised[i] > 0.5) {
    data$eyeloc[i] = "TR" #top right
  } else if (data$x_pred_normalised[i] < 0.5 && data$y_pred_normalised[i] < 0.5) {
    data$eyeloc[i] = "LL" #low left
  } else if (data$x_pred_normalised[i] > 0.5 && data$y_pred_normalised[i] < 0.5) {
    data$eyeloc[i] = "LR" #low right
  } else {
  }
}


# Step3: Define looking objects (Target Animal,Target Instrument,Distractor Animal,Distractor Instrument)
data <- data %>%
  mutate(look_object = ifelse(TA == eyeloc, "TA", ifelse(
    TI == eyeloc, "TI", ifelse(DA == eyeloc, "DA", "DI")))) %>%
  as.data.frame()


# Step4: Define time windows (1=Verb, 2=N1, 3=N2)
data_length <- length(data$trial)
for (i in 1:data_length) {
  if (data$time_elapsed[i] > data$verb_onset_new[i] && data$time_elapsed[i] < data$n1_onset[i]) {
    data$timewindow[i] = 1 #200 msec offset
  } else if (data$time_elapsed[i] > data$n1_onset_new[i] && data$time_elapsed[i] < data$n2_onset[i]) {
    data$timewindow[i] = 2 #200 msec offset
  } else if (data$time_elapsed[i] > data$n2_onset_new[i] && data$time_elapsed[i] < data$n2_window_end[i]) {
    data$timewindow[i] = 3 #200 msec offset
  } else {data$timewindow[i] = "N/A"
  }
}

#write.csv(data, file = "well_fixation_raw_temp.csv",row.names=T)
data <- read.csv("well_fixation_raw_temp.csv")
data <- data[-(1)]
# Step5: Clean data and check data loss (face convergence below 0.5 & at least 2/3 frames left in each trial)
### Drop frames with low face convergence value within each trials
data$frame_drop <- "drop"
data[data$face_conf > 0.5,]$frame_drop <- "keep" #mark frames that need to be dropped

subdata <- subset(data,task=="vb")
subdata <- subset(subdata, cond!="modtrain" & cond!="insttrain") # a total of 263,514 frames in VB test trials
drop_count <- subset(subdata,frame_drop=="drop") # 28,102 frames were drop (10.6%)

data$frame_drop <- as.factor(data$frame_drop)
dataloss <- data %>% group_by(subject,trial) %>% count(frame_drop,.drop = FALSE)
dataloss <- dataloss %>% group_by(subject,trial) %>% dplyr::mutate(total=sum(n))
dataloss <- dataloss %>% group_by(subject,trial) %>% dplyr::mutate(drop_percentage=paste0(round(n/sum(n),2)))

dataloss_drop <- subset(dataloss,frame_drop=="drop")
dataloss_drop <- dataloss_drop[-(3:5)]

### If data loss < 0.33, keep the trial. If not, drop that trial. 
dataloss_drop$trial_drop <- "drop"
dataloss_drop[dataloss_drop$drop_percentage < 0.33,]$trial_drop <- "keep"
cond_list <- data[c(1:2,20)]
cond_list <- distinct(cond_list,.keep_all= TRUE)

data <- left_join(data,dataloss_drop)
data_clean <- subset(data,frame_drop!="drop") ## drop frames
data_clean <- subset(data_clean,trial_drop!="drop") ## drop trials

### Then check how many trials left in each participant and check which participant need to be dropped
dataloss_drop <- left_join(dataloss_drop,cond_list)
## check the trial loss in **Verb bias test trials** in each participant
test_trial <- subset(dataloss_drop, cond=="pre_test_mod"|cond=="pre_test_inst"|cond=="modtest_1"|cond=="modtest_2"|cond=="modtest_3"|cond=="insttest_1"|cond=="insttest_2"|cond=="insttest_3")
dataloss_drop_subjectlist <- subset(test_trial,trial_drop=="drop")
dataloss_drop_subjectlist <- dataloss_drop_subjectlist %>% group_by(subject) %>% count(trial_drop,.drop = FALSE) #Drop 234 trials
dataloss_keep_subjectlist <- subset(test_trial,trial_drop=="keep")
dataloss_keep_subjectlist <- dataloss_keep_subjectlist %>% group_by(subject) %>% count(trial_drop,.drop = FALSE) #Drop 234 trials

## check the trial loss in **ALL trials** in each participant
#dataloss_drop_subjectlist <- subset(dataloss_drop,trial_drop=="drop")
#dataloss_drop_subjectlist <- dataloss_drop_subjectlist %>% group_by(subject) %>% count(trial_drop,.drop = FALSE) 


# Step6: Save eye gaze data based on different tasks (for stats/plots)
## Verb bias task
vb <- subset(data_clean,task=="vb")
write.csv(vb,"well_vb_eyefixation_raw.csv")
## Semantic verb task
semantic <- subset(data_clean,task=="semantic")
write.csv(semantic,"well_semantic_eyefixation_raw.csv")


# Step7: Identify first fixation in each trial/subject/task
### 1.Verb bias task
#vb <-read.csv("well_vb_eyefixation_raw.csv")
vb <- vb[-c(35:38)]
vb$firstlook <- NA
data_length <- length(vb$trial)
for (i in 1:data_length) { #Examine looking object b/w row N and row N+1 (0=same object,1=different object)
  if (vb$look_object[i] == vb$look_object[i+1]) {
    vb$firstlook[i+1] = 0
  } else {vb$firstlook[i+1] = 1}
}

subdata <- subset(vb, firstlook!=0)
subdata$fixation_duration <- NA
subdata_length <- length(subdata$trial)
subdata$fixation_duration[1] <- 0

for (i in 1:subdata_length) {
  subdata$fixation_duration[i] = subdata$time_elapsed[i+1] - subdata$time_elapsed[i]
}

subdata1 <- subset(subdata, time_elapsed > 200) #200ms offset from the beginning of sentence
subdata2 <- subset(subdata1, fixation_duration >= 200) #fixation duration must > 200ms
subdata3 <- distinct(subdata2,trial,subject, .keep_all= TRUE) #keep the first fixation and drop other fixations in the trial
write.csv(subdata3, file = "well_vb_firstfixation_raw.csv",row.names=T)


### 2.Semantic verb task
#semantic <-read.csv("well_semantic_eyefixation_raw.csv")
semantic <- semantic[-c(35:38)]
semantic$firstlook <- NA
data_length <- length(semantic$trial)

for (i in 1:data_length) {
  if (semantic$look_object[i] == semantic$look_object[i+1]) {
    semantic$firstlook[i+1] = 0
  } else {semantic$firstlook[i+1] = 1}
}

subdata <- subset(semantic, firstlook!=0)
subdata$fixation_duration <- NA
subdata_length <- length(subdata$trial)
subdata$fixation_duration[1] <- 0

for (i in 1:subdata_length) {
  subdata$fixation_duration[i] = subdata$time_elapsed[i+1] - subdata$time_elapsed[i]
}

subdata1 <- subset(subdata, time_elapsed > 200)
subdata2 <- subset(subdata1, fixation_duration >= 200)
subdata3 <- distinct(subdata2,trial,subject, .keep_all= TRUE)
write.csv(subdata3, file = "well_semantic_firstfixation_raw.csv",row.names=T)