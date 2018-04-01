####load in data 
library(reshape)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
base_dir<-"/Volumes/Apollus/JHy2plus/AdvInference/Causal_Activity/data/outlet/"
setwd(base_dir)

###############################################################
###############################################################
#demo data
demo<-read.csv("/Volumes/Apollus/JHy2plus/AdvInference/Causal_Activity/data/demo_2015.08.26.csv")
demoids<-unique(demo$id)
#demo2 <- demo %>% mutate(ids=factor(id)) %>% select(-id)
demo<-demo %>% rename(ids = id)

demo3<-rename(demo, amputated=amputationDefinition1, curr_employed=interviewCurrentlyWorking,
              fup_bear_weight=followupWeightBearing, fup_Amb_status=followupAmbulatoryStatus)

#demo2<-filter(demo,id %in%actids)
#length(which(demoids %in% actids))


###############################################################
###############################################################
#Activity data
subject_folders<-list.dirs('.',recursive=F,full.names=F)
subj_filenames<-matrix(unlist(strsplit(subject_folders," ")),ncol=2,byrow=T)[,2]
subj_fnames2<-gsub("_","-",subj_filenames)
out_dat<-list()
out_long<-list()
allsubj_long<-data.frame(subj="",datetime=now(),steps=0)
for(i in 1:length(subj_filenames)){
	if(substr(subject_folders[i],1,4) %in% c("1532", "1339", "1563", "1572", "1894","1528","1926")){
		#do nothing for now
	}else{
		setwd(paste0(base_dir,subject_folders[i],"/"))
		out_dat[[i]]<-read.csv(paste0(subj_fnames2[i]," Data.csv"),skip = 29)[-1,]
		out_long[[i]]<-melt(out_dat[[i]],id.vars="Dates.")
		out_long[[i]]$date<-substr(out_long[[i]]$variable, 2,nchar(as.character(out_long[[i]]$variable)))
		out_long[[i]]$dt<-mdy_hms(paste0(out_long[[i]]$date," ",out_long[[i]]$Dates.))
		out_long[[i]]<-out_long[[i]][,-c(1,2,4)]
		names(out_long[[i]])<-c("steps","datetime")
		out_long[[i]]$subj<-substr(subject_folders[i],1,4)
		out_long[[i]]<-out_long[[i]][,c(3,2,1)]
		allsubj_long <-rbind(allsubj_long,out_long[[i]])
	}
}
allsubj_long <-allsubj_long[-1,]
allsubj_long$date<-date(allsubj_long$datetime)
allsubj_long$hour<-hour(allsubj_long$datetime)
allsubj_long <- allsubj_long %>% mutate(ids=as.integer(levels(subj))[subj]) 

#subj_hr<-allsubj_long %>% group_by(subj, date, hour) %>% summarize(sumhr = sum(steps), mnhr=mean(steps))
subj_meanday<-allsubj_long %>% group_by(subj, date, hour) %>% summarize(sumhr = sum(steps),mnhr=mean(steps)) %>% 
  group_by(subj, hour) %>% summarize(mnday=mean(sumhr),mnday_hr=mean(mnhr)) %>% mutate(ids=as.integer(levels(subj))[subj]) 
#subj_meanday2<-subj_hr %>% group_by(subj, date) %>% summarize(mnday = mean(sumhr),mnday_hour=mean(mnhr))

##limit data to subjects with demographic information
avg_hr_act <- subj_meanday %>% filter(ids %in% demoids) #%>%  select(-subj)
act_ids<-unique(avg_hr_act$ids)

##Create smaller testing subset, for computational purposes (to start)
set.seed(1253213)
testids<-act_ids[sample(1:length(act_ids),size=30)]
testdat<-filter(avg_hr_act, ids %in% testids)


##demo subset from test data
demo_test<-filter(demo,ids %in% testids)
demo_test<-rename(demo_test, amputated=amputationDefinition1, curr_employed=interviewCurrentlyWorking,
                  fup_bear_weight=followupWeightBearing, fup_Amb_status=followupAmbulatoryStatus)
demo_test<-rename(demo_test, amputated=amputationDefinition1, curr_employed=interviewCurrentlyWorking,
                  fup_bear_weight=followupWeightBearing, fup_Amb_status=followupAmbulatoryStatus)

##merge datasets
actDemo_dat<-demo %>% left_join(avg_hr_act,by="ids") %>% select(-subj)
mergetest_dat<-demo_test %>% left_join(testdat,by="ids") %>% select(-subj)

summary(mergetest_dat$curr_employed)
#set up for running model
Y_test<-mergetest_dat %>% dcast(ids+curr_employed+gender+age+bmi+amputated~hour,value.var="mnday_hr")
x_test<-model.matrix(~amputated+bmi+age+gender+curr_employed,Y_test[,2:6])

test_fosr<-fosr(Y=Y_test[,7:30], X=x_test,argvals=c(0:23))
#test2_fosr<-fosr(mnday_hr~amputated + ids, data=Y_test)
plot(test_fosr)


###
Y_mod<-actDemo_dat %>% dcast(ids+curr_employed+gender+age+bmi+amputated~hour,value.var="mnday_hr")
x_model<-model.matrix(~amputated+bmi+age+gender+curr_employed,Y_test[,2:6])

test_fosr<-fosr(Y=Y_test[,7:30],X=x_model)

###############################################################
###############################################################
1532, 1339, 1563, 1572
1339, and 1894 (trial 1 and or trial 2)
1528???? what is the deal with this? names are strange
1926 different name than folder name

dat<-matrix(c(4,1,2,3),2)
dat2<-matrix(c(3,2,3,2),2)
fisher.test(dat2,alternative = "greater")

























