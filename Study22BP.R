##Read in data##
data_BP_A=read.csv("Study22BP_A.csv")
data_BP_B=read.csv("Study22BP_B.csv")
data_BP_C=read.csv("Study22BP_C.csv")
data_BP_covariates=read.csv("Study22BP_covariates.csv")
str(data_BP_A)
str(data_BP_B)
str(data_BP_C)
library(chron)
library(psych)
library (ggplot2)
library(reshape)

##Convert times##
data_BP_A$Time2=as.POSIXlt(data_BP_A$Time, "%m/%d/%Y %H:%M", tz="")
data_BP_B$Time2=as.POSIXlt(data_BP_B$Time, "%m/%d/%Y %H:%M", tz="")
data_BP_C$Time2=as.POSIXlt(data_BP_C$Time, "%m/%d/%Y %H:%M", tz="")

##converts to character##
data_BP_A$Time3=format(as.POSIXct(data_BP_A$Time2),format="%H:%M")
data_BP_B$Time3=format(as.POSIXct(data_BP_B$Time2),format="%H:%M")
data_BP_C$Time3=format(as.POSIXct(data_BP_C$Time2),format="%H:%M")

##Add active/rest phase and other variables##
data_BP_A$Phase=ifelse(data_BP_A$Time2$hour>=19|data_BP_A$Time2$hour<7, "active", "rest")
data_BP_A$Period.Phase=with(data_BP_A, interaction(Period,Phase))
data_BP_A$Exp_time=as.numeric(difftime(data_BP_A$Time2, "2016-06-03 0:00:00"))
data_A_baseline= data_BP_A[which(data_BP_A$Period=="baseline"),]
data_A_HSD1= data_BP_A[which(data_BP_A$Period=="HSD1"),]
data_A_HSDC= data_BP_A[which(data_BP_A$Period=="HSDC"),]
data_A_HSD2= data_BP_A[which(data_BP_A$Period=="HSD2"),]
data_BP_A$Period_time=ifelse(data_BP_A$Period=="baseline", as.numeric(difftime(data_BP_A$Time2, "2016-06-03 0:00:00")),
 ifelse(data_BP_A$Period=="HSD1", as.numeric(difftime(data_BP_A$Time2, "2016-06-10 0:00:00")), 
 ifelse(data_BP_A$Period=="HSDC", as.numeric(difftime(data_BP_A$Time2, "2016-06-16 0:00:00")),as.numeric(difftime(data_BP_A$Time2, "2016-06-27 0:00:00")))))



data_BP_B$Phase=ifelse(data_BP_B$Time2$hour>=19|data_BP_B$Time2$hour<7, "active", "rest")
data_BP_B$Period.Phase=with(data_BP_B, interaction(Period,Phase))
data_BP_B$Exp_time=as.numeric(difftime(data_BP_B$Time2, "2016-06-10 0:00:00"))
data_B_baseline= data_BP_B[which(data_BP_B$Period=="baseline"),]
data_B_HSD1= data_BP_B[which(data_BP_B$Period=="HSD1"),]
data_B_HSDC= data_BP_B[which(data_BP_B$Period=="HSDC"),]
data_B_HSD2= data_BP_B[which(data_BP_B$Period=="HSD2"),]
data_BP_B$Period_time=ifelse(data_BP_B$Period=="baseline", as.numeric(difftime(data_BP_B$Time2, "2016-06-10 0:00:00")),
 ifelse(data_BP_B$Period=="HSD1", as.numeric(difftime(data_BP_B$Time2, "2016-06-16 0:00:00")), 
 ifelse(data_BP_B$Period=="HSDC", as.numeric(difftime(data_BP_B$Time2, "2016-06-27 0:00:00")),as.numeric(difftime(data_BP_B$Time2, "2016-07-05 0:00:00")))))


data_BP_C$Phase=ifelse(data_BP_C$Time2$hour>=19|data_BP_C$Time2$hour<7, "active", "rest")
data_BP_C$Period.Phase=with(data_BP_C, interaction(Period,Phase))
data_BP_C$Exp_time=as.numeric(difftime(data_BP_C$Time2, "2016-04-15 0:00:00"))
data_C_baseline= data_BP_C[which(data_BP_C$Period=="baseline"),]
data_C_HSD1= data_BP_C[which(data_BP_C$Period=="HSD1"),]
data_C_HSDC= data_BP_C[which(data_BP_C$Period=="HSDC"),]
data_C_HSD2= data_BP_C[which(data_BP_C$Period=="HSD2"),]
data_BP_C$Period_time=ifelse(data_BP_C$Period=="baseline", as.numeric(difftime(data_BP_C$Time2, "2016-04-15 0:00:00")),
 ifelse(data_BP_C$Period=="HSD1", as.numeric(difftime(data_BP_C$Time2, "2016-04-22 0:00:00")), 
 ifelse(data_BP_C$Period=="HSDC", as.numeric(difftime(data_BP_C$Time2, "2016-04-29 0:00:00")),as.numeric(difftime(data_BP_C$Time2, "2016-05-05 0:00:00")))))

time_varsA=names(data_BP_A) %in%c("Date","Time","Time2","Time3")
data_BP_A2=data_BP_A[!time_varsA]
time_varsB=names(data_BP_B) %in%c("Date","Time","Time2","Time3")
data_BP_B2=data_BP_B[!time_varsB]
time_varsC=names(data_BP_C) %in%c("Date","Time","Time2","Time3")
data_BP_C2=data_BP_C[!time_varsC]

data_BP_AB=merge(data_BP_A, data_BP_B, all=TRUE)
data_BP_all=merge(data_BP_AB, data_BP_C, all=TRUE)

data_BP_AB2=merge(data_BP_A2, data_BP_B2, all=TRUE)
data_BP_all2=merge(data_BP_AB2, data_BP_C2, all=TRUE)

data_BP_all$Period_day=ifelse(data_BP_all$Period_time<24, "1",
 ifelse(data_BP_all$Period_time>24 & data_BP_all$Period_time<48, "2", 
 ifelse(data_BP_all$Period_time>=48 & data_BP_all$Period_time<72, "3",
 ifelse(data_BP_all$Period_time>=72 & data_BP_all$Period_time<96, "4",
 ifelse(data_BP_all$Period_time>=96 & data_BP_all$Period_time<120, "5","6")))))

data_BP_all$Phase_day=ifelse(data_BP_all$Period_time<19, "Rest1",
 ifelse(data_BP_all$Period_time>=19 & data_BP_all$Period_time<31, "Active1", 
 ifelse(data_BP_all$Period_time>=31 & data_BP_all$Period_time<43, "Rest2",
 ifelse(data_BP_all$Period_time>=43 & data_BP_all$Period_time<55, "Active2",
 ifelse(data_BP_all$Period_time>=55 & data_BP_all$Period_time<67, "Rest3",
ifelse(data_BP_all$Period_time>=67 & data_BP_all$Period_time<79, "Active3",
ifelse(data_BP_all$Period_time>=79 & data_BP_all$Period_time<91, "Rest4",
ifelse(data_BP_all$Period_time>=91 & data_BP_all$Period_time<103, "Active4","Rest5"))))))))

data_BP_all2$Period_day=ifelse(data_BP_all2$Period_time<24, "1",
 ifelse(data_BP_all2$Period_time>24 & data_BP_all2$Period_time<48, "2", 
 ifelse(data_BP_all2$Period_time>=48 & data_BP_all2$Period_time<72, "3",
 ifelse(data_BP_all2$Period_time>=72 & data_BP_all2$Period_time<96, "4",
 ifelse(data_BP_all2$Period_time>=96 & data_BP_all2$Period_time<120, "5","6")))))

data_BP_all2$Phase_day=ifelse(data_BP_all2$Period_time<19, "Rest1",
 ifelse(data_BP_all2$Period_time>=19 & data_BP_all2$Period_time<31, "Active1", 
 ifelse(data_BP_all2$Period_time>=31 & data_BP_all2$Period_time<43, "Rest2",
 ifelse(data_BP_all2$Period_time>=43 & data_BP_all2$Period_time<55, "Active2",
 ifelse(data_BP_all2$Period_time>=55 & data_BP_all2$Period_time<67, "Rest3",
ifelse(data_BP_all2$Period_time>=67 & data_BP_all2$Period_time<79, "Active3",
ifelse(data_BP_all2$Period_time>=79 & data_BP_all2$Period_time<91, "Rest4",
ifelse(data_BP_all2$Period_time>=91 & data_BP_all2$Period_time<103, "Active4","Rest5"))))))))

id_vars=c("Date","Day", "Time", "Period", "Time2","Time3","Phase", "Period.Phase","Exp_time","Period_time","Period_day", "Phase_day")
systolic=c(id_vars,"A9.Systolic","A3.Systolic","A1.Systolic","A5.Systolic","A4.Systolic","C3.Systolic","C5.Systolic","C10.Systolic")
MAP=c(id_vars,"A9.Pressure","A3.Pressure","A1.Pressure","A5.Pressure","A4.Pressure","C3.Pressure","C5.Pressure","C10.Pressure")
diastolic=c(id_vars,"A9.Diastolic","A3.Diastolic","A1.Diastolic","A5.Diastolic","A4.Diastolic","C3.Diastolic","C5.Diastolic","C10.Diastolic")
HR=c(id_vars,"A9.Heart_Rate","A3.Heart_Rate","A1.Heart_Rate","A5.Heart_Rate","A4.Heart_Rate","C3.Heart_Rate","C5.Heart_Rate","C10.Heart_Rate")
PP=c(id_vars,"A9.Pulse_Pressure","A3.Pulse_Pressure","A1.Pulse_Pressure","A5.Pulse_Pressure","A4.Pulse_Pressure","C3.Pulse_Pressure","C5.Pulse_Pressure","C10.Pulse_Pressure")
activity=c(id_vars,"A9.Activity","A3.Activity","A1.Activity","A5.Activity","A4.Activity","C3.Activity","C5.Activity","C10.Activity")

data_SBP=data_BP_all[systolic]
data_MAP=data_BP_all[MAP]
data_MAPv2=data_BP_all2[c("Period","Period_time","Period_day", "Phase","Phase_day","A9.Pressure","A3.Pressure","A1.Pressure","A5.Pressure","A4.Pressure","C3.Pressure","C5.Pressure","C10.Pressure")]
data_DBP=data_BP_all[diastolic]
data_HR=data_BP_all[HR]
data_PP=data_BP_all[PP]
data_activity=data_BP_all[activity]

colnames(data_SBP)=c(id_vars,"280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")
colnames(data_MAP)=c(id_vars,"280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")
colnames(data_MAPv2)=c("Period","Period_time","Period_day", "Phase","Phase_day","280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")
colnames(data_DBP)=c(id_vars,"280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")
colnames(data_HR)=c(id_vars,"280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")
colnames(data_PP)=c(id_vars,"280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")
colnames(data_activity)=c(id_vars,"280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")

data_SBP2=melt(data_SBP,id=id_vars, variable_name="Mouse_ID")
data_MAP2=melt(data_MAP,id=id_vars, variable_name="Mouse_ID")
data_DBP2=melt(data_DBP,id=id_vars, variable_name="Mouse_ID")
data_HR2=melt(data_HR,id=id_vars, variable_name="Mouse_ID")
data_PP2=melt(data_PP,id=id_vars, variable_name="Mouse_ID")
data_activity2=melt(data_activity,id=id_vars, variable_name="Mouse_ID")

data_MAP_clean= data_MAP2[which(data_MAP2$value <250 & data_MAP2$value >0),]
data_SBP_clean= data_SBP2[which(data_SBP2$value <250 & data_SBP2$value >0),]
data_DBP_clean= data_DBP2[which(data_DBP2$value <250 & data_DBP2$value >0),]
data_PP_clean= data_PP2[which(data_PP2$value <250 & data_PP2$value >0),]

pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))
ggplot(data_MAP_clean, aes(Exp_time,value,color=Mouse_ID))+geom_point()+pub_specs 
ggplot(data_SBP_clean, aes(Exp_time,value,color=Mouse_ID))+geom_point()+pub_specs
ggplot(data_DBP_clean, aes(Exp_time,value,color=Mouse_ID))+geom_point()+pub_specs
ggplot(data_HR2, aes(Exp_time,value,color=Mouse_ID))+geom_point()+pub_specs
ggplot(data_PP_clean, aes(Exp_time,value,color=Mouse_ID))+geom_point()+pub_specs
ggplot(data_activity2, aes(Exp_time,value,color=Mouse_ID))+geom_point()+pub_specs

data_MAP_baseline= data_MAP_clean[which(data_MAP_clean$Period=="baseline"),]
data_MAP_HSD1= data_MAP_clean[which(data_MAP_clean$Period=="HSD1"),]
data_MAP_HSDC= data_MAP_clean[which(data_MAP_clean$Period=="HSDC"),]
data_MAP_HSD2= data_MAP_clean[which(data_MAP_clean$Period=="HSD2"),]

MAP.baseline=data_MAPv2[which(data_MAPv2$Period=="baseline"),]
MAP.baseline2=MAP.baseline[c("Period_time","280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")]
MAP.baseline3=melt(MAP.baseline2,id="Period_time", variable_name="Mouse_ID")
MAP.baseline4= MAP.baseline3[which(MAP.baseline3$value <250 & MAP.baseline3$value >0),]
MAP.baseline5=cast(MAP.baseline4,Mouse_ID~Period_time)
write.table(MAP.baseline5, "MAP.baseline_data.txt", sep="\t") 

MAP.HSD1=data_MAPv2[which(data_MAPv2$Period=="HSD1"),]
MAP.HSD1.2=MAP.HSD1[c("Period_time","280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")]
MAP.HSD1.3=melt(MAP.HSD1.2,id="Period_time", variable_name="Mouse_ID")
MAP.HSD1.4= MAP.HSD1.3[which(MAP.HSD1.3$value <250 & MAP.HSD1.3$value >0),]
MAP.HSD1.5=cast(MAP.HSD1.4,Mouse_ID~Period_time)
write.table(MAP.HSD1.5, "MAP.HSD1_data.txt", sep="\t") 

MAP.HSDC=data_MAPv2[which(data_MAPv2$Period=="HSDC"),]
MAP.HSDC.2=MAP.HSDC[c("Period_time","280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")]
MAP.HSDC.3=melt(MAP.HSDC.2,id="Period_time", variable_name="Mouse_ID")
MAP.HSDC.4= MAP.HSD1.3[which(MAP.HSDC.3$value <250 & MAP.HSDC.3$value >0),]
MAP.HSDC.5=cast(MAP.HSD1.4,Mouse_ID~Period_time)
write.table(MAP.HSDC.5, "MAP.HSDC_data.txt", sep="\t") 

MAP.HSD2=data_MAPv2[which(data_MAPv2$Period=="HSD2"),]

ggplot(data_MAP_baseline, aes(Period_time,value,color=Mouse_ID))+geom_point()+pub_specs+labs(title="Baseline") 
ggplot(data_MAP_HSD1, aes(Period_time,value,color=Mouse_ID))+geom_point()+pub_specs+labs(title="HSD1")
ggplot(data_MAP_HSDC, aes(Period_time,value,color=Mouse_ID))+geom_point()+pub_specs+labs(title="HSD+celecoxib")
ggplot(data_MAP_HSD2, aes(Period_time,value,color=Mouse_ID))+geom_point()+pub_specs+labs(title="HSD2")

MAP.baseline6=MAP.baseline[c("Period_time","Period_day", "Phase","Phase_day","280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")]
MAP.baseline7=melt(MAP.baseline6,id=c("Period_time","Period_day","Phase","Phase_day"), variable_name="Mouse_ID")
MAP.baseline8=MAP.baseline7[which(MAP.baseline7$value <250 & MAP.baseline7$value >0),]

MAP.baseline_mean=cast(MAP.baseline8,Mouse_ID~Phase_day, mean)
MAP.baseline_sd=cast(MAP.baseline8,Mouse_ID~Phase_day, sd)
CV=function(dataset){
	sd(dataset)*100/mean(dataset)
	}
MAP.baseline_CV=cast(MAP.baseline8,Mouse_ID~Phase_day, CV)
colnames(MAP.baseline_mean)=c("Mouse_ID","Baseline_Active1_mean","Baseline_Active2_mean","Baseline_Active3_mean","Baseline_Rest1_mean","Baseline_Rest2_mean","Baseline_Rest3_mean", "Baseline_Rest4_mean")
colnames(MAP.baseline_sd)=c("Mouse_ID","Baseline_Active1_sd","Baseline_Active2_sd","Baseline_Active3_sd","Baseline_Rest1_sd","Baseline_Rest2_sd","Baseline_Rest3_sd", "Baseline_Rest4_sd")
colnames(MAP.baseline_CV)=c("Mouse_ID","Baseline_Active1_CV","Baseline_Active2_CV","Baseline_Active3_CV","Baseline_Rest1_CV","Baseline_Rest2_CV","Baseline_Rest3_CV", "Baseline_Rest4_CV")
MAP.baseline_mean.sd=merge(MAP.baseline_mean,MAP.baseline_sd, by="Mouse_ID")
MAP.baseline_summary=merge(MAP.baseline_mean.sd,MAP.baseline_CV, by="Mouse_ID")

MAP.baseline_mean2=cast(MAP.baseline8,Mouse_ID~Phase, mean)
MAP.baseline_sd2=cast(MAP.baseline8,Mouse_ID~Phase, sd)
MAP.baseline_CV2=cast(MAP.baseline8,Mouse_ID~Phase,CV)
colnames(MAP.baseline_mean2)=c("Mouse_ID","Baseline_Active_mean","Baseline_Rest_mean")
colnames(MAP.baseline_sd2)=c("Mouse_ID","Baseline_Active_sd","Baseline_Rest_sd")
colnames(MAP.baseline_CV2)=c("Mouse_ID","Baseline_Active_CV","Baseline_Rest_CV")
MAP.baseline_mean.sd2=merge(MAP.baseline_mean2,MAP.baseline_sd2, by="Mouse_ID")
MAP.baseline_summary2=merge(MAP.baseline_mean.sd2,MAP.baseline_CV2, by="Mouse_ID")

MAP.HSD1.6=MAP.HSD1[c("Period_time","Period_day", "Phase","Phase_day","280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")]
MAP.HSD1.7=melt(MAP.HSD1.6,id=c("Period_time","Period_day","Phase","Phase_day"), variable_name="Mouse_ID")
MAP.HSD1.8=MAP.HSD1.7[which(MAP.HSD1.7$value <250 & MAP.HSD1.7$value >0),]

MAP.HSD1_mean=cast(MAP.HSD1.8,Mouse_ID~Phase_day, mean)
MAP.HSD1_sd=cast(MAP.HSD1.8,Mouse_ID~Phase_day, sd)
MAP.HSD1_CV=cast(MAP.HSD1.8,Mouse_ID~Phase_day, CV)
MAP.HSD1_mean2=cast(MAP.HSD1.8,Mouse_ID~Phase_day, mean)
MAP.HSD1_sd2=cast(MAP.HSD1.8,Mouse_ID~Phase_day, sd)
MAP.HSD1_C2V=cast(MAP.HSD1.8,Mouse_ID~Phase_day, CV)
colnames(MAP.HSD1_mean)=c("Mouse_ID","HSD1_Active1_mean","HSD1_Active2_mean","HSD1_Active3_mean","HSD1_Active4_mean","HSD1_Rest1_mean","HSD1_Rest2_mean","HSD1_Rest3_mean", "HSD1_Rest4_mean", "HSD1_Rest5_mean")
colnames(MAP.HSD1_sd)=c("Mouse_ID","HSD1_Active1_sd","HSD1_Active2_sd","HSD1_Active3_sd","HSD1_Active4_sd","HSD1_Rest1_sd","HSD1_Rest2_sd","HSD1_Rest3_sd", "HSD1_Rest4_sd", "HSD1_Rest5_sd")
colnames(MAP.HSD1_CV)=c("Mouse_ID","HSD1_Active1_CV","HSD1_Active2_CV","HSD1_Active3_CV","HSD1_Active4_CV","HSD1_Rest1_CV","HSD1_Rest2_CV","HSD1_Rest3_CV", "HSD1_Rest4_CV", "HSD1_Rest5_CV")
MAP.HSD1_mean.sd=merge(MAP.HSD1_mean,MAP.HSD1_sd, by="Mouse_ID")
MAP.HSD1_summary=merge(MAP.HSD1_mean.sd,MAP.HSD1_CV, by="Mouse_ID")

MAP.HSD1_mean2=cast(MAP.HSD1.8,Mouse_ID~Phase, mean)
MAP.HSD1_sd2=cast(MAP.HSD1.8,Mouse_ID~Phase, sd)
MAP.HSD1_CV2=cast(MAP.HSD1.8,Mouse_ID~Phase,CV)
colnames(MAP.HSD1_mean2)=c("Mouse_ID","HSD1_Active_mean","HSD1_Rest_mean")
colnames(MAP.HSD1_sd2)=c("Mouse_ID","HSD1_Active_sd","HSD1_Rest_sd")
colnames(MAP.HSD1_CV2)=c("Mouse_ID","HSD1_Active_CV","HSD1_Rest_CV")
MAP.HSD1_mean.sd2=merge(MAP.HSD1_mean2,MAP.HSD1_sd2, by="Mouse_ID")
MAP.HSD1_summary2=merge(MAP.HSD1_mean.sd2,MAP.HSD1_CV2, by="Mouse_ID")

MAP.HSDC.6=MAP.HSDC[c("Period_time","Period_day","Phase", "Phase_day","280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")]
MAP.HSDC.7=melt(MAP.HSDC.6,id=c("Period_time","Period_day","Phase","Phase_day"), variable_name="Mouse_ID")
MAP.HSDC.8=MAP.HSDC.7[which(MAP.HSDC.7$value <250 & MAP.HSDC.7$value >0),]

MAP.HSDC_mean=cast(MAP.HSDC.8,Mouse_ID~Phase_day, mean)
MAP.HSDC_sd=cast(MAP.HSDC.8,Mouse_ID~Phase_day, sd)
MAP.HSDC_CV=cast(MAP.HSDC.8,Mouse_ID~Phase_day, CV)
colnames(MAP.HSDC_mean)=c("Mouse_ID","HSDC_Active1_mean","HSDC_Active2_mean","HSDC_Active3_mean","HSDC_Active4_mean","HSDC_Rest1_mean","HSDC_Rest2_mean","HSDC_Rest3_mean", "HSDC_Rest4_mean", "HSDC_Rest5_mean")
colnames(MAP.HSDC_sd)=c("Mouse_ID","HSDC_Active1_sd","HSDC_Active2_sd","HSDC_Active3_sd","HSDC_Active4_sd","Rest1_sd","HSDC_Rest2_sd","HSDC_Rest3_sd", "HSDC_Rest4_sd", "HSDC_Rest5_sd")
colnames(MAP.HSDC_CV)=c("Mouse_ID","HSDC_Active1_CV","HSDC_Active2_CV","HSDC_Active3_CV","HSDC_Active4_CV","Rest1_CV","HSDC_Rest2_CV","HSDC_Rest3_CV", "HSDC_Rest4_CV", "HSDC_Rest5_CV")
MAP.HSDC_mean.sd=merge(MAP.HSDC_mean,MAP.HSDC_sd, by="Mouse_ID")
MAP.HSDC_summary=merge(MAP.HSDC_mean.sd,MAP.HSDC_CV, by="Mouse_ID")

MAP.HSDC_mean2=cast(MAP.HSDC.8,Mouse_ID~Phase, mean)
MAP.HSDC_sd2=cast(MAP.HSDC.8,Mouse_ID~Phase, sd)
MAP.HSDC_CV2=cast(MAP.HSDC.8,Mouse_ID~Phase,CV)
colnames(MAP.HSDC_mean2)=c("Mouse_ID","HSDC_Active_mean","HSDC_Rest_mean")
colnames(MAP.HSDC_sd2)=c("Mouse_ID","HSDC_Active_sd","HSDC_Rest_sd")
colnames(MAP.HSDC_CV2)=c("Mouse_ID","HSDC_Active_CV","HSDC_Rest_CV")
MAP.HSDC_mean.sd2=merge(MAP.HSDC_mean2,MAP.HSDC_sd2, by="Mouse_ID")
MAP.HSDC_summary2=merge(MAP.HSDC_mean.sd2,MAP.HSDC_CV2, by="Mouse_ID")

MAP.HSD2.6=MAP.HSD2[c("Period_time","Period_day","Phase", "Phase_day","280-3","280-2","275-2","278-4","278-2","276-1","277-3","280-4")]
MAP.HSD2.7=melt(MAP.HSD2.6,id=c("Period_time","Period_day","Phase","Phase_day"), variable_name="Mouse_ID")
MAP.HSD2.8=MAP.HSD2.7[which(MAP.HSD2.7$value <250 & MAP.HSD2.7$value >0),]

MAP.HSD2_mean=cast(MAP.HSD2.8,Mouse_ID~Phase_day, mean)
MAP.HSD2_sd=cast(MAP.HSD2.8,Mouse_ID~Phase_day, sd)
MAP.HSD2_CV=cast(MAP.HSD2.8,Mouse_ID~Phase_day, CV)
colnames(MAP.HSD2_mean)=c("Mouse_ID","HSD2_Active1_mean","HSD2_Active2_mean","HSD2_Active3_mean","HSD2_Active4_mean","HSD2_Rest1_mean","HSD2_Rest2_mean","HSD2_Rest3_mean", "HSD2_Rest4_mean", "HSD2_Rest5_mean")
colnames(MAP.HSD2_sd)=c("Mouse_ID","HSD2_Active1_sd","HSD2_Active2_sd","HSD2_Active3_sd","HSD2_Active4_sd","HSD2_Rest1_sd","HSD2_Rest2_sd","HSD2_Rest3_sd", "HSD2_Rest4_sd", "HSD2_Rest5_sd")
colnames(MAP.HSD2_CV)=c("Mouse_ID","HSD2_Active1_CV","HSD2_Active2_CV","HSD2_Active3_CV","HSD2_Active4_CV","HSD2_Rest1_CV","HSD2_Rest2_CV","HSD2_Rest3_CV", "HSD2_Rest4_CV", "HSD2_Rest5_CV")
MAP.HSD2_mean.sd=merge(MAP.HSD2_mean,MAP.HSD2_sd, by="Mouse_ID")
MAP.HSD2_summary=merge(MAP.HSD2_mean.sd,MAP.HSD2_CV, by="Mouse_ID")

MAP.HSD2_mean2=cast(MAP.HSD2.8,Mouse_ID~Phase, mean)
MAP.HSD2_sd2=cast(MAP.HSD2.8,Mouse_ID~Phase, sd)
MAP.HSD2_CV2=cast(MAP.HSD2.8,Mouse_ID~Phase,CV)
colnames(MAP.HSD2_mean2)=c("Mouse_ID","HSD2_Active_mean","HSD2_Rest_mean")
colnames(MAP.HSD2_sd2)=c("Mouse_ID","HSD2_Active_sd","HSD2_Rest_sd")
colnames(MAP.HSD2_CV2)=c("Mouse_ID","HSD2_Active_CV","HSD2_Rest_CV")
MAP.HSD2_mean.sd2=merge(MAP.HSD2_mean2,MAP.HSD2_sd2, by="Mouse_ID")
MAP.HSD2_summary2=merge(MAP.HSD2_mean.sd2,MAP.HSD2_CV2, by="Mouse_ID")

MAP.baseline.HSD1=merge(MAP.baseline_summary2,MAP.HSD1_summary2, by="Mouse_ID")
MAP.HSDC.HSD2=merge(MAP.HSDC_summary2,MAP.HSD2_summary2, by="Mouse_ID")
MAP.summary=merge(MAP.baseline.HSD1,MAP.HSDC.HSD2, by="Mouse_ID")
MAP.summary2=stack(MAP.summary)
MAP.summary2$Mouse_ID = rep(MAP.summary$Mouse_ID, 24) 

MAP.baseline.HSD1_mean=merge(MAP.baseline_mean2,MAP.HSD1_mean2, by="Mouse_ID")
MAP.HSDC.HSD2_mean=merge(MAP.HSDC_mean2,MAP.HSD2_mean2, by="Mouse_ID")
MAP.mean=merge(MAP.baseline.HSD1_mean,MAP.HSDC.HSD2_mean, by="Mouse_ID")

MAP.mean2=stack(MAP.mean)
MAP.mean2$Mouse_ID = rep(MAP.summary$Mouse_ID, 8) 

MAP.mean.rest=MAP.mean[c("Mouse_ID","Baseline_Rest_mean","HSD1_Rest_mean","HSDC_Rest_mean","HSD2_Rest_mean")]
MAP.mean.active=MAP.mean[c("Mouse_ID","Baseline_Active_mean","HSD1_Active_mean","HSDC_Active_mean","HSD2_Active_mean")]

MAP.mean.active2=MAP.mean.active
MAP.mean.rest2=MAP.mean.rest
MAP.mean.active2$HSD1_percentcontrol=MAP.mean.active2$HSD1_Active_mean*100/MAP.mean.active2$Baseline_Active_mean
MAP.mean.active2$HSDC_percentcontrol=MAP.mean.active2$HSDC_Active_mean*100/MAP.mean.active2$Baseline_Active_mean
MAP.mean.active2$HSD2_percentcontrol=MAP.mean.active2$HSD2_Active_mean*100/MAP.mean.active2$Baseline_Active_mean

MAP.mean.rest2$HSD1_percentcontrol=MAP.mean.rest2$HSD1_Rest_mean*100/MAP.mean.rest2$Baseline_Rest_mean
MAP.mean.rest2$HSDC_percentcontrol=MAP.mean.rest2$HSDC_Rest_mean*100/MAP.mean.rest2$Baseline_Rest_mean
MAP.mean.rest2$HSD2_percentcontrol=MAP.mean.rest2$HSD2_Rest_mean*100/MAP.mean.rest2$Baseline_Rest_mean

MAP.mean.rest2=stack(MAP.mean.rest)
MAP.mean.rest2$Mouse_ID = rep(MAP.mean.rest$Mouse_ID, 4) 
MAP.mean.active2=stack(MAP.mean.active)
MAP.mean.active2$Mouse_ID = rep(MAP.mean.active$Mouse_ID, 4) 

MAP.active.aov = aov(values ~ ind + Mouse_ID, data=MAP.mean.active2)
TukeyHSD(MAP.active.aov, which="ind")
MAP.rest.aov = aov(values ~ ind + Mouse_ID, data=MAP.mean.rest2)
TukeyHSD(MAP.rest.aov, which="ind")

MAP.baseline.HSD1_CV=merge(MAP.baseline_CV2,MAP.HSD1_CV2, by="Mouse_ID")
MAP.HSDC.HSD2_CV=merge(MAP.HSDC_CV2,MAP.HSD2_CV2, by="Mouse_ID")
MAP.CV=merge(MAP.baseline.HSD1_CV,MAP.HSDC.HSD2_CV, by="Mouse_ID")

MAP.CV2=stack(MAP.CV)
MAP.CV2$Mouse_ID = rep(MAP.CV$Mouse_ID, 8) 

MAP.CV.rest=MAP.CV[c("Mouse_ID","Baseline_Rest_CV","HSD1_Rest_CV","HSDC_Rest_CV","HSD2_Rest_CV")]
MAP.CV.active=MAP.CV[c("Mouse_ID","Baseline_Active_CV","HSD1_Active_CV","HSDC_Active_CV","HSD2_Active_CV")]

MAP.CV.rest2=stack(MAP.CV.rest)
MAP.CV.rest2$Mouse_ID = rep(MAP.CV.rest$Mouse_ID, 4) 
MAP.CV.active2=stack(MAP.CV.active)
MAP.CV.active2$Mouse_ID = rep(MAP.CV.active$Mouse_ID, 4) 

MAP.activeCV.aov = aov(values ~ ind + Mouse_ID, data=MAP.CV.active2)
TukeyHSD(MAP.activeCV.aov, which="ind")
MAP.restCV.aov = aov(values ~ ind + Mouse_ID, data=MAP.CV.rest2)
TukeyHSD(MAP.restCV.aov, which="ind")

MAP.mean.HSDC=MAP.mean[c("Mouse_ID","HSDC_Rest_mean","HSDC_Active_mean")]
MAP.mean.HSD=MAP.mean[c("Mouse_ID","HSD1_Rest_mean","HSD1_Active_mean")]
MAP.mean.cele=merge(data_BP_covariates,MAP.mean.HSDC, by.x="Mouse_ID", all=TRUE)
MAP.mean.HSD1=merge(data_BP_covariates,MAP.mean.HSD, by.x="Mouse_ID", all=TRUE)
MAP.mean.all=merge(data_BP_covariates,MAP.mean, by.x="Mouse_ID", all=TRUE)
MAP.mean.all$HSD1_Active_percent=MAP.mean.all$HSD1_Active_mean*100/MAP.mean.all$Baseline_Active_mean
MAP.mean.all$HSDC_Active_percent=MAP.mean.all$HSDC_Active_mean*100/MAP.mean.all$Baseline_Active_mean
MAP.mean.all$HSD2_Active_percent=MAP.mean.all$HSD2_Active_mean*100/MAP.mean.all$Baseline_Active_mean

MAP.mean.all$HSD1_Rest_percent=MAP.mean.all$HSD1_Rest_mean*100/MAP.mean.all$Baseline_Rest_mean
MAP.mean.all$HSDC_Rest_percent=MAP.mean.all$HSDC_Rest_mean*100/MAP.mean.all$Baseline_Rest_mean
MAP.mean.all$HSD2_Rest_percent=MAP.mean.all$HSD2_Rest_mean*100/MAP.mean.all$Baseline_Rest_mean

MAP.mean.all$PGIM_HSD1_percent=MAP.mean.all$PGIM_HSD1*100/MAP.mean.all$PGIM_basal
MAP.mean.all$PGIM_HSDC_percent=MAP.mean.all$PGIM_HSDC*100/MAP.mean.all$PGIM_basal
MAP.mean.all$PGIM_HSD2_percent=MAP.mean.all$PGIM_HSD2*100/MAP.mean.all$PGIM_basal

MAP.mean.all$PGEM_HSD1_percent=MAP.mean.all$PGEM_HSD1*100/MAP.mean.all$PGEM_basal
MAP.mean.all$PGEM_HSDC_percent=MAP.mean.all$PGEM_HSDC*100/MAP.mean.all$PGEM_basal
MAP.mean.all$PGEM_HSD2_percent=MAP.mean.all$PGEM_HSD2*100/MAP.mean.all$PGEM_basal

MAP.mean.all$KO_Group=ifelse(MAP.mean.all$Cre=="-", "control", as.character(MAP.mean.all$TM_dose))
MAP.mean.all$Medulla_tertile= with(MAP.mean.all, cut (Renal_medulla_Ptgs2, breaks=quantile(Renal_medulla_Ptgs2, probs=seq(0,1,by=1/3), na.rm=TRUE),include.lowest=TRUE))
MAP.mean.all$Cortex_tertile= with(MAP.mean.all, cut (Renal_cortex_Ptgs2, breaks=quantile(Renal_cortex_Ptgs2, probs=seq(0,1,by=1/3), na.rm=TRUE),include.lowest=TRUE))
MAP.mean.all$Cortex_COX1_tertile= with(MAP.mean.all, cut (Renal_cortex_Ptgs1, breaks=quantile(Renal_cortex_Ptgs1, probs=seq(0,1,by=1/3), na.rm=TRUE),include.lowest=TRUE))

cor(MAP.mean.cele$Celecoxib_trough, MAP.mean.cele$HSDC_Active_mean, method = "pearson")
ggplot(MAP.mean.all, aes(Celecoxib_trough,PGIM_HSDC,color=Cortex_COX1_tertile))+geom_point()+pub_specs 
ggplot(MAP.mean.all, aes(PGIM_HSD2,HSD2_Active_mean,color=Mouse_ID))+geom_point()+pub_specs 

