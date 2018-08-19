##Read in data##
data=read.csv("COX_assay_results8162017.csv")
data_ABPM=read.csv("ABPM_data_8232017.csv")
data_pk=read.csv("Plasma_levels_02202018.csv")

data_ABPM$Time2=strptime(data_ABPM$Time,"%H:%M:%S")
data_ABPM$Base_Time=strptime("08:00:00", "%H:%M:%S")
data_ABPM$Hours=as.numeric(difftime(data_ABPM$Time2,data_ABPM$Base_Time, units ="hours")+data_ABPM$Day*24)


library(reshape)
data2=melt(data,id.vars=c("SubjectID","Assay", "Treatment"))

##Add time in hours##
data2$time=ifelse(data2$variable=="Day1",0,ifelse(data2$variable=="Day7_T0",168,ifelse(data2$variable=="Day7_T0.5",168.5,ifelse(data2$variable=="Day7_T1",169,ifelse(data2$variable=="Day7_T2",170,ifelse(data2$variable=="Day7_T4",172,ifelse(data2$variable=="Day7_T8",176,ifelse(data2$variable=="Day7_T12",180,ifelse(data2$variable=="Day8",192,ifelse(data2$variable=="Day9",216,240))))))))))
data2$time2=data2$time-168

data_pk$time=ifelse(data_pk$Day.Time=="Day1",0,ifelse(data_pk$Day.Time=="Day7_T0",168,ifelse(data_pk$Day.Time=="Day7_T0.5",168.5,ifelse(data_pk$Day.Time=="Day7_T1",169,ifelse(data_pk$Day.Time=="Day7_T2",170,ifelse(data_pk$Day.Time=="Day7_T4",172,ifelse(data_pk$Day.Time=="Day7_T8",176,ifelse(data_pk$Day.Time=="Day7_T12",180,ifelse(data_pk$Day.Time=="Day8",192,ifelse(data_pk$Day.Time=="Day9",216,240))))))))))
data_pk$time2=data_pk$time-168

##Calculate percent free##
data_pk$cele_perc_free=data_pk$cele_free*100/data_pk$cele_bound
data_pk$nap_perc_free=data_pk$nap_free*100/data_pk$nap_bound

##Convert to mg/L##
data_pk$cele_bound=data_pk$cele_bound/1000
data_pk$cele_free=data_pk$cele_free/1000
data_pk$nap_bound=data_pk$nap_bound/1000
data_pk$nap_free=data_pk$nap_free/1000

##Calculate total in mg/L##
data_pk$cele_total=(data_pk$cele_bound*.0001+data_pk$cele_free*.00035)/.0001
data_pk$nap_total=(data_pk$nap_bound*.0001+data_pk$nap_free*.00035)/.0001

##Add dose##
data_pk$dose=ifelse(data_pk$Treatment=="Celecoxib",100,ifelse(data_pk$Treatment=="Naproxen",250,NA))

library(MESS)
library(dplyr)
##Calculate AUC##
summary_pk=data_pk[which(data_pk$Treatment!="Placebo"),]%>% group_by(SubjectID,Treatment) %>% summarize (auc_cele24=auc(time,cele_bound, from=168, to=192), auc_cele12=auc(time,cele_bound, from=168, to=180),auc_nap24=auc(time,nap_bound, from=168, to=192), auc_nap12=auc(time,nap_bound, from=168, to=180))
summary_pk$cele_cl=100/summary_pk$auc_cele12
summary_pk$nap_cl=250/summary_pk$auc_nap12

mean_raw=data2 %>% group_by(SubjectID, Assay, Treatment) %>% summarize (average=mean(value, na.rm=TRUE))
placebo_mean=subset(mean_raw,Treatment=="Placebo",select=-Treatment) 

baseline=subset(data2,variable=="Day1", select=c("SubjectID","Assay","Treatment","value"))

library(plyr)
baseline=rename(baseline, c("value"="Day1"))

data3=merge(data2,placebo_mean, by=c("SubjectID","Assay"))
data4=merge(data3,baseline, by=c("SubjectID","Assay","Treatment"))
data4$norm_plac=data4$value*100/data4$average
data4$norm_Day1=data4$value*100/data4$Day1

detach(package:plyr)
summary_all=data4 %>% group_by(SubjectID,Assay,Treatment) %>% summarize (auc24=auc(time,value, from=168, to=192),auc12=auc(time,value, from=168, to=180),tmax=time[which.min(value)], auc24_norm_plac=auc(time,norm_plac, from=168, to=192),auc12_norm_plac=auc(time,norm_plac, from=168, to=180),auc6_norm_plac=auc(time, norm_plac, from=168, to=174), auc_peak_norm_plac=auc(time, norm_plac, from=169, to=173),tmax_norm_plac=time[which.min(norm_plac)],auc24_norm_Day1=auc(time,norm_Day1, from=168, to=192),auc12_norm_Day1=auc(time,norm_Day1, from=168, to=180),tmax_norm_Day1=time[which.min(norm_Day1)])

library(plyr)
cele_summary=subset(summary_all,Treatment=="Celecoxib", select=-Treatment)
cele_summary=rename(cele_summary,c("auc24"="cele_auc24","auc12"="cele_auc12","tmax"="cele_tmax","auc24_norm_plac"="cele_auc24_norm_plac","auc12_norm_plac"="cele_auc12_norm_plac","tmax_norm_plac"="cele_tmax_norm_plac","auc24_norm_Day1"="cele_auc24_norm_Day1","auc12_norm_Day1"="cele_auc12_norm_Day1","tmax_norm_Day1"="cele_tmax_norm_Day1"))

nap_summary=subset(summary_all,Treatment=="Naproxen", select=-Treatment)
nap_summary=rename(nap_summary,c("auc24"="nap_auc24","auc12"="nap_auc12","tmax"="nap_tmax","auc24_norm_plac"="nap_auc24_norm_plac","auc12_norm_plac"="nap_auc12_norm_plac","tmax_norm_plac"="nap_tmax_norm_plac","auc24_norm_Day1"="nap_auc24_norm_Day1","auc12_norm_Day1"="nap_auc12_norm_Day1","tmax_norm_Day1"="nap_tmax_norm_Day1"))

plac_summary=subset(summary_all,Treatment=="Placebo", select=-Treatment)
plac_summary=rename(plac_summary,c("auc24"="plac_auc24","auc12"="plac_auc12","tmax"="plac_tmax","auc24_norm_plac"="plac_auc24_norm_plac","auc12_norm_plac"="plac_auc12_norm_plac","tmax_norm_plac"="plac_tmax_norm_plac","auc24_norm_Day1"="plac_auc24_norm_Day1","auc12_norm_Day1"="plac_auc12_norm_Day1","tmax_norm_Day1"="plac_tmax_norm_Day1"))

summary_drug=merge(cele_summary,nap_summary, by=c("SubjectID","Assay"))
summary_wide=merge(summary_drug,plac_summary, by=c("SubjectID","Assay"))

summary_wide$cele_auc12_ratio=summary_wide$cele_auc12/summary_wide$plac_auc12
summary_wide$cele_auc12_ratio_norm_plac=summary_wide$cele_auc12_norm_plac/summary_wide$plac_auc12_norm_plac

summary_wide_COX2=summary_wide[which(summary_wide$Assay=="PGE2"),]
summary(summary_wide_COX2$cele_auc12_ratio)
summary(summary_wide_COX2$cele_auc12_ratio_norm_plac)

data_BP=subset(data_ABPM,data_ABPM$Measurement=="Diastolic" |data_ABPM$Measurement=="Systolic" |data_ABPM$Measurement=="MAP", select=c("SubjectID","Treatment","Hours","Measurement","Value"))

data_PG_norm_plac=data4[c("SubjectID","Assay","Treatment","time","norm_plac")]
data_PG_norm_plac=rename(data_PG_norm_plac,c("Assay"="Measurement", "time"="Hours", "norm_plac"="Value"))

data_PG_norm_Day1=data4[c("SubjectID","Assay","Treatment","time","norm_Day1")]
data_PG_norm_Day1=rename(data_PG_norm_Day1,c("Assay"="Measurement", "time"="Hours", "norm_Day1"="Value"))

data_all_norm_plac=rbind(data_BP,data_PG_norm_plac)
data_all_norm_plac$Assay1=as.factor(ifelse(data_all_norm_plac$Measurement=="Diastolic"|data_all_norm_plac$Measurement=="Systolic"|data_all_norm_plac$Measuremen=="MAP","BP","PG"))
data_all_norm_plac$Assay2=as.factor(ifelse(data_all_norm_plac$Assay1=="BP","BP",as.character(data_all_norm_plac$Measurement)))

data_all_norm_Day1=rbind(data_BP,data_PG_norm_plac)
data_all_norm_Day1$Assay1=as.factor(ifelse(data_all_norm_Day1$Measurement=="Diastolic"|data_all_norm_Day1$Measurement=="Systolic"|data_all_norm_Day1$Measurement=="MAP","BP","PG"))
data_all_norm_Day1$Assay2=as.factor(ifelse(data_all_norm_Day1$Assay1=="BP","BP",as.character(data_all_norm_Day1$Measurement)))

data_ABPM$Interval=cut(data_ABPM$Hours,breaks=seq(167.5,192.5,by=1),labels=0:24)
data_ABPM2=data.frame(cast(data_ABPM[c("SubjectID","Treatment","Measurement","Value","Interval")],SubjectID+Treatment+Interval~Measurement,c(mean, sd,median), value="Value"))


data_ABPM12=data.frame(cast(subset(data_ABPM,as.numeric(data_ABPM$Interval)<13, select=c("SubjectID","Treatment","Measurement","Value")), SubjectID+Treatment~Measurement,c(mean, sd, median), value="Value"))
data_ABPMpeak=data.frame(cast(subset(data_ABPM,as.numeric(data_ABPM$Interval)>1 & as.numeric(data_ABPM$Interval)<5, select=c("SubjectID","Treatment","Measurement","Value")), SubjectID+Treatment~Measurement,c(mean, sd, median), value="Value"))

data_ABPM12b=data.frame(cast(subset(data_ABPM,as.numeric(data_ABPM$Interval)<13, select=c("SubjectID","Treatment","Measurement","Value")), SubjectID~Treatment+Measurement,c(mean, sd, median), value="Value"))
data_ABPMpeakb=data.frame(cast(subset(data_ABPM,as.numeric(data_ABPM$Interval)>1 & as.numeric(data_ABPM$Interval)<5, select=c("SubjectID","Treatment","Measurement","Value")), SubjectID~Treatment+Measurement,c(mean, sd, median), value="Value"))

data_ABPM12b$Cele_MAP_mean_diff=data_ABPM12b$Celecoxib_MAP_mean-data_ABPM12b$Placebo_MAP_mean
data_ABPM12b$Nap_MAP_mean_diff=data_ABPM12b$Naproxen_MAP_mean-data_ABPM12b$Placebo_MAP_mean
data_ABPMpeakb$Cele_MAP_mean_diff=data_ABPMpeakb$Celecoxib_MAP_mean-data_ABPMpeakb$Placebo_MAP_mean
data_ABPMpeakb$Nap_MAP_mean_diff=data_ABPMpeakb$Naproxen_MAP_mean-data_ABPMpeakb$Placebo_MAP_mean

data4$Interval=as.factor(ifelse(data4$time=="168.5", NA,ifelse(data4$time=="168", 0,cut(data4$time,breaks=seq(168,193,by=1),labels=0:24))))
data_COX2=subset(data4,data4$Assay=="PGE2",select=c("SubjectID","Treatment","value","norm_plac","norm_Day1","Interval"))
data_COX2=rename(data_COX2,c("value"="PGE2","norm_plac"="PGE2_norm_plac","norm_Day1"="PGE2_norm_Day1"))
data_COX2_24=subset(data_COX2, is.na(data_COX2$Interval)=="FALSE")

data_COX1=subset(data4,data4$Assay=="TxB2",select=c("SubjectID","Treatment","value","norm_plac","norm_Day1","Interval"))
data_COX1=rename(data_COX1,c("value"="TxB2","norm_plac"="TxB2_norm_plac","norm_Day1"="TxB2_norm_Day1"))

data_COX1_24=subset(data_COX1, is.na(data_COX1$Interval)=="FALSE")

data_COX=merge(data_COX1_24,data_COX2_24, by=c("SubjectID","Treatment","Interval"), all=TRUE)

data_ABPM_COX=merge(data_ABPM2,data_COX,by=c("SubjectID","Treatment","Interval"), all=TRUE)


data_cele=data_pk[which(data_pk$Treatment=="Celecoxib"),]
data_cele_pk=subset(data_cele, time2>=0,select=c("SubjectID","Day.Time","time2","cele_bound","dose"))
library(nlme)
data_cele_pk2=groupedData(data=data_cele_pk[which(is.na(data_cele_pk$cele_bound)=="FALSE"),], formula=cele_bound~time2|SubjectID)
cele_nlme=nlme(cele_bound~SSfol(dose,time2,lKa,lKe,lCl), data=data_cele_pk2, fixed=lKa+lKe+lCl~1,random=lKa+lKe+lCl~1, na.action=na.omit)

cele_lme=lme(log(cele_bound)~time2, data=data_cele_pk2[which(data_cele_pk2$time2>2),])
cele_glm=glm(log(cele_bound)~time2, data=data_cele_pk2[which(data_cele_pk2$time2>2),])

data_nap=data_pk[which(data_pk$Treatment=="Naproxen"),]
data_nap_pk=subset(data_nap, time2>=0,select=c("SubjectID","Day.Time","time2","nap_bound","dose"))

data_nap_pk2=groupedData(data=data_nap_pk[which(is.na(data_nap_pk$nap_bound)=="FALSE"),], formula=nap_bound~time2|SubjectID)
nap_nlme=nlme(nap_free~SSfol(dose,time2,lKa,lKe,lCl), data=data_nap_pk2, fixed=lKa+lKe+lCl~1,random=lKa+lKe+lCl~1, na.action=na.omit)

nap_lme=lme(log(nap_bound)~time2, data=data_nap_pk2[which(data_nap_pk2$time2>2),])
nap_glm=glm(log(nap_bound)~time2, data=data_nap_pk2[which(data_nap_pk2$time2>2),])

nap_par=as.data.frame(coef(nap_lme))
nap_par$half.life=-log(2)/nap_par$time2

cele_par=as.data.frame(coef(cele_lme))
cele_par$half.life=-log(2)/cele_par$time2


data_pk_COX1=merge(data_pk, data4[which(data4$Assay=="TxB2"),], by=c("SubjectID","Treatment","time","time2"))
data_pk_COX=rename(data_pk_COX1,c("value"="TxB2", "norm_plac"="TxB2_norm_plac"))

data_pk_COX2=merge(data_pk_COX, data4[which(data4$Assay=="PGE2"),], by=c("SubjectID","Treatment","time","time2"))
data_pk_pd=rename(data_pk_COX2,c("value"="PGE2", "norm_plac"="PGE2_norm_plac"))

library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),
 axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))

pdf("Lab_meeting_02222018.pdf")
ggplot(data_nap,aes(time2,nap_total))+geom_point(size=3, na.rm=TRUE, color='darkred')+geom_line(na.rm=TRUE, color='darkred')+xlim(0,24)+pub_specs +facet_wrap(~SubjectID, ncol=3, nrow=5)+theme(legend.position="bottom")+labs(title="Total Naproxen",x="Hours",y="Plasma Concentrations (mg/l)")
ggplot(data_cele,aes(time2,cele_total))+geom_point(size=3, na.rm=TRUE, color='darkblue')+geom_line(na.rm=TRUE, color='darkblue')+xlim(0,24)+pub_specs +facet_wrap(~SubjectID, ncol=3,nrow=5)+theme(legend.position="bottom")+labs(title="Total Celecoxib",x="Hours",y="Plasma Concentrations (mg/l)")

ggplot(data_nap,aes(time2,nap_total))+geom_point(size=3, na.rm=TRUE, color='darkred')+geom_line(na.rm=TRUE, color='darkred')+xlim(0,24)+pub_specs +facet_wrap(~SubjectID, ncol=3, nrow=5, scales="free_y")+theme(legend.position="bottom")+labs(title="Total Naproxen",x="Hours",y="Plasma Concentrations (mg/l)")
ggplot(data_cele,aes(time2,cele_total))+geom_point(size=3, na.rm=TRUE, color='darkblue')+geom_line(na.rm=TRUE, color='darkblue')+xlim(0,24)+pub_specs +facet_wrap(~SubjectID, ncol=3,nrow=5, scales="free_y")+theme(legend.position="bottom")+labs(title="Total Celecoxib",x="Hours",y="Plasma Concentrations (mg/l)")

ggplot(data_nap,aes(time2,nap_total))+geom_point(size=3, na.rm=TRUE, color='darkred')+geom_smooth()+xlim(0,72)+pub_specs +theme(legend.position="bottom")+labs(title="Total Naproxen",x="Hours",y="Plasma Concentrations (mg/l)")
ggplot(data_cele,aes(time2,cele_total))+geom_point(size=3, na.rm=TRUE, color='darkblue')+geom_smooth()+xlim(0,72)+pub_specs +theme(legend.position="bottom")+labs(title="Total Celecoxib",x="Hours",y="Plasma Concentrations (mg/l)")

ggplot(data_nap,aes(nap_total, nap_perc_free,color=Treatment))+ylim(0,10)+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Naproxen",x="Plasma Concentrations (mg/l)",y="Percent Free")
ggplot(data_cele,aes(cele_total,cele_perc_free,color=SubjectID))+ylim(0,10)+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Celecoxib",y="Percent Free",x="Plasma Concentrations (mg/l)")

ggplot(data_nap,aes(nap_total, nap_perc_free))+ylim(0,10)+geom_point(size=3, na.rm=TRUE, color='darkred')+geom_line(color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Naproxen",x="Plasma Concentrations (mg/l)",y="Percent Free")+facet_wrap(~SubjectID, ncol=3, nrow=5)
ggplot(data_cele,aes(cele_total,cele_perc_free))+ylim(0,10)+geom_point(size=3, na.rm=TRUE, color='darkblue')+geom_line(color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Celecoxib",y="Percent Free",x="Plasma Concentrations (mg/l)")+facet_wrap(~SubjectID, ncol=3, nrow=5)

ggplot(data_nap,aes(nap_total, nap_perc_free))+geom_point(size=3, na.rm=TRUE, color='darkred')+geom_line(color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Naproxen",x="Plasma Concentrations (mg/l)",y="Percent Free")+facet_wrap(~SubjectID, ncol=3, nrow=5, scale="free_y")
ggplot(data_cele,aes(cele_total,cele_perc_free))+geom_point(size=3, na.rm=TRUE, color='darkblue')+geom_line(color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Celecoxib",y="Percent Free",x="Plasma Concentrations (mg/l)")+facet_wrap(~SubjectID, ncol=3, nrow=5, scale="free_y")




ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_total,PGE2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Celecoxib",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_total,TxB2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Celecoxib",y="Serum TxB2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()

ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_free,PGE2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Free Celecoxib",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_free,TxB2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Free Celecoxib",y="Serum TxB2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()

ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_total,PGE2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Naproxen",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_total,TxB2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Naproxen",y="Serum TxB2\n(%activity relative to placebo))",x="Plasma Concentrations (mg/l)")+scale_x_log10()

ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_free,PGE2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Free Naproxen",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_free,TxB2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Free Naproxen",y="Serum TxB2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()

ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_total,PGE2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Celecoxib",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_total,TxB2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Celecoxib",y="Serum TxB2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")

ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_free,PGE2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Free Celecoxib",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_free,TxB2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Free Celecoxib",y="Serum TxB2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")

ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_total,PGE2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Naproxen",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_total,TxB2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Naproxen",y="Serum TxB2\n(%activity relative to placebo))",x="Plasma Concentrations (mg/l)")

ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_free,PGE2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Free Naproxen",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_free,TxB2_norm_plac))+ylim(0,200)+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Free Naproxen",y="Serum TxB2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")



ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_total,PGE2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Celecoxib",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()+facet_wrap(~SubjectID, ncol=3, nrow=5, scale="free")
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_total,TxB2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Celecoxib",y="Serum TxB2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()+facet_wrap(~SubjectID, ncol=3, nrow=5, scale="free")

ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_free,PGE2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Free Celecoxib",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()+facet_wrap(~SubjectID, ncol=3, nrow=5, scale="free")
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),],aes(cele_free,TxB2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkblue')+pub_specs +theme(legend.position="bottom")+labs(title="Free Celecoxib",y="Serum TxB2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()+facet_wrap(~SubjectID, ncol=3, nrow=5, scale="free")

ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_total,PGE2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Naproxen",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()+facet_wrap(~SubjectID, ncol=3, nrow=5, scale="free")
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_total,TxB2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Naproxen",y="Serum TxB2\n(%activity relative to placebo))",x="Plasma Concentrations (mg/l)")+scale_x_log10()+facet_wrap(~SubjectID, ncol=3, nrow=5, scale="free")

ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_free,PGE2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Free Naproxen",y="Plasma PGE2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()+facet_wrap(~SubjectID, ncol=3, nrow=5, scale="free")
ggplot(data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),],aes(nap_free,TxB2_norm_plac))+geom_point(size=3, na.rm=TRUE, color='darkred')+pub_specs +theme(legend.position="bottom")+labs(title="Free Naproxen",y="Serum TxB2\n(%activity relative to placebo)",x="Plasma Concentrations (mg/l)")+scale_x_log10()+facet_wrap(~SubjectID, ncol=3, nrow=5, scale="free")


dev.off()


nap_pk_pd=data_pk_pd[which(data_pk_pd$Treatment=="Naproxen"),]
cele_pk_pd=data_pk_pd[which(data_pk_pd$Treatment=="Celecoxib"),]


write.csv(cele_par, "Cele_par.csv")
write.csv(nap_par, "Nap_par.csv")
write.csv(summary_pk[which(summary_pk$Treatment=="Celecoxib"),], "Cele_summary.csv")
write.csv(summary_pk[which(summary_pk$Treatment=="Naproxen"),], "Nap_summary.csv")



