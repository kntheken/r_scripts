##Read in data##
data=read.csv("COX_assay_results8162017.csv")
data_ABPM=read.csv("ABPM_data_8232017.csv")

data_ABPM$Time2=strptime(data_ABPM$Time,"%H:%M:%S")
data_ABPM$Base_Time=strptime("08:00:00", "%H:%M:%S")
data_ABPM$Hours=as.numeric(difftime(data_ABPM$Time2,data_ABPM$Base_Time, units ="hours")+data_ABPM$Day*24)


library(reshape)
data2=melt(data,id.vars=c("SubjectID","Assay", "Treatment"))

##Add time in hours##
data2$time=ifelse(data2$variable=="Day1",0,ifelse(data2$variable=="Day7_T0",168,ifelse(data2$variable=="Day7_T0.5",168.5,ifelse(data2$variable=="Day7_T1",169,ifelse(data2$variable=="Day7_T2",170,ifelse(data2$variable=="Day7_T4",172,ifelse(data2$variable=="Day7_T8",176,ifelse(data2$variable=="Day7_T12",180,ifelse(data2$variable=="Day8",192,ifelse(data2$variable=="Day9",216,240))))))))))
data2$time2=data2$time-168

library(MESS)
library(dplyr)

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

summary_ABPM_COX=merge(summary_all,data_ABPM12, by=c("SubjectID","Treatment"),all=TRUE)

data_ABPM12_plac=subset(data_ABPM12,data_ABPM12$Treatment=="Placebo", select=c("SubjectID","Diastolic_mean", "Systolic_mean","MAP_mean","HR_mean", "PP_mean"))
data_ABPM12_plac=rename(data_ABPM12_plac,c("Diastolic_mean"="Diastolic_plac", "Systolic_mean"="Systolic_plac","MAP_mean"="MAP_plac","HR_mean"="HR_plac", "PP_mean"="PP_plac"))

data_ABPMpeak_plac=subset(data_ABPMpeak,data_ABPMpeak$Treatment=="Placebo",select=c("SubjectID","Diastolic_mean", "Systolic_mean","MAP_mean","HR_mean", "PP_mean"))
data_ABPMpeak_plac=rename(data_ABPMpeak_plac,c("Diastolic_mean"="Diastolic_plac", "Systolic_mean"="Systolic_plac","MAP_mean"="MAP_plac","HR_mean"="HR_plac", "PP_mean"="PP_plac"))

data_ABPM12c=merge(data_ABPM12,data_ABPM12_plac, by="SubjectID")
data_ABPMpeakc=merge(data_ABPMpeak,data_ABPMpeak_plac, by="SubjectID")

summary_ABPM12c_COX=merge(summary_all, data_ABPM12c, by=c("SubjectID","Treatment"))
summary_ABPMpeakc_COX=merge(summary_all, data_ABPMpeakc, by=c("SubjectID","Treatment"))

summary_ABPM12c_COX$MAP_diff=summary_ABPM12c_COX$MAP_mean-summary_ABPM12c_COX$MAP_plac
summary_ABPMpeakc_COX$MAP_diff=summary_ABPMpeakc_COX$MAP_mean-summary_ABPMpeakc_COX$MAP_plac
summary_ABPM12c_COX$SBP_diff=summary_ABPM12c_COX$Systolic_mean-summary_ABPM12c_COX$Systolic_plac
summary_ABPMpeakc_COX$SBP_diff=summary_ABPMpeakc_COX$Systolic_mean-summary_ABPMpeakc_COX$Systolic_plac

summary_ABPM12c_COX$Group=as.factor(ifelse(summary_ABPM12c_COX$MAP_diff>=5, "Increase",ifelse(summary_ABPM12c_COX$MAP_diff<=-5,"Decrease","No Change")))
summary_ABPM12c_COX$Group2=as.factor(ifelse(summary_ABPM12c_COX$MAP_diff>=5, "Increase",ifelse(summary_ABPM12c_COX$MAP_diff<=-5,"Decrease","No Change")))
summary_ABPMpeakc_COX$Group=as.factor(ifelse(summary_ABPMpeakc_COX$MAP_diff>=5, "Increase",ifelse(summary_ABPMpeakc_COX$MAP_diff<=-5,"Decrease","No Change")))
summary_ABPMpeakc_COX$Group2=as.factor(ifelse(summary_ABPMpeakc_COX$MAP_diff>=5, "Increase",ifelse(summary_ABPMpeakc_COX$MAP_diff<=-5,"Decrease","No Change")))
summary_ABPM12c_COX$Group3=as.factor(ifelse(summary_ABPM12c_COX$SBP_diff>=5, "Increase",ifelse(summary_ABPM12c_COX$SBP_diff<=-5,"Decrease","No Change")))
summary_ABPMpeakc_COX$Group3=as.factor(ifelse(summary_ABPMpeakc_COX$SBP_diff>=5, "Increase",ifelse(summary_ABPMpeakc_COX$SBP_diff<=-5,"Decrease","No Change")))


test=data.frame(cast(summary_ABPM12c_COX[c("SubjectID","Assay","Treatment","Group")],SubjectID+Assay~Treatment, fun.aggregate=identity,value="Group"))
test$Group2=ifelse(test$Celecoxib=="Increase"|test$Naproxen=="Increase","Increase", ifelse(test$Celecoxib=="Decrease"|test$Naproxen=="Decrease","Decrease","No Change"))
test2=data.frame(cast(summary_ABPMpeakc_COX[c("SubjectID","Assay","Treatment","Group")],SubjectID+Assay~Treatment, fun.aggregate=identity,value="Group"))
test2$Group2=ifelse(test2$Celecoxib=="Increase"|test2$Naproxen=="Increase","Increase", ifelse(test2$Celecoxib=="Decrease"|test2$Naproxen=="Decrease","Decrease","No Change"))

test3=data.frame(cast(summary_ABPM12c_COX[c("SubjectID","Assay","Treatment","Group3")],SubjectID+Assay~Treatment, fun.aggregate=identity,value="Group3"))
test3$Group4=ifelse(test3$Celecoxib=="Increase"|test3$Naproxen=="Increase","Increase", ifelse(test3$Celecoxib=="Decrease"|test3$Naproxen=="Decrease","Decrease","No Change"))
test4=data.frame(cast(summary_ABPMpeakc_COX[c("SubjectID","Assay","Treatment","Group3")],SubjectID+Assay~Treatment, fun.aggregate=identity,value="Group3"))
test4$Group4=ifelse(test4$Celecoxib=="Increase"|test4$Naproxen=="Increase","Increase", ifelse(test4$Celecoxib=="Decrease"|test4$Naproxen=="Decrease","Decrease","No Change"))



summary_ABPM12d_COX=merge(summary_ABPM12c_COX,test[c("SubjectID","Assay","Group2")], by=c("SubjectID","Assay"))
summary_ABPMpeakd_COX=merge(summary_ABPMpeakc_COX,test2[c("SubjectID","Assay","Group2")], by=c("SubjectID","Assay"))

data_ABPM_COX2=merge(data_ABPM_COX,test2[c("SubjectID","Group2")], by="SubjectID")
data_ABPM_COX3=merge(data_ABPM_COX,summary_ABPMpeakc_COX[c("SubjectID","Treatment","Group")], by=c("SubjectID","Treatment"))
data_ABPM_COX4=merge(data_ABPM_COX,test[c("SubjectID","Group2")], by="SubjectID")
data_ABPM_COX5=merge(data_ABPM_COX4,test3[c("SubjectID","Group4")], by="SubjectID")
data_ABPM_COX6=merge(data_ABPM_COX2,test4[c("SubjectID","Group4")], by="SubjectID")

ggplot(summary_ABPM12d_COX[which(summary_ABPM_COX$Assay=="PGE2"),],aes(auc12_norm_plac,MAP_mean,group=SubjectID,color=Group2))+geom_line(na.rm=TRUEr)+geom_point(size=3, na.rm=TRUE)+pub_specs +theme(legend.position="bottom")+labs(title="COX-2 Activity vs Mean Arterial Pressure",x="COX-2 Activity\n (AUC over dosing interval)",y="Mean Arterial Pressure (mm Hg)")
ggplot(summary_ABPM_COX[which(summary_ABPM_COX$Assay=="PGE2"),],aes(auc12_norm_plac,Systolic_mean,group=SubjectID,color=Treatment, label=SubjectID))+geom_line()+geom_point(size=3, na.rm=TRUE)+pub_specs +theme(legend.position="bottom")+labs(title="COX-2 Activity vs Systolic Blood Pressure",x="COX-2 Activity\n (AUC over dosing interval)",y="Systolic Blood Pressure (mm Hg)")+geom_text(vjust=0, nudge_y=0.5, check_overlap=TRUE)
ggplot(summary_ABPM_COX[which(summary_ABPM_COX$Assay=="PGE2"),],aes(auc12_norm_plac,Diastolic_mean,group=SubjectID,color=Treatment, label=SubjectID))+geom_line()+geom_point(size=3, na.rm=TRUE)+pub_specs +theme(legend.position="bottom")+labs(title="COX-2 Activity vs Diastolic Blood Pressure",x="COX-2 Activity\n (AUC over dosing interval)",y="Diastolic Blood Pressure (mm Hg)")+geom_text(vjust=0, nudge_y=0.5, check_overlap=TRUE)

ggplot(summary_ABPM_COX[which(summary_ABPM_COX$Assay=="TxB2"),],aes(auc12_norm_plac,MAP_mean,group=SubjectID,color=Treatment, label=SubjectID))+geom_line()+geom_point(size=3, na.rm=TRUE)+pub_specs +theme(legend.position="bottom")+labs(title="COX-1 Activity vs Mean Arterial Pressure",x="COX-1 Activity\n (AUC over dosing interval)",y="Mean Arterial Pressure (mm Hg)")+geom_text(vjust=0, nudge_y=0.5, check_overlap=TRUE)
ggplot(summary_ABPM_COX[which(summary_ABPM_COX$Assay=="TxB2"),],aes(auc12_norm_plac,Systolic_mean,group=SubjectID,color=Treatment, label=SubjectID))+geom_line()+geom_point(size=3, na.rm=TRUE)+pub_specs +theme(legend.position="bottom")+labs(title="COX-1 Activity vs Systolic Blood Pressure",x="COX-1 Activity\n (AUC over dosing interval)",y="Systolic Blood Pressure (mm Hg)")+geom_text(vjust=0, nudge_y=0.5, check_overlap=TRUE)
ggplot(summary_ABPM_COX[which(summary_ABPM_COX$Assay=="TxB2"),],aes(auc12_norm_plac,Diastolic_mean,group=SubjectID,color=Treatment, label=SubjectID))+geom_line()+geom_point(size=3, na.rm=TRUE)+pub_specs +theme(legend.position="bottom")+labs(title="COX-1 Activity vs Diastolic Blood Pressure",x="COX-1 Activity\n (AUC over dosing interval)",y="Diastolic Blood Pressure (mm Hg)")+geom_text(vjust=0, nudge_y=0.5, check_overlap=TRUE)

pdf("More_Clinical_Study_Graphs.pdf")
ggplot(summary_ABPM_COX,aes(Treatment,auc_peak_norm_plac,fill=Treatment))+geom_boxplot()+pub_specs +facet_grid(.~Assay)+theme(legend.position="bottom")+labs(title="COX Suppression over Time by Treatment",subtitle="9 am to 1 pm",y="AUC")
ggplot(summary_ABPM_COX,aes(Treatment,auc6_norm_plac,fill=Treatment))+geom_boxplot()+pub_specs +facet_grid(.~Assay)+theme(legend.position="bottom")+labs(title="COX Suppression over Time by Treatment",subtitle="8 am to 2 pm",y="AUC")
ggplot(summary_ABPM_COX,aes(Treatment,auc12_norm_plac,fill=Treatment))+geom_boxplot()+pub_specs +facet_grid(.~Assay)+theme(legend.position="bottom")+labs(title="COX Suppression over Time by Treatment",subtitle="8 am to 8 pm",y="AUC")
ggplot(summary_ABPM_COX,aes(Treatment,auc24_norm_plac,fill=Treatment))+geom_boxplot()+pub_specs +facet_grid(.~Assay)+theme(legend.position="bottom")+labs(title="COX Suppression over Time by Treatment",subtitle="24 hours",y="AUC")

ggplot(data4,aes(time,norm_plac,fill=Treatment))+geom_boxplot(aes(group=cut_width(time,0.5)))+xlim(168,192)+facet_grid(Assay~Treatment)+pub_specs +labs(title="Ex vivo COX Activity",x="Hours",y="COX Activity\n (%activity relative to placebo)")+theme(legend.position="none")+geom_hline(yintercept=100)
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,value,fill=Treatment))+geom_boxplot(aes(group=cut_width(time,0.5)))+xlim(168,192)+facet_grid(.~Treatment)+pub_specs +labs(title="Ex vivo COX-2 Activity",x="Hours",y="Plasma PGE2 (ng/ml)")+theme(legend.position="none")
ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,value,fill=Treatment))+geom_boxplot(aes(group=cut_width(time,0.5)))+xlim(168,192)+facet_grid(.~Treatment)+pub_specs +labs(title="Ex vivo COX-1 Activity",x="Hours",y="Serum TxB2 (ng/ml)")+theme(legend.position="none")

ggplot(data4,aes(time,norm_plac,color=Treatment))+geom_point()+stat_summary(fun.y="median", size=1, geom="line")+xlim(168,192)+facet_grid(Assay~Treatment)+pub_specs +labs(title="Ex vivo COX Activity",x="Hours",y="COX Activity\n (%activity relative to placebo)")+theme(legend.position="none")+geom_hline(yintercept=100)+ylim(0,300)
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,value,color=Treatment))+geom_point()+stat_summary(fun.y="median", size=1, geom="line")+scale_x_log10()+pub_specs +labs(title="Ex vivo COX-2 Activity",x="Hours",y="Plasma PGE2 (ng/ml)")+theme(legend.position="none")+ylim(0,50)
ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,value,color=Treatment))+geom_point()+stat_summary(fun.y="median", size=1, geom="line")+xlim(168,192)+facet_grid(.~Treatment)+pub_specs +labs(title="Ex vivo COX-1 Activity",x="Hours",y="Serum TxB2 (ng/ml)")+theme(legend.position="none")+ylim(0,300)

ggplot(summary_ABPMpeakd_COX,aes(Group2,auc_peak_norm_plac,fill=Group2))+geom_boxplot()+facet_grid(Assay~Treatment)+pub_specs +theme(legend.position="bottom")+labs(title="COX Suppression by Blood Pressure Response Group*",subtitle="9 am - 1 pm",y="AUC", caption="*Greater than 5 mm Hg change in MAP from placebo for either NSAID")
ggplot(summary_ABPM12d_COX,aes(Group2,auc12_norm_plac,fill=Group2))+geom_boxplot()+facet_grid(Assay~Treatment)+pub_specs +theme(legend.position="bottom")+labs(title="COX Suppression by Blood Pressure Response Group",subtitle="8 am - 8 pm",y="AUC", caption="*Greater than 5 mm Hg change in MAP from placebo for either NSAID")

ggplot(summary_ABPMpeakd_COX,aes(Group,auc_peak_norm_plac,fill=Group))+geom_boxplot()+facet_grid(Assay~Treatment)+pub_specs +theme(legend.position="bottom")+labs(title="COX Suppression by Blood Pressure Response Group*",subtitle="9 am - 1 pm",y="AUC", caption="*Greater than 5 mm Hg change in MAP from placebo for each NSAID")
ggplot(summary_ABPM12d_COX,aes(Group,auc12_norm_plac,fill=Group))+geom_boxplot()+facet_grid(Assay~Treatment)+pub_specs +theme(legend.position="bottom")+labs(title="COX Suppression by Blood Pressure Response Group",subtitle="8 am - 8 pm",y="AUC", caption="*Greater than 5 mm Hg change in MAP from placebo for each NSAID")

ggplot(summary_ABPMpeakd_COX[which(summary_ABPMpeakd_COX$Treatment=="Celecoxib"|summary_ABPMpeakd_COX$Treatment=="Naproxen"),],aes(Group,auc_peak_norm_plac,fill=Group))+geom_boxplot()+facet_grid(Assay~Treatment)+pub_specs +theme(legend.position="bottom")+labs(title="COX Suppression by Blood Pressure Response Group*",subtitle="9 am - 1 pm",y="AUC", caption="*Greater than 5 mm Hg change in MAP from placebo for each NSAID")
ggplot(summary_ABPM12d_COX[which(summary_ABPMpeakd_COX$Treatment=="Celecoxib"|summary_ABPMpeakd_COX$Treatment=="Naproxen"),],aes(Group,auc12_norm_plac,fill=Group))+geom_boxplot()+facet_grid(Assay~Treatment)+pub_specs +theme(legend.position="bottom")+labs(title="COX Suppression by Blood Pressure Response Group",subtitle="8 am - 8 pm",y="AUC", caption="*Greater than 5 mm Hg change in MAP from placebo for each NSAID")

ggplot(data_ABPM_COX5,aes(PGE2_norm_plac,Systolic_mean, color=Group4))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Systolic Blood Pressure",subtitle="8am - 8pm" ,y="Systolic Blood Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX6,aes(PGE2_norm_plac,Systolic_mean, color=Group4))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Systolic Blood Pressure", y="Systolic Blood Pressure (mm Hg)", subtitle="9am - 1 pm",x="COX-2 Activity (% relative to placebo)")




ggplot(data_ABPM_COX2,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(Treatment~Group2)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX2,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(.~Group2)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX2,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX2,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")

ggplot(data_ABPM_COX2,aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(Treatment~Group2)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX2,aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(.~Group2)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX2,aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX2,aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")

ggplot(data_ABPM_COX4,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(Treatment~Group2)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(.~Group2)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")

ggplot(data_ABPM_COX4,aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(Treatment~Group2)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4,aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(.~Group2)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4,aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4,aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")

ggplot(data_ABPM_COX4[which(data_ABPM_COX4$Treatment=="Celecoxib"|data_ABPM_COX4$Treatment=="Naproxen"),],aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(Treatment~Group2)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4[which(data_ABPM_COX4$Treatment=="Celecoxib"|data_ABPM_COX4$Treatment=="Naproxen"),],aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(.~Group2)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4[which(data_ABPM_COX4$Treatment=="Celecoxib"|data_ABPM_COX4$Treatment=="Naproxen"),],aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4[which(data_ABPM_COX4$Treatment=="Celecoxib"|data_ABPM_COX4$Treatment=="Naproxen"),],aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")

ggplot(data_ABPM_COX4[which(data_ABPM_COX4$Treatment=="Celecoxib"|data_ABPM_COX4$Treatment=="Naproxen"),],aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(Treatment~Group2)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4[which(data_ABPM_COX4$Treatment=="Celecoxib"|data_ABPM_COX4$Treatment=="Naproxen"),],aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(.~Group2)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4[which(data_ABPM_COX4$Treatment=="Celecoxib"|data_ABPM_COX4$Treatment=="Naproxen"),],aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX4[which(data_ABPM_COX4$Treatment=="Celecoxib"|data_ABPM_COX4$Treatment=="Naproxen"),],aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")


ggplot(data_ABPM_COX4,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(.~Treatment)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")

ggplot(data_ABPM_COX4,aes(PGE2_norm_plac,TxB2_norm_plac, color=Treatment))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+scale_x_log10()+scale_y_log10()
dev.off()

ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,value,color=SubjectID))+geom_point(size=3, na.rm=TRUE)+geom_smooth(na.rm=TRUE)+xlim(168,192)+facet_grid(.~Treatment)+pub_specs +labs(title="Ex vivo COX-2 Activity",x="Hours",y="Plasma PGE2 (ng/ml)")+theme(legend.position="none")
ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,value,color=SubjectID))+geom_point(size=3, na.rm=TRUE)+geom_smooth(na.rm=TRUE)+xlim(168,192)+facet_grid(.~Treatment)+pub_specs +labs(title="Ex vivo COX-1 Activity",x="Hours",y="Serum TxB2 (ng/ml)")+theme(legend.position="none")



library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),
 axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))


pdf("Clinical_Study_BP_Analysis.pdf")
ggplot(data_ABPMpeak,aes(Treatment,Systolic_mean, color=Treatment))+geom_point()+geom_errorbar(aes(ymin=Systolic_mean-Systolic_sd, ymax=Systolic_mean+Systolic_sd))+pub_specs+facet_wrap(~SubjectID, nrow=3, ncol=5)+theme(legend.position="bottom")+labs(title="Systolic Blood Pressure", subtitle="9 am - 1 pm", y="Systolic Blood Pressure (mm Hg)")+theme(axis.text.x=element_text(angle=-45, hjust=0))
ggplot(data_ABPMpeak,aes(Treatment,Diastolic_mean, color=Treatment))+geom_point()+geom_errorbar(aes(ymin=Diastolic_mean-Diastolic_sd, ymax=Diastolic_mean+Diastolic_sd))+pub_specs+facet_wrap(~SubjectID, nrow=3, ncol=5)+theme(legend.position="bottom")+labs(title="Diastolic Blood Pressure", subtitle="9 am - 1 pm", y="Diastolic Blood Pressure (mm Hg)")+theme(axis.text.x=element_text(angle=-45, hjust=0))
ggplot(data_ABPMpeak,aes(Treatment,MAP_mean, color=Treatment))+geom_point()+geom_errorbar(aes(ymin=MAP_mean-MAP_sd, ymax=MAP_mean+MAP_sd))+pub_specs+facet_wrap(~SubjectID, nrow=3, ncol=5)+theme(legend.position="bottom")+labs(title="Mean Arterial Pressure", subtitle="9 am - 1 pm", y="Mean Arterial Pressure (mm Hg)")+theme(axis.text.x=element_text(angle=-45, hjust=0))

ggplot(data_ABPM12,aes(Treatment,Systolic_mean, color=Treatment))+geom_point()+geom_errorbar(aes(ymin=Systolic_mean-Systolic_sd, ymax=Systolic_mean+Systolic_sd))+pub_specs+facet_wrap(~SubjectID, nrow=3, ncol=5)+theme(legend.position="bottom")+labs(title="Systolic Blood Pressure", subtitle="8 am - 8 pm", y="Systolic Blood Pressure (mm Hg)")+theme(axis.text.x=element_text(angle=-45, hjust=0))
ggplot(data_ABPM12,aes(Treatment,Diastolic_mean, color=Treatment))+geom_point()+geom_errorbar(aes(ymin=Diastolic_mean-Diastolic_sd, ymax=Diastolic_mean+Diastolic_sd))+pub_specs+facet_wrap(~SubjectID, nrow=3, ncol=5)+theme(legend.position="bottom")+labs(title="Diastolic Blood Pressure", subtitle="8 am - 8 pm", y="Diastolic Blood Pressure (mm Hg)")+theme(axis.text.x=element_text(angle=-45, hjust=0))
ggplot(data_ABPM12,aes(Treatment,MAP_mean, color=Treatment))+geom_point()+geom_errorbar(aes(ymin=MAP_mean-MAP_sd, ymax=MAP_mean+MAP_sd))+pub_specs+facet_wrap(~SubjectID, nrow=3, ncol=5)+theme(legend.position="bottom")+labs(title="Mean Arterial Pressure", subtitle="8 am - 8 pm", y="Mean Arterial Pressure (mm Hg)")+theme(axis.text.x=element_text(angle=-45, hjust=0))


ggplot(data_ABPM_COX,aes(PGE2_norm_plac,MAP_mean,color=Treatment))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", alpha=0)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX,aes(PGE2_norm_plac,Systolic_mean,color=Treatment))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", alpha=0)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Systolic Blood Pressure", y="Systolic Blood Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")
ggplot(data_ABPM_COX,aes(PGE2_norm_plac,Diastolic_mean,color=Treatment))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", alpha=0)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Diastolic Blood Pressure", y="Diastolic Blood Pressure (mm Hg)", x="COX-2 Activity (% relative to placebo)")

ggplot(data_ABPM_COX,aes(TxB2_norm_plac,MAP_mean,color=Treatment))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", alpha=0)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX,aes(TxB2_norm_plac,Systolic_mean,color=Treatment))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", alpha=0)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Systolic Blood Pressure", y="Systolic Blood Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")
ggplot(data_ABPM_COX,aes(TxB2_norm_plac,Diastolic_mean,color=Treatment))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", alpha=0)+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Diastolic BloodPressure", y="Diastolic Blood Pressure (mm Hg)", x="COX-1 Activity (% relative to placebo)")

dev.off()

pdf("Clinical_Study_Preliminary_Analysis.pdf")
ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,norm_plac,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+scale_x_log10()+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-1 Activity",x="Hours",y="Serum TxB2\n (%activity relative to placebo)")
png("COX-2_Activity.png")
ggplot(data4[which(data4$Assay=="PGE2"&data4$Treatment=="Celecoxib"),],aes(time2,norm_plac,color=SubjectID))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(0,12)+ylim(0,200)+pub_specs +facet_wrap(~SubjectID, nrow=3)+theme(legend.position="none")+geom_hline(yintercept=100)+labs(x="Hours",y="COX-2 Activity\n (%activity relative to placebo)")+scale_x_continuous(breaks=c(0,2,4,6,8,10,12), limits=c(0,12))
dev.off()

ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,norm_Day1,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-1 Activity",x="Hours",y="Serum TxB2\n (%activity relative to baseline)")
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,norm_Day1,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-2 Activity",x="Hours",y="Plasma PGE2\n (%activity relative to baseline)")

ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,value,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,750)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+labs(title="COX-1 Activity",x="Hours",y="Serum TxB2 (ng/ml)")
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,value,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,55)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+labs(title="COX-2 Activity",x="Hours",y="Plasma PGE2 (ng/ml)")


ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,norm_plac,color=SubjectID))+geom_point(size=3, na.rm=TRUE)+geom_smooth(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~Treatment)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-1 Activity",x="Hours",y="Serum TxB2\n (%activity relative to placebo)")
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,norm_plac,color=SubjectID))+geom_point(size=3, na.rm=TRUE)+geom_smooth(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~Treatment)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-2 Activity",x="Hours",y="Plasma PGE2\n (%activity relative to placebo)")

ggplot(data4,aes(time,norm_plac,color=SubjectID))+geom_point(size=3, na.rm=TRUE)+geom_smooth(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_grid(Assay~Treatment)+geom_hline(yintercept=100)+labs(title="Ex vivo COX Activity",x="Hours",y="COX Activity\n (%activity relative to placebo)")+theme(legend.position="none")

ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="1" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 1")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="2" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 2")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="27" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 27")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="30" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 30")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="36" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 36")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="69" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 69")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="72" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 72")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="78" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 78")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="95" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 95")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="96" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+ylim(0,300)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 96")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="127" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 127")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="145" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 145")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="181" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 181")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="205" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 205")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="265" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 265")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)

ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="1" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 1")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="2" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 2")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="27" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 27")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="30" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 30")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="36" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 36")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="69" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 69")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="72" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 72")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="78" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 78")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="95" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 95")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="96" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+ylim(0,300)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 96")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="127" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 127")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="145" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 145")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="181" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 181")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="205" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 205")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="265" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Treatment~.)+theme(legend.position="bottom")+labs(title="Subject 265")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)


ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="1" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 1")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="2" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 2")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="27" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 27")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="30" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 30")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="36" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 36")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="69" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 69")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="72" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 72")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="78" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 78")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="95" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 95")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="96" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+ylim(0,350)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 96")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="127" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 127")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="145" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 145")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="181" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 181")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="205" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 205")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="265" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 265")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)

ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="1" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 1")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="2" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 2")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="27" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 27")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="30" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 30")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="36" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 36")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="69" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 69")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="72" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 72")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="78" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 78")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="95" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 95")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="96" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+ylim(0,350)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 96")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="127" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 127")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="145" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 145")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="181" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 181")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="205" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 205")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all_norm_plac[which(data_all_norm_plac$SubjectID=="265" ),],aes(Hours,Value,color=Measurement))+geom_line(size=1)+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 265")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)

dev.off()
