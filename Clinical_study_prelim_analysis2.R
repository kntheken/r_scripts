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

summary_all=data4 %>% group_by(SubjectID,Assay,Treatment) %>% summarize (auc24=auc(time,value, from=168, to=192),auc12=auc(time,value, from=168, to=180),tmax=time[which.min(value)], auc24_norm_plac=auc(time,norm_plac, from=168, to=192),auc12_norm_plac=auc(time,norm_plac, from=168, to=180),tmax_norm_plac=time[which.min(norm_plac)],auc24_norm_Day1=auc(time,norm_Day1, from=168, to=192),auc12_norm_Day1=auc(time,norm_Day1, from=168, to=180),tmax_norm_Day1=time[which.min(norm_Day1)])

cele_summary=subset(summary_all,Treatment=="Celecoxib", select=-Treatment)
cele_summary=rename(cele_summary,c("auc24"="cele_auc24","auc12"="cele_auc12","tmax"="cele_tmax","auc24_norm_plac"="cele_auc24_norm_plac","auc12_norm_plac"="cele_auc12_norm_plac","tmax_norm_plac"="cele_tmax_norm_plac","auc24_norm_Day1"="cele_auc24_norm_Day1","auc12_norm_Day1"="cele_auc12_norm_Day1","tmax_norm_Day1"="cele_tmax_norm_Day1"))

nap_summary=subset(summary_all,Treatment=="Naproxen", select=-Treatment)
nap_summary=rename(nap_summary,c("auc24"="nap_auc24","auc12"="nap_auc12","tmax"="nap_tmax","auc24_norm_plac"="nap_auc24_norm_plac","auc12_norm_plac"="nap_auc12_norm_plac","tmax_norm_plac"="nap_tmax_norm_plac","auc24_norm_Day1"="nap_auc24_norm_Day1","auc12_norm_Day1"="nap_auc12_norm_Day1","tmax_norm_Day1"="nap_tmax_norm_Day1"))

plac_summary=subset(summary_all,Treatment=="Placebo", select=-Treatment)
plac_summary=rename(plac_summary,c("auc24"="plac_auc24","auc12"="plac_auc12","tmax"="plac_tmax","auc24_norm_plac"="plac_auc24_norm_plac","auc12_norm_plac"="plac_auc12_norm_plac","tmax_norm_plac"="plac_tmax_norm_plac","auc24_norm_Day1"="plac_auc24_norm_Day1","auc12_norm_Day1"="plac_auc12_norm_Day1","tmax_norm_Day1"="plac_tmax_norm_Day1"))

summary_drug=merge(cele_summary,nap_summary, by=c("SubjectID","Assay"))
summary_wide=merge(summary_drug,plac_summary, by=c("SubjectID","Assay"))

summary_raw3$cele_auc24_percent=summary_raw3$cele_auc24*100/summary_raw3$plac_auc24
summary_raw3$cele_auc12_percent=summary_raw3$cele_auc12*100/summary_raw3$plac_auc12
summary_raw3$nap_auc24_percent=summary_raw3$nap_auc24*100/summary_raw3$plac_auc24
summary_raw3$nap_auc12_percent=summary_raw3$nap_auc12*100/summary_raw3$plac_auc12

summary_norm3$cele_auc24_percent=summary_norm3$cele_auc24*100/summary_norm3$plac_auc24
summary_norm3$cele_auc12_percent=summary_norm3$cele_auc12*100/summary_norm3$plac_auc12
summary_norm3$nap_auc24_percent=summary_norm3$nap_auc24*100/summary_norm3$plac_auc24
summary_norm3$nap_auc12_percent=summary_norm3$nap_auc12*100/summary_norm3$plac_auc12

detach(package:plyr)
summary_raw3%>% group_by(Assay) %>% summarize(cele12_median=median(cele_auc12_percent),cele24_median=median(cele_auc24_percent),nap12_median=median(nap_auc12_percent),nap24_median=median(nap_auc24_percent))
summary_norm3%>% group_by(Assay) %>% summarize(cele12_median=median(cele_auc12_percent),cele24_median=median(cele_auc24_percent),nap12_median=median(nap_auc12_percent),nap24_median=median(nap_auc24_percent))

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

data4$Interval=as.factor(ifelse(data4$time=="168.5", NA,ifelse(data4$time=="168", 0,cut(data4$time,breaks=seq(168,193,by=1),labels=0:24))))
data_COX2=subset(data4,data4$Assay=="PGE2",select=c("SubjectID","Treatment","value","norm_plac","norm_Day1","Interval"))
data_COX2=rename(data_COX2,c("value"="PGE2","norm_plac"="PGE2_norm_plac","norm_Day1"="PGE2_norm_Day1"))
data_COX2_24=subset(data_COX2, is.na(data_COX2$Interval)=="FALSE")

data_COX1=subset(data4,data4$Assay=="TxB2",select=c("SubjectID","Treatment","value","norm_plac","norm_Day1","Interval"))
data_COX1=rename(data_COX1,c("value"="TxB2","norm_plac"="TxB2_norm_plac","norm_Day1"="TxB2_norm_Day1"))
data_COX1_24=subset(data_COX1, is.na(data_COX1$Interval)=="FALSE")

data_COX=merge(data_COX1_24,data_COX2_24, by=c("SubjectID","Treatment","Interval"), all=TRUE)

data_ABPM_COX=merge(data_ABPM2,data_COX,by=c("SubjectID","Treatment","Interval"), all=TRUE)

library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),
 axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))

ggplot(data_ABPM_COX,aes(PGE2_norm_plac,Systolic_mean,color=Treatment))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", alpha=0)+facet_grid(.~SubjectID)+scale_x_log10()

cor(data_ABPM_COX$PGE2_norm_plac,data_ABPM_COX$MAP_mean, use="complete.obs", method="spearman")

pdf("Clinical_Study_Preliminary_Analysis.pdf")
ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,norm_plac,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-1 Activity",x="Hours",y="Serum TxB2\n (%activity relative to placebo)")
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,norm_plac,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-2 Activity",x="Hours",y="Plasma PGE2\n (%activity relative to placebo)")

ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,norm_Day1,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-1 Activity",x="Hours",y="Serum TxB2\n (%activity relative to baseline)")
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,norm_Day1,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-2 Activity",x="Hours",y="Plasma PGE2\n (%activity relative to baseline)")

ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,value,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,750)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+labs(title="COX-1 Activity",x="Hours",y="Serum TxB2 (ng/ml)")
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,value,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,55)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+labs(title="COX-2 Activity",x="Hours",y="Plasma PGE2 (ng/ml)")

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
