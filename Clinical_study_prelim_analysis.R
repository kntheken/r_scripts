##Read in data##
data=read.csv("COX_assay_results8162017.csv")
data_ABPM=read.csv("ABPM_data_8232017.csv")

data_ABPM$Time2=strptime(data_ABPM$Time,"%H:%M:%S")
data_ABPM$Base_Time=strptime("08:00:00", "%H:%M:%S")
data_ABPM$Hours=as.numeric(difftime(data_ABPM$Time2,data_ABPM$Base_Time, units ="hours")+data_ABPM$Day*24)

data_norm=data[c("SubjectID","Assay", "Treatment")]
data_norm$Day1=data$Day1*100/data$Day1
data_norm$Day7_T0=data$Day7_T0*100/data$Day1
data_norm$Day7_T0.5=data$Day7_T0.5*100/data$Day1
data_norm$Day7_T1=data$Day7_T1*100/data$Day1
data_norm$Day7_T2=data$Day7_T2*100/data$Day1
data_norm$Day7_T4=data$Day7_T4*100/data$Day1
data_norm$Day7_T8=data$Day7_T8*100/data$Day1
data_norm$Day7_T12=data$Day7_T12*100/data$Day1
data_norm$Day8=data$Day8*100/data$Day1
data_norm$Day9=data$Day9*100/data$Day1
data_norm$Day10=data$Day10*100/data$Day1

library(reshape)
data2=melt(data,id.vars=c("SubjectID","Assay", "Treatment"))
data_norm2=melt(data_norm,id.vars=c("SubjectID","Assay", "Treatment"))

##Add time in hours##
data2$time=ifelse(data2$variable=="Day1",0,ifelse(data2$variable=="Day7_T0",168,ifelse(data2$variable=="Day7_T0.5",168.5,ifelse(data2$variable=="Day7_T1",169,ifelse(data2$variable=="Day7_T2",170,ifelse(data2$variable=="Day7_T4",172,ifelse(data2$variable=="Day7_T8",176,ifelse(data2$variable=="Day7_T12",180,ifelse(data2$variable=="Day8",192,ifelse(data2$variable=="Day9",216,240))))))))))
data_norm2$time=ifelse(data_norm2$variable=="Day1",0,ifelse(data_norm2$variable=="Day7_T0",168,ifelse(data_norm2$variable=="Day7_T0.5",168.5,ifelse(data_norm2$variable=="Day7_T1",169,ifelse(data_norm2$variable=="Day7_T2",170,ifelse(data_norm2$variable=="Day7_T4",172,ifelse(data_norm2$variable=="Day7_T8",176,ifelse(data_norm2$variable=="Day7_T12",180,ifelse(data_norm2$variable=="Day8",192,ifelse(data_norm2$variable=="Day9",216,240))))))))))


library(MESS)
library(dplyr)


mean_raw=data2 %>% group_by(SubjectID, Assay, Treatment) %>% summarize (average=mean(value, na.rm=TRUE))
placebo_mean=subset(mean_raw,Treatment=="Placebo",select=-Treatment) 

baseline=subset(data2,variable=="Day1", select=c("SubjectID","Assay","Treatment","value"))

library(plyr)
baseline=rename(baseline, c("value"="Day1")
data3=merge(data2,placebo_mean, by=c("SubjectID","Assay"))
data4=merge(data3,baseline, by=c("SubjectID","Assay","Treatment"))
data4$norm_plac=data4$value*100/data4$average
data4$norm_Day1=data4$value*100/data4$Day1


summary_all=data4 %>% group_by(SubjectID,Assay,Treatment) %>% summarize (auc24=auc(time,value, from=168, to=192),auc12=auc(time,value, from=168, to=180),tmax=time[which.min(value)], auc24_norm_plac=auc(time,norm_plac, from=168, to=192),auc12_norm_plac=auc(time,norm_plac, from=168, to=180),tmax_norm_plac=time[which.min(norm_plac)],auc24_norm_Day1=auc(time,norm_Day1, from=168, to=192),auc12_norm_Day1=auc(time,norm_Day1, from=168, to=180),tmax_norm_Day1=time[which.min(norm_Day1)])

cele_summary=subset(summary_all,Treatment=="Celecoxib", select=-Treatment]
cele_raw=rename(cele_summary,c("auc24"="cele_auc24","auc12"="cele_auc12","tmax"="cele_tmax","auc24_norm_plac"="cele_auc24_norm_plac","auc12_norm_plac"="cele_auc12_norm_plac","tmax_norm_plac"="cele_tmax_norm_plac","auc24_norm_Day1"="cele_auc24_norm_Day1","auc12_norm_Day1"="cele_auc12_norm_Day1","tmax_norm_Day1"="cele_tmax_norm_Day1"))

nap_summary=subset(summary_all,Treatment=="Naproxen", select=-Treatment]
nap_summary=rename(nap_summary,c("auc24"="nap_auc24","auc12"="nap_auc12","tmax"="nap_tmax","auc24_norm_plac"="nap_auc24_norm_plac","auc12_norm_plac"="nap_auc12_norm_plac","tmax_norm_plac"="nap_tmax_norm_plac","auc24_norm_Day1"="nap_auc24_norm_Day1","auc12_norm_Day1"="nap_auc12_norm_Day1","tmax_norm_Day1"="nap_tmax_norm_Day1"))

plac_summary=subset(summary_all,Treatment=="Placebo", select=-Treatment]
plac_summary=rename(plac_summary,c("auc24"="plac_auc24","auc12"="plac_auc12","tmax"="plac_tmax","auc24_norm_plac"="plac_auc24_norm_plac","auc12_norm_plac"="plac_auc12_norm_plac","tmax_norm_plac"="plac_tmax_norm_plac","auc24_norm_Day1"="plac_auc24_norm_Day1","auc12_norm_Day1"="plac_auc12_norm_Day1","tmax_norm_Day1"="plac_tmax_norm_Day1"))



data_norm_COX2=data_norm[which(data_norm$Assay=="PGE2"),]
data_norm_COX1=data_norm[which(data_norm$Assay=="TxB2"),]
data_selectivity=data_norm_COX2[c("SubjectID","Treatment")]
data_selectivity$Day1=data_norm_COX2$Day1/data_norm_COX1$Day1
data_selectivity$Day7_T0=(100-data_norm_COX2$Day7_T0)/(100-data_norm_COX1$Day7_T0)
data_selectivity$Day7_T0.5=(100-data_norm_COX2$Day7_T0.5)/(100-data_norm_COX1$Day7_T0.5)
data_selectivity$Day7_T1=(100-data_norm_COX2$Day7_T1)/(100-data_norm_COX1$Day7_T1)
data_selectivity$Day7_T2=(100-data_norm_COX2$Day7_T2)/(100-data_norm_COX1$Day7_T2)
data_selectivity$Day7_T4=(100-data_norm_COX2$Day7_T4)/(100-data_norm_COX1$Day7_T4)
data_selectivity$Day7_T8=(100-data_norm_COX2$Day7_T8)/(100-data_norm_COX1$Day7_T8)
data_selectivity$Day7_T12=(100-data_norm_COX2$Day7_T12)/(100-data_norm_COX1$Day7_T12)
data_selectivity$Day8=(100-data_norm_COX2$Day8)/(100-data_norm_COX1$Day8)
data_selectivity$Day9=(100-data_norm_COX2$Day9)/(100-data_norm_COX1$Day9)
data_selectivity$Day10=(100-data_norm_COX2$Day10)/(100-data_norm_COX1$Day10)

data_selectivity2=melt(data_selectivity,id.vars=c("SubjectID", "Treatment"))
data_selectivity2$time=ifelse(data_selectivity2$variable=="Day1",0,ifelse(data_selectivity2$variable=="Day7_T0",168,ifelse(data_selectivity2$variable=="Day7_T0.5",168.5,ifelse(data_selectivity2$variable=="Day7_T1",169,ifelse(data_selectivity2$variable=="Day7_T2",170,ifelse(data_selectivity2$variable=="Day7_T4",172,ifelse(data_selectivity2$variable=="Day7_T8",176,ifelse(data_selectivity2$variable=="Day7_T12",180,ifelse(data_selectivity2$variable=="Day8",192,ifelse(data_selectivity2$variable=="Day9",216,240))))))))))



summary_raw2=merge(cele_raw,nap_raw,by=c("SubjectID","Assay"),all=TRUE)
summary_raw3=merge(summary_raw2,plac_raw,by=c("SubjectID","Assay"),all=TRUE)

summary_norm2=merge(cele_norm,nap_norm,by=c("SubjectID","Assay"),all=TRUE)
summary_norm3=merge(summary_norm2,plac_norm,by=c("SubjectID","Assay"),all=TRUE)

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

data_BP=data_ABPM[which(data_ABPM$Measurement=="Diastolic" |data_ABPM$Measurement=="Systolic" |data_ABPM$Measurement=="MAP" ),]
data_BP=data_BP[c("SubjectID","Treatment","Hours","Measurement","Value")]
data_BP$Interval=cut(data_BP$Hours,breaks=seq(167.5,191.5,by=1),labels=0:23)
data_PG_norm=rename(data_norm2,c("Assay"="Measurement", "time"="Hours", "value"="Value"))
data_PG_norm=data_PG_norm[c("SubjectID","Treatment","Hours","Measurement","Value")]
data_all=rbind(data_BP,data_PG_norm)
data_all$Assay1=as.factor(ifelse(data_all$Measurement=="Diastolic"|data_all$Measurement=="Systolic"|data_all$Measurement=="MAP","BP","PG"))
data_all$Assay2=as.factor(ifelse(data_all$Assay1=="BP","BP",as.character(data_all$Measurement)))

library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),
 axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))

ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,norm_plac,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,norm_plac,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)

ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,norm_Day1,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,norm_Day1,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)

ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,value,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,750)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,value,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+xlim(168,192)+ylim(0,55)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")



ggplot(summary_raw3, aes(Assay,cele_auc12_percent, color=as.factor(SubjectID)))+geom_point()+pub_specs+scale_y_log10()
summary(summary_raw3)

ggplot(data_all[which(data_all$SubjectID=="1" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 1")+geom_rect(xmin=182, xmax=190, ymin=0,ymax=350, color="gray",alpha=0.005)
ggplot(data_all[which(data_all$SubjectID=="2" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 2")
ggplot(data_all[which(data_all$SubjectID=="27" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 27")
ggplot(data_all[which(data_all$SubjectID=="30" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 30")
ggplot(data_all[which(data_all$SubjectID=="36" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 36")
ggplot(data_all[which(data_all$SubjectID=="69" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 69")
ggplot(data_all[which(data_all$SubjectID=="72" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 72")
ggplot(data_all[which(data_all$SubjectID=="78" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 78")
ggplot(data_all[which(data_all$SubjectID=="95" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 95")
ggplot(data_all[which(data_all$SubjectID=="96" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+ylim(0,500)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 96")
ggplot(data_all[which(data_all$SubjectID=="127" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 127")
ggplot(data_all[which(data_all$SubjectID=="145" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 145")
ggplot(data_all[which(data_all$SubjectID=="181" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 181")
ggplot(data_all[which(data_all$SubjectID=="205" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 205")
ggplot(data_all[which(data_all$SubjectID=="265" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(.~Treatment)+theme(legend.position="bottom")+labs(title="Subject 265")

ggplot(data_all[which(data_all$SubjectID=="1" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 1")
ggplot(data_all[which(data_all$SubjectID=="2" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 2")
ggplot(data_all[which(data_all$SubjectID=="27" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 27")
ggplot(data_all[which(data_all$SubjectID=="30" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 30")
ggplot(data_all[which(data_all$SubjectID=="36" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 36")
ggplot(data_all[which(data_all$SubjectID=="69" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 69")
ggplot(data_all[which(data_all$SubjectID=="72" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 72")
ggplot(data_all[which(data_all$SubjectID=="78" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 78")
ggplot(data_all[which(data_all$SubjectID=="95" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 95")
ggplot(data_all[which(data_all$SubjectID=="96" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+ylim(0,350)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 96")
ggplot(data_all[which(data_all$SubjectID=="127" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 127")
ggplot(data_all[which(data_all$SubjectID=="145" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 145")
ggplot(data_all[which(data_all$SubjectID=="181" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 181")
ggplot(data_all[which(data_all$SubjectID=="205" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 205")
ggplot(data_all[which(data_all$SubjectID=="265" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay1~Treatment)+theme(legend.position="bottom")+labs(title="Subject 265")

ggplot(data_all[which(data_all$SubjectID=="1" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title=d"Subject 1")
ggplot(data_all[which(data_all$SubjectID=="2" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 2")
ggplot(data_all[which(data_all$SubjectID=="27" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 27")
ggplot(data_all[which(data_all$SubjectID=="30" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 30")
ggplot(data_all[which(data_all$SubjectID=="36" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 36")
ggplot(data_all[which(data_all$SubjectID=="69" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 69")
ggplot(data_all[which(data_all$SubjectID=="72" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 72")
ggplot(data_all[which(data_all$SubjectID=="78" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 78")
ggplot(data_all[which(data_all$SubjectID=="95" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 95")
ggplot(data_all[which(data_all$SubjectID=="96" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+ylim(0,350)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 96")
ggplot(data_all[which(data_all$SubjectID=="127" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 127")
ggplot(data_all[which(data_all$SubjectID=="145" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 145")
ggplot(data_all[which(data_all$SubjectID=="181" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 181")
ggplot(data_all[which(data_all$SubjectID=="205" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 205")
ggplot(data_all[which(data_all$SubjectID=="265" ),],aes(Hours,Value,color=Measurement))+geom_line()+pub_specs +xlim(167,192)+facet_grid(Assay2~Treatment)+theme(legend.position="bottom")+labs(title="Subject 265")
