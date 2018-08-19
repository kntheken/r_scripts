##Read in data##
data_wide=read.csv("Dental_master_wide.csv")
data_long=read.csv("Dental_master_long.csv")
pre=read.csv("Pre_rCBF.csv")
post=read.csv("Post_rCBF.csv")
post1=read.csv("Post1_rCBF.csv")
post2=read.csv("Post2_rCBF.csv")
post3=read.csv("Post3_rCBF.csv")
post4=read.csv("Post4_rCBF.csv")
data_luminex=read.csv("Dental_luminex.csv")


##Change Values to numeric and Dates to Date format##
data_wide$Ibuprofen.basal=as.numeric(levels(data_wide$Ibuprofen.basal))[data_wide$Ibuprofen.basal]
data_wide$Ibuprofen.PS1=as.numeric(levels(data_wide$Ibuprofen.PS1))[data_wide$Ibuprofen.PS1]
data_wide$Ibuprofen.PS2=as.numeric(levels(data_wide$Ibuprofen.PS2))[data_wide$Ibuprofen.PS2]
data_wide$APAP.basal=as.numeric(levels(data_wide$APAP.basal))[data_wide$APAP.basal]
data_wide$APAP.PS1=as.numeric(levels(data_wide$APAP.PS1))[data_wide$APAP.PS1]
data_wide$APAP.PS2=as.numeric(levels(data_wide$APAP.PS2))[data_wide$APAP.PS2]
data_wide$COX.1.PS1=as.numeric(levels(data_wide$COX.1.PS1))[data_wide$COX.1.PS1]
data_wide$Date=as.Date(data_wide$Date, format="%m/%d/%Y")
data_wide$Demo_LMP=as.Date(data_wide$Demo_LMP, format="%m/%d/%Y")
data_wide$Demo_Gender=as.factor(data_wide$Demo_Gender)
data_wide$Weight=data_wide$Demo_Wt/2.2
data_wide$Height=data_wide$Height_Inch*0.0254
data_wide$Discharge.Time=strptime(data_wide$Discharge.Time,format="%H:%M")
data_wide$Drug.Time=strptime(data_wide$Drug.Time,format="%H:%M")
data_wide$DOS_Surgery_Start=strptime(data_wide$DOS_Surgery_Start,format="%H:%M")
data_wide$DOS_Surgery_Stop=strptime(data_wide$DOS_Surgery_Stop,format="%H:%M")
data_wide$Basal.blood.time=strptime(data_wide$Basal.blood.time,format="%H:%M")
data_wide$Basal.urine.time=strptime(data_wide$Basal.urine.time,format="%H:%M")
data_wide$PS1.blood.time=strptime(data_wide$PS1.blood.time,format="%H:%M")
data_wide$PS1.urine.time=strptime(data_wide$PS1.urine.time,format="%H:%M")
data_wide$PS2.blood.time=strptime(data_wide$PS2.blood.time,format="%H:%M")
data_wide$PS2.urine.time=strptime(data_wide$PS2.urine.time,format="%H:%M")
data_wide$Rescue.Time1=strptime(data_wide$Rescue.Time1,format="%H:%M")
data_wide$Surgery.basal.time=difftime(data_wide$Basal.blood.time,data_wide$DOS_Surgery_Stop, units="hours")
data_wide$Surgery.PS1.time=difftime(data_wide$PS1.blood.time,data_wide$DOS_Surgery_Stop, units="hours")
data_wide$Surgery.PS2.time=difftime(data_wide$PS2.blood.time,data_wide$DOS_Surgery_Stop, units="hours")

##Create Responder Status variable##
data_wide$Status=ifelse(data_wide$Group=="Ibuprofen"& data_wide$Rescue=="N","Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$Rescue=="Y","Non-Responder","Placebo"))
data_wide$Status2=factor(data_wide$Status, levels=c("Placebo", "Non-Responder", "Responder"))
data_long$Status=ifelse(data_long$Group=="Ibuprofen"& data_long$Rescue=="N","Responder",ifelse(data_long$Group=="Ibuprofen"& data_long$Rescue=="Y","Non-Responder","Placebo"))
data_long$Status2=factor(data_long$Status, levels=c("Placebo", "Non-Responder", "Responder"))

##Create Percent Baseline variables##
data_wide$PTGS1.PS1.percent=(data_wide$PTGS1.PS1*100)/data_wide$PTGS1.basal
data_wide$PTGS2.PS1.percent=(data_wide$PTGS2.PS1*100)/data_wide$PTGS2.basal
data_wide$PACER.PS1.percent=(data_wide$PACER.PS1*100)/data_wide$PACER.basal

data_wide$PTGS1.PS2.percent=(data_wide$PTGS1.PS2*100)/data_wide$PTGS1.basal
data_wide$PTGS2.PS2.percent=(data_wide$PTGS2.PS2*100)/data_wide$PTGS2.basal
data_wide$PACER.PS2.percent=(data_wide$PACER.PS2*100)/data_wide$PACER.basal

data_wide$COX.1.PS1.percent=(data_wide$COX.1.PS1*100)/data_wide$COX.1.basal
data_wide$COX.1.PS2.percent=(data_wide$COX.1.PS2*100)/data_wide$COX.1.basal

data_wide$COX.2.PS1.percent=(data_wide$COX.2.PS1*100)/data_wide$COX.2.basal
data_wide$COX.2.PS2.percent=(data_wide$COX.2.PS2*100)/data_wide$COX.2.basal

data_wide$PGEM.PS1.percent=(data_wide$PGEM.PS1*100)/data_wide$PGEM.basal
data_wide$PGEM.PS2.percent=(data_wide$PGEM.PS2*100)/data_wide$PGEM.basal

data_wide$PGDM.PS1.percent=(data_wide$PGDM.PS1*100)/data_wide$PGDM.basal
data_wide$PGDM.PS2.percent=(data_wide$PGDM.PS2*100)/data_wide$PGDM.basal

data_wide$PGIM.PS1.percent=(data_wide$PGIM.PS1*100)/data_wide$PGIM.basal
data_wide$PGIM.PS2.percent=(data_wide$PGIM.PS2*100)/data_wide$PGIM.basal

data_wide$TxM.PS1.percent=(data_wide$TxM.PS1*100)/data_wide$TxM.basal
data_wide$TxM.PS2.percent=(data_wide$TxM.PS2*100)/data_wide$TxM.basal

data_luminex$IL10.PS1.percent=(data_luminex$IL10.PS1*100)/data_luminex$IL10.basal
data_luminex$IL10.PS2.percent=(data_luminex$IL10.PS2*100)/data_luminex$IL10.basal

data_luminex$IL1b.PS1.percent=(data_luminex$IL1b.PS1*100)/data_luminex$IL1b.basal
data_luminex$IL1b.PS2.percent=(data_luminex$IL1b.PS2*100)/data_luminex$IL1b.basal

data_luminex$IL6.PS1.percent=(data_luminex$IL6.PS1*100)/data_luminex$IL6.basal
data_luminex$IL6.PS2.percent=(data_luminex$IL6.PS2*100)/data_luminex$IL6.basal

data_luminex$IL8.PS1.percent=(data_luminex$IL8.PS1*100)/data_luminex$IL8.basal
data_luminex$IL8.PS2.percent=(data_luminex$IL8.PS2*100)/data_luminex$IL8.basal

data_luminex$MCP1.PS1.percent=(data_luminex$MCP1.PS1*100)/data_luminex$MCP1.basal
data_luminex$MCP1.PS2.percent=(data_luminex$MCP1.PS2*100)/data_luminex$MCP1.basal

data_luminex$TNFa.PS1.percent=(data_luminex$TNFa.PS1*100)/data_luminex$TNFa.basal
data_luminex$TNFa.PS2.percent=(data_luminex$TNFa.PS2*100)/data_luminex$TNFa.basal

data_wide$PS1.pain.percent=(data_wide$PS1.Pain*100)/data_wide$End1.pain.score
data_wide$PS2.pain.percent=(data_wide$PS2.Pain*100)/data_wide$End1.pain.score

##Making basal,PS1, and PS2 data sets##
basal=data_wide[c("Subject","Demo_Gender","Age","Status2","DOS_Trauma_SUM","Drug.Time","Surgery.basal.time","Ibuprofen.basal","APAP.basal","COX.1.basal","COX.2.basal","PGEM.basal","PGDM.basal","PGIM.basal","TxM.basal","PTGS1.basal","PTGS2.basal","PACER.basal")]
basal$COX.1.percent=100
basal$COX.2.percent=100
basal$PGEM.percent=100
basal$PGDM.percent=100
basal$PGIM.percent=100
basal$TxM.percent=100
basal$PTGS1.percent=100
basal$PTGS2.percent=100
basal$PACER.percent=100
basal$Event="Baseline"
PS1=data_wide[c("Subject","Demo_Gender","Age","Status2","DOS_Trauma_SUM","Drug.Time","Surgery.PS1.time","Ibuprofen.PS1","APAP.PS1","COX.1.PS1","COX.2.PS1","PGEM.PS1","PGDM.PS1","PGIM.PS1","TxM.PS1","COX.1.PS1.percent","COX.2.PS1.percent","PGEM.PS1.percent","PGDM.PS1.percent","PGIM.PS1.percent","TxM.PS1.percent","PTGS1.PS1","PTGS2.PS1","PACER.PS1","PTGS1.PS1.percent","PTGS2.PS1.percent","PACER.PS1.percent")]
PS1$Event="Post-surgery 1"
PS2=data_wide[c("Subject","Demo_Gender","Age","Status2","DOS_Trauma_SUM","Drug.Time","Surgery.PS2.time","Ibuprofen.PS2","APAP.PS2","COX.1.PS2","COX.2.PS2","PGEM.PS2","PGDM.PS2","PGIM.PS2","TxM.PS2","COX.1.PS2.percent","COX.2.PS2.percent","PGEM.PS2.percent","PGDM.PS2.percent","PGIM.PS2.percent","TxM.PS2.percent","PTGS1.PS2","PTGS2.PS2","PACER.PS2","PTGS1.PS2.percent","PTGS2.PS2.percent","PACER.PS2.percent")]
PS2$Event="Post-surgery 2"

luminex_basal=data_luminex[c("Subject","IL10.basal","IL1b.basal","IL6.basal","IL8.basal","MCP1.basal","TNFa.basal")]
luminex_basal$IL10.basal.percent=100
luminex_basal$IL1b.basal.percent=100
luminex_basal$IL6.basal.percent=100
luminex_basal$IL8.basal.percent=100
luminex_basal$MCP1.basal.percent=100
luminex_basal$TNFa.basal.percent=100
luminex_basal$Event="Baseline"
luminex_PS1=data_luminex[c("Subject","IL10.PS1","IL1b.PS1","IL6.PS1","IL8.PS1","MCP1.PS1","TNFa.PS1","IL10.PS1.percent","IL1b.PS1.percent","IL6.PS1.percent","IL8.PS1.percent","MCP1.PS1.percent","TNFa.PS1.percent")]
luminex_PS1$Event="Post-surgery 1"
luminex_PS2=data_luminex[c("Subject","IL10.PS2","IL1b.PS2","IL6.PS2","IL8.PS2","MCP1.PS2","TNFa.PS2","IL10.PS2.percent","IL1b.PS2.percent","IL6.PS2.percent","IL8.PS2.percent","MCP1.PS2.percent","TNFa.PS2.percent")]
luminex_PS2$Event="Post-surgery 2"

detach(package:dplyr)
library(plyr)
basal=rename(basal, c("Surgery.basal.time"="Time","Ibuprofen.basal"="Ibuprofen","APAP.basal"="APAP","COX.1.basal"="COX.1","COX.2.basal"="COX.2","PGEM.basal"="PGEM","PGDM.basal"="PGDM","PGIM.basal"="PGIM","TxM.basal"="TxM","PTGS1.basal"="PTGS1","PTGS2.basal"="PTGS2","PACER.basal"="PACER"))
PS1=rename(PS1,c("Surgery.PS1.time"="Time","Ibuprofen.PS1"="Ibuprofen","APAP.PS1"="APAP","COX.1.PS1"="COX.1","COX.2.PS1"="COX.2","PGEM.PS1"="PGEM","PGDM.PS1"="PGDM","PGIM.PS1"="PGIM","TxM.PS1"="TxM","COX.1.PS1.percent"="COX.1.percent","COX.2.PS1.percent"="COX.2.percent","PGEM.PS1.percent"="PGEM.percent","PGDM.PS1.percent"="PGDM.percent","PGIM.PS1.percent"="PGIM.percent","TxM.PS1.percent"="TxM.percent","PTGS1.PS1"="PTGS1","PTGS2.PS1"="PTGS2","PACER.PS1"="PACER","PTGS1.PS1.percent"="PTGS1.percent","PTGS2.PS1.percent"="PTGS2.percent","PACER.PS1.percent"="PACER.percent"))
PS2=rename(PS2,c("Surgery.PS2.time"="Time","Ibuprofen.PS2"="Ibuprofen","APAP.PS2"="APAP","COX.1.PS2"="COX.1","COX.2.PS2"="COX.2","PGEM.PS2"="PGEM","PGDM.PS2"="PGDM","PGIM.PS2"="PGIM","TxM.PS2"="TxM","COX.1.PS2.percent"="COX.1.percent","COX.2.PS2.percent"="COX.2.percent","PGEM.PS2.percent"="PGEM.percent","PGDM.PS2.percent"="PGDM.percent","PGIM.PS2.percent"="PGIM.percent","TxM.PS2.percent"="TxM.percent","PTGS1.PS2"="PTGS1","PTGS2.PS2"="PTGS2","PACER.PS2"="PACER","PTGS1.PS2.percent"="PTGS1.percent","PTGS2.PS2.percent"="PTGS2.percent","PACER.PS2.percent"="PACER.percent"))

luminex_basal=rename(luminex_basal, c("IL10.basal"="IL10","IL1b.basal"="IL1b","IL6.basal"="IL6","IL8.basal"="IL8","MCP1.basal"="MCP1","TNFa.basal"="TNFa","IL10.basal.percent"="IL10.percent","IL1b.basal.percent"="IL1b.percent","IL6.basal.percent"="IL6.percent","IL8.basal.percent"="IL8.percent","MCP1.basal.percent"="MCP1.percent","TNFa.basal.percent"="TNFa.percent"))
luminex_PS1=rename(luminex_PS1,c("IL10.PS1"="IL10","IL1b.PS1"="IL1b","IL6.PS1"="IL6","IL8.PS1"="IL8","MCP1.PS1"="MCP1","TNFa.PS1"="TNFa","IL10.PS1.percent"="IL10.percent","IL1b.PS1.percent"="IL1b.percent","IL6.PS1.percent"="IL6.percent","IL8.PS1.percent"="IL8.percent","MCP1.PS1.percent"="MCP1.percent","TNFa.PS1.percent"="TNFa.percent"))
luminex_PS2=rename(luminex_PS2,c("IL10.PS2"="IL10","IL1b.PS2"="IL1b","IL6.PS2"="IL6","IL8.PS2"="IL8","MCP1.PS2"="MCP1","TNFa.PS2"="TNFa","IL10.PS2.percent"="IL10.percent","IL1b.PS2.percent"="IL1b.percent","IL6.PS2.percent"="IL6.percent","IL8.PS2.percent"="IL8.percent","MCP1.PS2.percent"="MCP1.percent","TNFa.PS2.percent"="TNFa.percent"))

lumbasal.PS1=rbind(luminex_basal,luminex_PS1)
luminex=rbind(lumbasal.PS1,luminex_PS2)

basal.PS1=rbind(basal,PS1)
measurement=rbind(basal.PS1,PS2)


luminex.all=merge(luminex,measurement, by=c("Subject","Event"))

baseline=data_wide[c("Subject","Age","Weight","Height","Demo_Gender","Status2","Group","Number_Teeth","DOS_Trauma_SUM","Length_Surgery","End1.pain.score","Pain.relief.score","Ibuprofen.basal","APAP.basal","COX.1.basal","COX.2.basal","PGEM.basal","PGDM.basal","PGIM.basal","TxM.basal","PTGS1.basal","PTGS2.basal","PACER.basal","deltaTdrug.PS1blood","EvalPain_Relief_Rate","Ibuprofen.PS1","APAP.PS1","COX.1.PS1","COX.2.PS1","PGEM.PS1","PGDM.PS1","PGIM.PS1","TxM.PS1","COX.1.PS1.percent","COX.2.PS1.percent","PGEM.PS1.percent","PGDM.PS1.percent","PGIM.PS1.percent","TxM.PS1.percent","PTGS1.PS1","PTGS2.PS1","PACER.PS1","PTGS1.PS1.percent","PTGS2.PS1.percent","PACER.PS1.percent","Ibuprofen.PS2","APAP.PS2","COX.1.PS2","COX.2.PS2","PGEM.PS2","PGDM.PS2","PGIM.PS2","TxM.PS2","COX.1.PS2.percent","COX.2.PS2.percent","PGEM.PS2.percent","PGDM.PS2.percent","PGIM.PS2.percent","TxM.PS2.percent","PTGS1.PS2","PTGS2.PS2","PACER.PS2","PTGS1.PS2.percent","PTGS2.PS2.percent","PACER.PS2.percent")]

all_bio=merge(baseline,data_luminex, by="Subject")
all_bio.i=all_bio[which(all_bio$Status2!="Placebo"),]

##Calculating end time in hours for survival curve##
data_wide$End.Time=ifelse(data_wide$Rescue=="Y",(data_wide$Rescue.Time1-data_wide$Drug.Time)/60,data_wide$Discharge.Time-data_wide$Drug.Time)

data_wide$End.Time2=ifelse(data_wide$Rescue=="Y",(data_wide$Rescue.Time1-data_wide$Drug.Time)/60,4)




##Merging rCBF vertically##
prepost=rbind(pre,post)
post12=rbind(post1,post2)
post34=rbind(post4,post4)
post14=rbind(post12,post34)
rCBF_all=rbind(prepost,post14)
rCBF_all$Time=strptime(rCBF_all$Time,format="%H:%M")
rCBF_all$Drug.Time=strptime(rCBF_all$Drug.Time,format="%H:%M")
response=data_wide[c("Subject","Status2")]
rCBF_all_response=merge(rCBF_all,response, by="Subject")
rCBF_all_response$dTime=(rCBF_all_response$Time-rCBF_all_response$Drug.Time)

##Reshape and merge rCBF data horizontally##
library(reshape2)
pre_melt=melt(pre,id.vars="Subject",variable.name="Region",value.name="Pre")
post_melt=melt(post,id.vars="Subject",variable.name="Region",value.name="Post")
post1_melt=melt(post1,id.vars="Subject",variable.name="Region",value.name="Post1")
post2_melt=melt(post2,id.vars="Subject",variable.name="Region",value.name="Post2")
post3_melt=melt(post3,id.vars="Subject",variable.name="Region",value.name="Post3")
post4_melt=melt(post4,id.vars="Subject",variable.name="Region",value.name="Post4")

post01=merge(post_melt,post1_melt,by=c("Subject","Region"))
post23=merge(post2_melt,post3_melt,by=c("Subject","Region"))
post03=merge(post01,post23,by=c("Subject","Region"))
post04=merge(post03,post4_melt,by=c("Subject","Region"))
rCBF=merge(post04,pre_melt,by=c("Subject","Region"))

##Create responder status by pain relief descriptors##
data_wide$Status_PRR50=factor(ifelse(data_wide$Group=="Ibuprofen"& data_wide$EvalPain_Relief_Rate>=50,"Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$EvalPain_Relief_Rate<50,"Non-Responder","Placebo")),levels=c("Placebo", "Non-Responder", "Responder"))
data_wide$Status_PRR70=factor(ifelse(data_wide$Group=="Ibuprofen"& data_wide$EvalPain_Relief_Rate>70,"Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$EvalPain_Relief_Rate<=70,"Non-Responder","Placebo")),levels=c("Placebo", "Non-Responder", "Responder"))
data_wide$Status_PID100=factor(ifelse(data_wide$Group=="Ibuprofen"& data_wide$Study.Perc.PID==100,"Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$Study.Perc.PID<100,"Non-Responder","Placebo")),levels=c("Placebo", "Non-Responder", "Responder"))
data_wide$Status_PID70=factor(ifelse(data_wide$Group=="Ibuprofen"& data_wide$Study.Perc.PID>=70,"Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$Study.Perc.PID<70,"Non-Responder","Placebo")),levels=c("Placebo", "Non-Responder", "Responder"))
data_wide$Status_PID50=factor(ifelse(data_wide$Group=="Ibuprofen"& data_wide$Study.Perc.PID>=50,"Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$Study.Perc.PID<50,"Non-Responder","Placebo")),levels=c("Placebo", "Non-Responder", "Responder"))
data_wide$Status_PGIC3=factor(ifelse(data_wide$Group=="Ibuprofen"& data_wide$Max_PGIC_Before_Rescue>=3,"Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$Max_PGIC_Before_Rescue<3,"Non-Responder","Placebo")),levels=c("Placebo", "Non-Responder", "Responder"))
data_wide$Status_PGIC2=factor(ifelse(data_wide$Group=="Ibuprofen"& data_wide$Max_PGIC_Before_Rescue>=2,"Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$Max_PGIC_Before_Rescue<2,"Non-Responder","Placebo")),levels=c("Placebo", "Non-Responder", "Responder"))
data_wide$Status_Rdesc34=factor(ifelse(data_wide$Group=="Ibuprofen"& data_wide$EvalPain_Relief_Desc>=3,"Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$EvalPain_Relief_Desc<3,"Non-Responder","Placebo")),levels=c("Placebo", "Non-Responder", "Responder"))
data_wide$Status_Rdesc24=factor(ifelse(data_wide$Group=="Ibuprofen"& data_wide$EvalPain_Relief_Desc>=2,"Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$EvalPain_Relief_Desc<2,"Non-Responder","Placebo")),levels=c("Placebo", "Non-Responder", "Responder"))

data_wide$PRR70_score=ifelse(data_wide$Status_PRR70=="Responder",1,0)
data_wide$PID70_score=ifelse(data_wide$Status_PID70=="Responder",1,0)
data_wide$PGIC3_score=ifelse(data_wide$Status_PGIC3=="Responder",1,0)
data_wide$Rdesc_score=ifelse(data_wide$Status_Rdesc34=="Responder",1,0)
data_wide$Rescue_score=ifelse(data_wide$Status2=="Responder",1,0)

data_wide$Resp_score=(data_wide$PRR70_score+data_wide$PID70_score+data_wide$PGIC3_score+data_wide$Rdesc_score+data_wide$Rescue_score)

data_wide$Status_Comp=factor(ifelse(data_wide$Group=="Ibuprofen"& data_wide$Resp_score>=3,"Responder",ifelse(data_wide$Group=="Ibuprofen"& data_wide$Resp_score<3,"Non-Responder","Placebo")),levels=c("Placebo", "Non-Responder", "Responder"))


##Calculate PK parameters##
data_wide$Ke=(log(data_wide$Ibuprofen.PS1)-log(data_wide$Ibuprofen.PS2))/(data_wide$deltaTdrug.PS2blood-data_wide$deltaTdrug.PS1blood)
data_wide$half.life=log(2)/data_wide$Ke

##Create subsets for each brain region##
insula=rCBF[which(rCBF$Region=="Insula"),]
S1=rCBF[which(rCBF$Region=="S1"),]
S2=rCBF[which(rCBF$Region=="S2"),]
thalamus=rCBF[which(rCBF$Region=="Thalamus"),]
ACC=rCBF[which(rCBF$Region=="ACC"),]
amygdala=rCBF[which(rCBF$Region=="Amygdala"),]
hippocampus=rCBF[which(rCBF$Region=="Hippocampus"),]
parahippocampus=rCBF[which(rCBF$Region=="Parahippocampus"),]
pain=rCBF[which(rCBF$Region=="Pain"),]
response=data_wide[c("Subject","Status2")]
rCBF_full=merge(rCBF,response,by="Subject")

library(reshape)
rCBF_melt=melt(rCBF_full, id.vars=c("Subject", "Region","Status2"),variable.name="Time",value.name="rCBF")
rCBF_post_melt=rCBF_melt[which(rCBF_melt$Time!="Pre"),]
mean_rCBF=cast(rCBF_post_melt,Subject~Region,mean, na.rm=TRUE)
mean_rCBF_full=merge(mean_rCBF,response, by="Subject")
write.csv(rCBF_full,file="Dental_Pain_rCBF.csv")

detach(package:reshape)
library(reshape2)
insula_response=merge(insula,response,by="Subject")
S1_response=merge(S1,response,by="Subject")
S2_response=merge(S2,response,by="Subject")
thalamus_response=merge(thalamus,response,by="Subject")
ACC_response=merge(ACC,response,by="Subject")
amygdala_response=merge(amygdala,response,by="Subject")
hippocampus_response=merge(hippocampus,response,by="Subject")
parahippocampus_response=merge(parahippocampus,response,by="Subject")
pain_response=merge(pain,response,by="Subject")

perc_pain_response=data.frame(Subject=pain_response$Subject,Status2=pain_response$Status2,Pre=pain_response$Pre*100/pain_response$Pre,Post=pain_response$Post*100/pain_response$Pre,Post1=pain_response$Post1*100/pain_response$Pre,Post2=pain_response$Post2*100/pain_response$Pre,Post3=pain_response$Post3*100/pain_response$Pre,Post4=pain_response$Post4*100/pain_response$Pre)

insula_resp_melt=melt(insula_response, id.vars=c("Subject", "Region","Status2"),variable.name="Time",value.name="rCBF")
insula_resp_melt$Time=factor(insula_resp_melt$Time,levels=c("Pre","Post","Post1","Post2","Post3","Post4"))

S1_resp_melt=melt(S1_response, id.vars=c("Subject", "Region","Status2"),variable.name="Time",value.name="rCBF")
S1_resp_melt$Time=factor(S1_resp_melt$Time,levels=c("Pre","Post","Post1","Post2","Post3","Post4"))

S2_resp_melt=melt(S2_response, id.vars=c("Subject", "Region","Status2"),variable.name="Time",value.name="rCBF")
S2_resp_melt$Time=factor(S2_resp_melt$Time,levels=c("Pre","Post","Post1","Post2","Post3","Post4"))

thalamus_resp_melt=melt(thalamus_response, id.vars=c("Subject", "Region","Status2"),variable.name="Time",value.name="rCBF")
thalamus_resp_melt$Time=factor(thalamus_resp_melt$Time,levels=c("Pre","Post","Post1","Post2","Post3","Post4"))

ACC_resp_melt=melt(ACC_response, id.vars=c("Subject", "Region","Status2"),variable.name="Time",value.name="rCBF")
ACC_resp_melt$Time=factor(ACC_resp_melt$Time,levels=c("Pre","Post","Post1","Post2","Post3","Post4"))

amygdala_resp_melt=melt(amygdala_response, id.vars=c("Subject", "Region","Status2"),variable.name="Time",value.name="rCBF")
amygdala_resp_melt$Time=factor(amygdala_resp_melt$Time,levels=c("Pre","Post","Post1","Post2","Post3","Post4"))

hippocampus_resp_melt=melt(hippocampus_response, id.vars=c("Subject", "Region","Status2"),variable.name="Time",value.name="rCBF")
hippocampus_resp_melt$Time=factor(hippocampus_resp_melt$Time,levels=c("Pre","Post","Post1","Post2","Post3","Post4"))

parahippocampus_resp_melt=melt(parahippocampus_response, id.vars=c("Subject", "Region","Status2"),variable.name="Time",value.name="rCBF")
parahippocampus_resp_melt$Time=factor(parahippocampus_resp_melt$Time,levels=c("Pre","Post","Post1","Post2","Post3","Post4"))

pain_resp_melt=melt(pain_response, id.vars=c("Subject", "Region","Status2"),variable.name="Time",value.name="rCBF")
pain_resp_melt$Time=factor(pain_resp_melt$Time,levels=c("Pre","Post","Post1","Post2","Post3","Post4"))

perc_pain_resp_melt=melt(perc_pain_response, id.vars=c("Subject","Status2"),variable.name="Time",value.name="rCBF")
perc_pain_resp_melt$Time=factor(perc_pain_resp_melt$Time,levels=c("Pre","Post","Post1","Post2","Post3","Post4"))

##Limiting to Pre-Post2##
insula_resp_melt2=insula_resp_melt[which(insula_resp_melt$Time=="Pre"|insula_resp_melt$Time=="Post"|insula_resp_melt$Time=="Post1"|insula_resp_melt$Time=="Post2"),]
S1_resp_melt2=S1_resp_melt[which(S1_resp_melt$Time=="Pre"|S1_resp_melt$Time=="Post"|S1_resp_melt$Time=="Post1"|S1_resp_melt$Time=="Post2"),]
S2_resp_melt2=S2_resp_melt[which(S2_resp_melt$Time=="Pre"|S2_resp_melt$Time=="Post"|S2_resp_melt$Time=="Post1"|S2_resp_melt$Time=="Post2"),]
thalamus_resp_melt2=thalamus_resp_melt[which(thalamus_resp_melt$Time=="Pre"|thalamus_resp_melt$Time=="Post"|thalamus_resp_melt$Time=="Post1"|thalamus_resp_melt$Time=="Post2"),]
ACC_resp_melt2=ACC_resp_melt[which(ACC_resp_melt$Time=="Pre"|ACC_resp_melt$Time=="Post"|ACC_resp_melt$Time=="Post1"|ACC_resp_melt$Time=="Post2"),]
amygdala_resp_melt2=amygdala_resp_melt[which(amygdala_resp_melt$Time=="Pre"|amygdala_resp_melt$Time=="Post"|amygdala_resp_melt$Time=="Post1"|amygdala_resp_melt$Time=="Post2"),]
hippocampus_resp_melt2=hippocampus_resp_melt[which(hippocampus_resp_melt$Time=="Pre"|hippocampus_resp_melt$Time=="Post"|hippocampus_resp_melt$Time=="Post1"|hippocampus_resp_melt$Time=="Post2"),]
parahippocampus_resp_melt2=parahippocampus_resp_melt[which(parahippocampus_resp_melt$Time=="Pre"|parahippocampus_resp_melt$Time=="Post"|parahippocampus_resp_melt$Time=="Post1"|parahippocampus_resp_melt$Time=="Post2"),]
pain_resp_melt2=pain_resp_melt[which(pain_resp_melt$Time=="Pre"|pain_resp_melt$Time=="Post"|pain_resp_melt$Time=="Post1"|pain_resp_melt$Time=="Post2"|pain_resp_melt$Time=="Post3"),]
perc_pain_resp_melt2=perc_pain_resp_melt[which(perc_pain_resp_melt$Time=="Pre"|perc_pain_resp_melt$Time=="Post"|perc_pain_resp_melt$Time=="Post1"|perc_pain_resp_melt$Time=="Post2"|perc_pain_resp_melt$Time=="Post3"),]

##Limiting to Post##
insula_resp_melt3=insula_resp_melt[which(insula_resp_melt$Time!="Pre"),]
S1_resp_melt3=S1_resp_melt[which(S1_resp_melt$Time!="Pre"),]
S2_resp_melt3=S2_resp_melt[which(S2_resp_melt$Time!="Pre"),]
thalamus_resp_melt3=thalamus_resp_melt[which(thalamus_resp_melt$Time!="Pre"),]
ACC_resp_melt3=ACC_resp_melt[which(ACC_resp_melt$Time!="Pre"),]
amygdala_resp_melt3=amygdala_resp_melt[which(amygdala_resp_melt$Time!="Pre"),]
hippocampus_resp_melt3=hippocampus_resp_melt[which(hippocampus_resp_melt$Time!="Pre"),]
parahippocampus_resp_melt3=parahippocampus_resp_melt[which(parahippocampus_resp_melt$Time!="Pre"),]
pain_resp_melt3=pain_resp_melt[which(pain_resp_melt$Time!="Pre"),]
perc_pain_resp_melt3=perc_pain_resp_melt[which(perc_pain_resp_melt$Time!="Pre"),]

##1-way anova on average post rCBF with 3 groups##
insula_1way=aov(Insula~Status2,mean_rCBF_full)
S1_1way=aov(S1~Status2,mean_rCBF_full)
S2_1way=aov(S2~Status2,mean_rCBF_full)
thalamus_1way=aov(Thalamus~Status2,mean_rCBF_full)
ACC_1way=aov(ACC~Status2,mean_rCBF_full)
amygdala_1way=aov(Amygdala~Status2,mean_rCBF_full)
hippocampus_1way=aov(Hippocampus~Status2,mean_rCBF_full)
parahippocampus_1way=aov(Parahippocampus~Status2,mean_rCBF_full)

##1-way anova on average post rCBF, ibu-treated only##
insula_1way_ibu=aov(Insula~Status2,mean_rCBF_full[which(mean_rCBF_full$Status2!="Placebo"),])
S1_1way_ibu=aov(S1~Status2,mean_rCBF_full[which(mean_rCBF_full$Status2!="Placebo"),])
S2_1way_ibu=aov(S2~Status2,mean_rCBF_full[which(mean_rCBF_full$Status2!="Placebo"),])
thalamus_1way_ibu=aov(Thalamus~Status2,mean_rCBF_full[which(mean_rCBF_full$Status2!="Placebo"),])
ACC_1way_ibu=aov(ACC~Status2,mean_rCBF_full[which(mean_rCBF_full$Status2!="Placebo"),])
amygdala_1way_ibu=aov(Amygdala~Status2,mean_rCBF_full[which(mean_rCBF_full$Status2!="Placebo"),])
hippocampus_1way_ibu=aov(Hippocampus~Status2,mean_rCBF_full[which(mean_rCBF_full$Status2!="Placebo"),])
parahippocampus_1way_ibu=aov(Parahippocampus~Status2,mean_rCBF_full[which(mean_rCBF_full$Status2!="Placebo"),])


##2-way anova with 3 groups##
insula_2way=aov(rCBF~Status2+Time,insula_resp_melt3)
S1_2way=aov(rCBF~Status2+Time,S1_resp_melt3)
S2_2way=aov(rCBF~Status2+Time,S2_resp_melt3)
thalamus_2way=aov(rCBF~Status2+Time,thalamus_resp_melt3)
ACC_2way=aov(rCBF~Status2+Time,ACC_resp_melt3)
amygdala_2way=aov(rCBF~Status2+Time,amygdala_resp_melt3)
hippocampus_2way=aov(rCBF~Status2+Time,hippocampus_resp_melt3)
parahippocampus_2way=aov(rCBF~Status2+Time,parahippocampus_resp_melt3)

##2-way anova, ibu treated only##
insula_2way_ibu=aov(rCBF~Status2+Time,insula_resp_melt3[which(insula_resp_melt3$Status2!="Placebo"),])
S1_2way_ibu=aov(rCBF~Status2+Time,S1_resp_melt3[which(S1_resp_melt3$Status2!="Placebo"),])
S2_2way_ibu=aov(rCBF~Status2+Time,S2_resp_melt3[which(S2_resp_melt3$Status2!="Placebo"),])
thalamus_2way_ibu=aov(rCBF~Status2+Time,thalamus_resp_melt3[which(thalamus_resp_melt3$Status2!="Placebo"),])
ACC_2way_ibu=aov(rCBF~Status2+Time,ACC_resp_melt3[which(ACC_resp_melt3$Status2!="Placebo"),])
amygdala_2way_ibu=aov(rCBF~Status2+Time,amygdala_resp_melt3[which(amygdala_resp_melt3$Status2!="Placebo"),])
hippocampus_2way_ibu=aov(rCBF~Status2+Time,hippocampus_resp_melt3[which(hippocampus_resp_melt3$Status2!="Placebo"),])
parahippocampus_2way_ibu=aov(rCBF~Status2+Time,parahippocampus_resp_melt3[which(parahippocampus_resp_melt3$Status2!="Placebo"),])

summary(insula_1way)
summary(S1_1way)
summary(S2_1way)
summary(thalamus_1way)
summary(ACC_1way)
summary(amygdala_1way)
summary(hippocampus_1way)
summary(parahippocampus_1way)

summary(insula_2way)
summary(S1_2way)
summary(S2_2way)
summary(thalamus_2way)
summary(ACC_2way)
summary(amygdala_2way)
summary(hippocampus_2way)
summary(parahippocampus_2way)

summary(insula_1way_ibu)
summary(S1_1way_ibu)
summary(S2_1way_ibu)
summary(thalamus_1way_ibu)
summary(ACC_1way_ibu)
summary(amygdala_1way_ibu)
summary(hippocampus_1way_ibu)
summary(parahippocampus_1way_ibu)

summary(insula_2way_ibu)
summary(S1_2way_ibu)
summary(S2_2way_ibu)
summary(thalamus_2way_ibu)
summary(ACC_2way_ibu)
summary(amygdala_2way_ibu)
summary(hippocampus_2way_ibu)
summary(parahippocampus_2way_ibu)


##Summary statistics by responder status##
library(MESS)
library(dplyr)
library(plyr)
Q1=function(x){
	quantile(x,0.25, na.rm=TRUE)}
Q3=function(x){
	quantile(x,0.75, na.rm=TRUE)}


all_subgroups=summarize(group_by(data_wide,Status2, Status_PRR50,Status_PRR70,Status_PID100,Status_PID70,Status_PID50,Status_PGIC3,Status_PGIC2,Status_Rdesc34,Status_Rdesc24,Demo_Gender), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

all_subgroups_nogender=summarize(group_by(data_wide,Status2, Status_PRR50,Status_PRR70,Status_PID100,Status_PID70,Status_PID50,Status_PGIC3,Status_PGIC2,Status_Rdesc34,Status_Rdesc24), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

PRR70_subgroups=summarize(group_by(data_wide,Status_PRR70,Demo_Gender), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

PRR70_subgroups_nogender=summarize(group_by(data_wide,Status_PRR70), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

PRR50_subgroups=summarize(group_by(data_wide,Status_PRR50,Demo_Gender), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

PRR50_subgroups_nogender=summarize(group_by(data_wide,Status_PRR50), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

PID70_subgroups=summarize(group_by(data_wide,Status_PID70,Demo_Gender), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

PID70_subgroups_nogender=summarize(group_by(data_wide,Status_PID70), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

PGIC3_subgroups=summarize(group_by(data_wide,Status_PGIC3,Demo_Gender), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

PGIC3_subgroups_nogender=summarize(group_by(data_wide,Status_PGIC3), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

Rdesc34_subgroups=summarize(group_by(data_wide,Status_Rdesc34,Demo_Gender), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

Rdesc34_subgroups_nogender=summarize(group_by(data_wide,Status_Rdesc34), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))
library(dplyr)
Rescue_subgroups=summarize(group_by(all_bio,Status2,Demo_Gender), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_median=median(Number_Teeth, na.rm=TRUE), Number_Teeth_Q1=Q1(Number_Teeth),Number_Teeth_Q3=Q3(Number_Teeth),End1_median=median(End1.pain.score, na.rm=TRUE), End1_Q1=Q1(End1.pain.score),End1_Q3=Q3(End1.pain.score),PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_median=median(DOS_Trauma_SUM, na.rm=TRUE), Trauma_Q1=Q1(DOS_Trauma_SUM),Trauma_Q3=Q3(DOS_Trauma_SUM),Pain.relief_median=median(Pain.relief.score, na.rm=TRUE), Pain.relief_Q1=Q1(Pain.relief.score),Pain.relief_Q3=Q3(Pain.relief.score),PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

Rescue_subgroups_nogender=summarize(group_by(all_bio,Status2), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_median=median(Number_Teeth, na.rm=TRUE), Number_Teeth_Q1=Q1(Number_Teeth),Number_Teeth_Q3=Q3(Number_Teeth),End1_median=median(End1.pain.score, na.rm=TRUE), End1_Q1=Q1(End1.pain.score),End1_Q3=Q3(End1.pain.score),PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_median=median(DOS_Trauma_SUM, na.rm=TRUE), Trauma_Q1=Q1(DOS_Trauma_SUM),Trauma_Q3=Q3(DOS_Trauma_SUM),Pain.relief_median=median(Pain.relief.score, na.rm=TRUE), Pain.relief_Q1=Q1(Pain.relief.score),Pain.relief_Q3=Q3(Pain.relief.score),PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

Tx_subgroups=summarize(group_by(all_bio,Group,Demo_Gender), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_median=median(Number_Teeth, na.rm=TRUE), Number_Teeth_Q1=Q1(Number_Teeth),Number_Teeth_Q3=Q3(Number_Teeth),End1_median=median(End1.pain.score, na.rm=TRUE), End1_Q1=Q1(End1.pain.score),End1_Q3=Q3(End1.pain.score),PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_median=median(DOS_Trauma_SUM, na.rm=TRUE), Trauma_Q1=Q1(DOS_Trauma_SUM),Trauma_Q3=Q3(DOS_Trauma_SUM),Pain.relief_median=median(Pain.relief.score, na.rm=TRUE), Pain.relief_Q1=Q1(Pain.relief.score),Pain.relief_Q3=Q3(Pain.relief.score),PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

Tx_subgroups_nogender=summarize(group_by(all_bio,Group), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_median=median(Number_Teeth, na.rm=TRUE), Number_Teeth_Q1=Q1(Number_Teeth),Number_Teeth_Q3=Q3(Number_Teeth),End1_median=median(End1.pain.score, na.rm=TRUE), End1_Q1=Q1(End1.pain.score),End1_Q3=Q3(End1.pain.score),PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_median=median(DOS_Trauma_SUM, na.rm=TRUE), Trauma_Q1=Q1(DOS_Trauma_SUM),Trauma_Q3=Q3(DOS_Trauma_SUM),Pain.relief_median=median(Pain.relief.score, na.rm=TRUE), Pain.relief_Q1=Q1(Pain.relief.score),Pain.relief_Q3=Q3(Pain.relief.score),PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

Comp_subgroups=summarize(group_by(data_wide,Status_Comp,Demo_Gender), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))

Comp_subgroups_nogender=summarize(group_by(data_wide,Status_Comp), N=length(Age),AGE_mean=mean(Age, na.rm=TRUE), AGE_sd=sd(Age, na.rm=TRUE),PS1Time_mean=mean(deltaTdrug.PS1, na.rm=TRUE), PS1Time_sd=sd(deltaTdrug.PS1, na.rm=TRUE),
	Length_Surgery_mean=mean(Length_Surgery, na.rm=TRUE), Length_Surgery_sd=sd(Length_Surgery, na.rm=TRUE),Number_Teeth_mean=mean(Number_Teeth, na.rm=TRUE), Number_Teeth_sd=sd(Number_Teeth, na.rm=TRUE),
	PRR_mean=mean(EvalPain_Relief_Rate, na.rm=TRUE), PRR_sd=sd(EvalPain_Relief_Rate, na.rm=TRUE),Trauma_mean=mean(DOS_Trauma_SUM, na.rm=TRUE), Trauma_sd=sd(DOS_Trauma_SUM, na.rm=TRUE),
	PTGS1_mean=mean(PTGS1.basal, na.rm=TRUE), PTGS1_sd=sd(PTGS1.basal, na.rm=TRUE),PTGS2_mean=mean(PTGS2.basal, na.rm=TRUE), PTGS2_sd=sd(PTGS2.basal, na.rm=TRUE),
	COX.1.basal_mean=mean(COX.1.basal, na.rm=TRUE), COX.1.basal_sd=sd(COX.1.basal, na.rm=TRUE), COX.2.basal_mean=mean(COX.2.basal, na.rm=TRUE), COX.2.basal_sd=sd(COX.2.basal, na.rm=TRUE),
	PGEM.basal_mean=mean(PGEM.basal, na.rm=TRUE), PGEM.basal_sd=sd(PGEM.basal, na.rm=TRUE),PGDM.basal_mean=mean(PGDM.basal, na.rm=TRUE), PGDM.basal_sd=sd(PGDM.basal, na.rm=TRUE),PGIM.basal_mean=mean(PGIM.basal, na.rm=TRUE), PGIM.basal_sd=sd(PGIM.basal, na.rm=TRUE),TxM.basal_mean=mean(TxM.basal, na.rm=TRUE), TxM.basal_sd=sd(TxM.basal, na.rm=TRUE),
	Ibuprofen.PS1_mean=mean(Ibuprofen.PS1, na.rm=TRUE), Ibuprofen.PS1_sd=sd(Ibuprofen.PS1, na.rm=TRUE),Ibuprofen.PS2_mean=mean(Ibuprofen.PS2, na.rm=TRUE), Ibuprofen.PS2_sd=sd(Ibuprofen.PS2, na.rm=TRUE),
	APAP.PS1_mean=mean(APAP.PS1, na.rm=TRUE), APAP.PS1_sd=sd(APAP.PS1, na.rm=TRUE),APAP.PS2_mean=mean(APAP.PS2, na.rm=TRUE), APAP.PS2_sd=sd(APAP.PS2, na.rm=TRUE),	
	COX.2.PS1.percent_mean=mean(COX.2.PS1.percent, na.rm=TRUE), COX.2.PS1.percent_sd=sd(COX.2.PS1.percent, na.rm=TRUE),
	PGEM.PS1.percent_mean=mean(PGEM.PS1.percent, na.rm=TRUE), PGEM.PS1.percent_sd=sd(PGEM.PS1.percent, na.rm=TRUE),
	PGIM.PS1.percent_mean=mean(PGIM.PS1.percent, na.rm=TRUE), PGIM.PS1.percent_sd=sd(PGIM.PS1.percent, na.rm=TRUE),
	PGDM.PS1.percent_mean=mean(PGDM.PS1.percent, na.rm=TRUE), PGDM.PS1.percent_sd=sd(PGDM.PS1.percent, na.rm=TRUE),
	TxM.PS1.percent_mean=mean(TxM.PS1.percent, na.rm=TRUE), TxM.PS1.percent_sd=sd(TxM.PS1.percent, na.rm=TRUE))



write.csv(all_subgroups,"All_baseline.csv")
write.csv(all_subgroups_nogender,"All_nogender_baseline.csv")
write.csv(PRR70_subgroups,"PRR70_baseline.csv")
write.csv(PRR70_subgroups_nogender,"PRR70_nogender_baseline.csv")
write.csv(PRR50_subgroups,"PRR50_baseline.csv")
write.csv(PRR50_subgroups_nogender,"PRR50_nogender_baseline.csv")
write.csv(PID70_subgroups,"PID70_baseline.csv")
write.csv(PID70_subgroups_nogender,"PID70_nogender_baseline.csv")
write.csv(PGIC3_subgroups,"PGIC3_baseline.csv")
write.csv(PGIC3_subgroups_nogender,"PGIC3_nogender_baseline.csv")
write.csv(Rdesc34_subgroups,"Rdesc34_baseline.csv")
write.csv(Rdesc34_subgroups_nogender,"Rdesc34_nogender_baseline.csv")
write.csv(Rescue_subgroups,"Rescue_baseline.csv")
write.csv(Rescue_subgroups_nogender,"Rescue_nogender_baseline.csv")
write.csv(Tx_subgroups,"Tx_baseline.csv")
write.csv(Tx_subgroups_nogender,"Tx_nogender_baseline.csv")

write.csv(Comp_subgroups,"Comp_baseline.csv")
write.csv(Comp_subgroups_nogender,"Comp_nogender_baseline.csv")


data_melt=melt(data_long,id.vars=c("Subject","Age","Event","deltaTdrug.rescue","End1.pain.score","Pain.relief.score","Rescue", "Group","Status","Status2"))
data_baseline_melt=data_melt[which(data_melt$Event=="Baseline"),]

base=ddply(data_baseline_melt,c("Status2","variable"),summarise,N=length(value,na.rm=TRUE),mean=mean(value,na.rm=TRUE),sd=sd(value,na.rm=TRUE),sem=sd(value,na.rm=TRUE)/sqrt(length(value,na.rm=TRUE)))
all=ddply(data_melt,c("Status2","Event","variable"),summarise,N=length(value),mean=mean(value,na.rm=TRUE),sd=sd(value,na.rm=TRUE),sem=sd(value,na.rm=TRUE)/sqrt(length(value)))

write.csv(base,file="Dental_baseline.csv")
write.csv(all,file="Dental_all_events.csv")

##Extracting only numeric variables for correlation - all subjects##
data_r=data_wide[which(data_wide$Status2=="Responder"),]
data_nr=data_wide[which(data_wide$Status=="Non-Responder"),]
data_p=data_wide[which(data_wide$Group=="Placebo"),]
data_i=all_bio[which(all_bio$Group=="Ibuprofen"),]
data_long_i=data_long[which(data_long$Group=="Ibuprofen"),]

data_noAPAP1=all_bio[which(is.na(all_bio$APAP.PS1)=="TRUE"|all_bio$APAP.PS1<1),]
data_noAPAPi=data_i[which(is.na(data_i$APAP.PS1)=="TRUE"|data_i$APAP.PS1<1),]


data=all_bio[,sapply(all_bio,is.numeric)]
all_corr=cor(data, use="pairwise.complete.obs", method="spearman")

data_inum=data_i[,sapply(data_i,is.numeric)]
ibu_corr=cor(data_inum, use="pairwise.complete.obs", method="spearman")

data_noapap1=data_noAPAP1[,sapply(data_noAPAP1,is.numeric)]
noAPAP1_corr=cor(data_noapap1, use="pairwise.complete.obs", method="spearman")

write.csv(all_corr,file="Dental_all_correlations.csv")
write.csv(ibu_corr,file="Dental_ibu_correlations.csv")
write.csv(noAPAP1_corr,file="Dental_noAPAP1_correlations.csv")

##Graphs##
library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),
 axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))

pdf("Dental_graphs_v2.pdf")
ggplot(data_wide, aes(Status2, Pain.relief.score))+geom_boxplot(aes(fill=Status2))+labs(title="Pain Relief Score", x="Responder Status", y="Pain Relief Score")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, End1.pain.score))+geom_boxplot(aes(fill=Status2))+labs(title="Pain Score Before Study Drug Administration", x="Responder Status", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.1.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="COX-1 Activity:  Post-Surgery 1", x="Responder Status", y="COX-1 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.2.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="COX-2 Activity:  Post-Surgery 1", x="Responder Status", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.2.basal))+geom_boxplot(aes(fill=Status2))+labs(title="COX-2 Activity:  Baseline", x="Responder Status", y=expression(PGE[2] (ng/ml)))+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.1.basal))+geom_boxplot(aes(fill=Status2))+labs(title="COX-1 Activity:  Baseline", x="Responder Status", y=expression(TxB[2] (ng/ml)))+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_noAPAP1, aes(Status2, PGEM.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="COX-2 Activity:  Post-Surgery 1 \nNo APAP", x="Responder Status", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.1.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="COX-1 Activity:  Post-Surgery 2", x="Responder Status", y="COX-1 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.2.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="COX-2 Activity:  Post-Surgery 2", x="Responder Status", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, deltaTdrug.rescue))+geom_boxplot(aes(fill=Status2))+labs(title="Time to Rescue", x="Responder Status", y="Hours")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+annotate("text", x=3,y=2.75,label="No rescue medication")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, Ibuprofen.PS1))+geom_boxplot(aes(fill=Status2))+labs(title="Ibuprofen Plasma Concentrations:\nPost-Surgery1", x="Responder Status", y="Ibuprofen Plasma Concentrations (micromolar)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, (Ibuprofen.PS1/Weight)))+geom_boxplot(aes(fill=Status2))+labs(title="Weight-Normalized Ibuprofen Plasma Concentrations:\nPost-Surgery1", x="Responder Status", y="Ibuprofen Plasma Concentrations (micromolar/kg)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, half.life))+geom_boxplot(aes(fill=Status2))+labs(title="Ibuprofen Half-Life", x="Responder Status", y="Ibuprofen Half-Life (h)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, Insula))+geom_boxplot(aes(fill=Status2))+labs(title="Insula", x="Responder Status", y="Relative CBF")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, S1))+geom_boxplot(aes(fill=Status2))+labs(title="S1", x="Responder Status", y="Relative CBF")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, S2))+geom_boxplot(aes(fill=Status2))+labs(title="S2", x="Responder Status", y="Relative CBF")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, ACC))+geom_boxplot(aes(fill=Status2))+labs(title="ACC", x="Responder Status", y="Relative CBF")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, Amygdala))+geom_boxplot(aes(fill=Status2))+labs(title="Amygdala", x="Responder Status", y="Relative CBF")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, Thalamus))+geom_boxplot(aes(fill=Status2))+labs(title="Thalamus", x="Responder Status", y="Relative CBF")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, Hippocampus))+geom_boxplot(aes(fill=Status2))+labs(title="Hippocampus", x="Responder Status", y="Relative CBF")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, Parahippocampus))+geom_boxplot(aes(fill=Status2))+labs(title="Parahippocampus", x="Responder Status", y="Relative CBF")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGEM.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PGEM:  Post-Surgery 1", x="Responder Status", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGEM.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PGEM:  Post-Surgery 2", x="Responder Status", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGIM.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PGIM:  Post-Surgery 1", x="Responder Status", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGIM.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PGIM:  Post-Surgery 2", x="Responder Status", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGEM.basal))+geom_boxplot(aes(fill=Status2))+labs(title="PGEM:  Baseline", x="Responder Status", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGIM.basal))+geom_boxplot(aes(fill=Status2))+labs(title="PGIM:  Baseline", x="Responder Status", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGEM.PS1.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Post-Surgery 1", x="Responder Status", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status2, PGEM.PS2.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Post-Surgery 2", x="Responder Status", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status2, PGIM.PS1.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Post-Surgery 1", x="Responder Status", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status2, PGIM.PS2.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Post-Surgery 2", x="Responder Status", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status2, PGEM.basal))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Baseline", x="Responder Status", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status2, PGIM.basal))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Baseline", x="Responder Status", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")




ggplot(data_wide, aes(Status_PRR70, Pain.relief.score))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="Pain Relief Score", x="Responder Status (PRR>70)", y="Pain Relief Score")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, End1.pain.score))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="Pain Score Before Study Drug Administration", x="Responder Status (PRR>70)", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, COX.1.PS1.percent))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="COX-1 Activity:  Post-Surgery 1", x="Responder Status (PRR>70)", y="COX-1 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, COX.2.PS1.percent))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="COX-2 Activity:  Post-Surgery 1", x="Responder Status (PRR>70)", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, COX.2.basal))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="COX-2 Activity:  Baseline", x="Responder Status (PRR>70)", y=expression(PGE[2] (ng/ml)))+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, COX.1.basal))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="COX-1 Activity:  Baseline", x="Responder Status (PRR>70)", y=expression(TxB[2] (ng/ml)))+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_noAPAP1, aes(Status_PRR70, COX.2.PS1.percent))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="COX-2 Activity:  Post-Surgery 1 \nNo APAP", x="Responder Status (PRR>70)", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, PGEM.PS1.percent))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="PGEM:  Post-Surgery 1", x="Responder Status (PRR>70)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, PGEM.PS2.percent))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="PGEM:  Post-Surgery 2", x="Responder Status (PRR>70)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, PGIM.PS1.percent))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="PGIM:  Post-Surgery 1", x="Responder Status (PRR>70)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, PGIM.PS2.percent))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="PGIM:  Post-Surgery 2", x="Responder Status (PRR>70)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, PGEM.basal))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="PGEM:  Baseline", x="Responder Status (PRR>70)", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, PGIM.basal))+geom_boxplot(aes(fill=Status_PRR70))+labs(title="PGIM:  Baseline", x="Responder Status (PRR>70)", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PRR70, PGEM.PS1.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Post-Surgery 1", x="Responder Status (PRR>70)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_PRR70, PGEM.PS2.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Post-Surgery 2", x="Responder Status (PRR>70)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_PRR70, PGIM.PS1.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Post-Surgery 1", x="Responder Status (PRR>70)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_PRR70, PGIM.PS2.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Post-Surgery 2", x="Responder Status (PRR>70)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_PRR70, PGEM.basal))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Baseline", x="Responder Status (PRR>70)", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_PRR70, PGIM.basal))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Baseline", x="Responder Status (PRR>70)", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")

ggplot(data_wide, aes(Status_PID70, Pain.relief.score))+geom_boxplot(aes(fill=Status_PID70))+labs(title="Pain Relief Score", x="Responder Status (PID>70)", y="Pain Relief Score")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, End1.pain.score))+geom_boxplot(aes(fill=Status_PID70))+labs(title="Pain Score Before Study Drug Administration", x="Responder Status (PID>70)", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, COX.1.PS1.percent))+geom_boxplot(aes(fill=Status_PID70))+labs(title="COX-1 Activity:  Post-Surgery 1", x="Responder Status (PID>70)", y="COX-1 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, COX.2.PS1.percent))+geom_boxplot(aes(fill=Status_PID70))+labs(title="COX-2 Activity:  Post-Surgery 1", x="Responder Status (PID>70)", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, COX.2.basal))+geom_boxplot(aes(fill=Status_PID70))+labs(title="COX-2 Activity:  Baseline", x="Responder Status (PID>70)", y=expression(PGE[2] (ng/ml)))+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, COX.1.basal))+geom_boxplot(aes(fill=Status_PID70))+labs(title="COX-1 Activity:  Baseline", x="Responder Status (PID>70)", y=expression(TxB[2] (ng/ml)))+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_noAPAP1, aes(Status_PID70, COX.2.PS1.percent))+geom_boxplot(aes(fill=Status_PID70))+labs(title="COX-2 Activity:  Post-Surgery 1 \nNo APAP", x="Responder Status (PID>70)", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, PGEM.PS1.percent))+geom_boxplot(aes(fill=Status_PID70))+labs(title="PGEM:  Post-Surgery 1", x="Responder Status (PID>70)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, PGEM.PS2.percent))+geom_boxplot(aes(fill=Status_PID70))+labs(title="PGEM:  Post-Surgery 2", x="Responder Status (PID>70)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, PGIM.PS1.percent))+geom_boxplot(aes(fill=Status_PID70))+labs(title="PGIM:  Post-Surgery 1", x="Responder Status (PID>70)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, PGIM.PS2.percent))+geom_boxplot(aes(fill=Status_PID70))+labs(title="PGIM:  Post-Surgery 2", x="Responder Status (PID>70)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, PGEM.basal))+geom_boxplot(aes(fill=Status_PID70))+labs(title="PGEM:  Baseline", x="Responder Status (PID>70)", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, PGIM.basal))+geom_boxplot(aes(fill=Status_PID70))+labs(title="PGIM:  Baseline", x="Responder Status (PID>70)", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_PID70, PGEM.PS1.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Post-Surgery 1", x="Responder Status (PID>70)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_PID70, PGEM.PS2.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Post-Surgery 2", x="Responder Status (PID>70)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_PID70, PGIM.PS1.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Post-Surgery 1", x="Responder Status (PID>70)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_PID70, PGIM.PS2.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Post-Surgery 2", x="Responder Status (PID>70)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_PID70, PGEM.basal))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Baseline", x="Responder Status (PID>70)", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_PID70, PGIM.basal))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Baseline", x="Responder Status (PID>70)", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")

ggplot(data_wide, aes(Status_Comp, Pain.relief.score))+geom_boxplot(aes(fill=Status_Comp))+labs(title="Pain Relief Score", x="Responder Status (Combined Score>2)", y="Pain Relief Score")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, End1.pain.score))+geom_boxplot(aes(fill=Status_Comp))+labs(title="Pain Score Before Study Drug Administration", x="Responder Status (Combined Score>2)", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, COX.1.PS1.percent))+geom_boxplot(aes(fill=Status_Comp))+labs(title="COX-1 Activity:  Post-Surgery 1", x="Responder Status (Combined Score>2)", y="COX-1 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, COX.2.PS1.percent))+geom_boxplot(aes(fill=Status_Comp))+labs(title="COX-2 Activity:  Post-Surgery 1", x="Responder Status (Combined Score>2)", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, COX.2.basal))+geom_boxplot(aes(fill=Status_Comp))+labs(title="COX-2 Activity:  Baseline", x="Responder Status (Combined Score>2)", y=expression(PGE[2] (ng/ml)))+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, COX.1.basal))+geom_boxplot(aes(fill=Status_Comp))+labs(title="COX-1 Activity:  Baseline", x="Responder Status (Combined Score>2)", y=expression(TxB[2] (ng/ml)))+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_noAPAP1, aes(Status_Comp, COX.2.PS1.percent))+geom_boxplot(aes(fill=Status_Comp))+labs(title="COX-2 Activity:  Post-Surgery 1 \nNo APAP", x="Responder Status (Combined Score>2)", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, PGEM.PS1.percent))+geom_boxplot(aes(fill=Status_Comp))+labs(title="PGEM:  Post-Surgery 1", x="Responder Status (Combined Score>2)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, PGEM.PS2.percent))+geom_boxplot(aes(fill=Status_Comp))+labs(title="PGEM:  Post-Surgery 2", x="Responder Status (Combined Score>2)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, PGIM.PS1.percent))+geom_boxplot(aes(fill=Status_Comp))+labs(title="PGIM:  Post-Surgery 1", x="Responder Status (Combined Score>2)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, PGIM.PS2.percent))+geom_boxplot(aes(fill=Status_Comp))+labs(title="PGIM:  Post-Surgery 2", x="Responder Status (Combined Score>2)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, PGEM.basal))+geom_boxplot(aes(fill=Status_Comp))+labs(title="PGEM:  Baseline", x="Responder Status (Combined Score>2)", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, PGIM.basal))+geom_boxplot(aes(fill=Status_Comp))+labs(title="PGIM:  Baseline", x="Responder Status (Combined Score>2)", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status_Comp, PGEM.PS1.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Post-Surgery 1", x="Responder Status (Combined Score>2)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_Comp, PGEM.PS2.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Post-Surgery 2", x="Responder Status (Combined Score>2)", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_Comp, PGIM.PS1.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Post-Surgery 1", x="Responder Status (Combined Score>2)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_Comp, PGIM.PS2.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Post-Surgery 2", x="Responder Status (Combined Score>2)", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_Comp, PGEM.basal))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM:  Baseline", x="Responder Status (Combined Score>2)", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")
ggplot(data_wide, aes(Status_Comp, PGIM.basal))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM:  Baseline", x="Responder Status (Combined Score>2)", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Spectral")



dev.off()


pdf("rCBF_over_time.pdf")
library(Hmisc)

##Individual curves all timepoints##
ggplot(insula_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+labs(title="Insula", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(S1_resp_melt, aes(Time, rCBF, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+labs(title="S1", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(S2_resp_melt, aes(Time, rCBF, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+labs(title="S2", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(thalamus_resp_melt, aes(Time, rCBF, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+labs(title="Thalamus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(ACC_resp_melt, aes(Time, rCBF, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+labs(title="ACC", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(amygdala_resp_melt, aes(Time, rCBF, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+labs(title="Amygdala", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(hippocampus_resp_melt, aes(Time, rCBF, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+labs(title="Hippocampus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(parahippocampus_resp_melt, aes(Time, rCBF, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+labs(title="Parahippocampus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(pain_resp_melt, aes(Time, rCBF, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+labs(title="Pain Score", x="Measurement", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(perc_pain_resp_melt, aes(Time, rCBF, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+labs(title="Percent of Maximum Pain Score", x="Measurement", y="Percent of Maximum Pain Score")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")

pdf("DC_graphs.pdf")

##Median with IQR, Pre-Post2##
Q1=function(x){
	quantile(x,0.25, na.rm=TRUE)}
Q3=function(x){
	quantile(x,0.75, na.rm=TRUE)}

ggplot(luminex.all, aes(Event, IL10, group=Status2,color=Status2))+labs(title="IL-10", x="Measurement", y="Serum IL-10 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, IL10.percent, group=Status2,color=Status2))+labs(title="IL-10", x="Measurement", y="Serum IL-10 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, IL1b, group=Status2,color=Status2))+labs(title="IL-1b", x="Measurement", y="Serum IL-1b (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, IL1b.percent, group=Status2,color=Status2))+labs(title="IL-1b", x="Measurement", y="Serum IL-1b (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, IL6, group=Status2,color=Status2))+labs(title="IL-6", x="Measurement", y="Serum IL-6 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, IL6.percent, group=Status2,color=Status2))+labs(title="IL-6", x="Measurement", y="Serum IL-6 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, IL8, group=Status2,color=Status2))+labs(title="IL-8", x="Measurement", y="Serum IL-8 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, IL8.percent, group=Status2,color=Status2))+labs(title="IL-8", x="Measurement", y="Serum IL-8 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, MCP1, group=Status2,color=Status2))+labs(title="MCP1", x="Measurement", y="Serum MCP1 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, MCP1.percent, group=Status2,color=Status2))+labs(title="MCP1", x="Measurement", y="Serum MCP1 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, TNFa, group=Status2,color=Status2))+labs(title="TNFa", x="Measurement", y="Serum TNFa (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, TNFa.percent, group=Status2,color=Status2))+labs(title="TNFa", x="Measurement", y="Serum TNFa (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, COX.1, group=Status2,color=Status2))+labs(title="COX-1 Activity", x="Measurement", y="Serum Thromboxane B2 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, COX.1.percent, group=Status2,color=Status2))+labs(title="COX-1 Activity", x="Measurement", y="Serum Thromboxane B2 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, COX.2, group=Status2,color=Status2))+labs(title="COX-2 Activity", x="Measurement", y="Plasma PGE2 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, COX.2.percent, group=Status2,color=Status2))+labs(title="COX-2 Activity", x="Measurement", y="Plasma PGE2 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PGEM, group=Status2,color=Status2))+labs(title="PGEM", x="Measurement", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PGEM.percent, group=Status2,color=Status2))+labs(title="PGEM", x="Measurement", y="Urinary PGEM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PGIM, group=Status2,color=Status2))+labs(title="PGIM", x="Measurement", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PGIM.percent, group=Status2,color=Status2))+labs(title="PGIM", x="Measurement", y="Urinary PGIM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PGDM, group=Status2,color=Status2))+labs(title="PGDM", x="Measurement", y="Urinary PGDM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PGDM.percent, group=Status2,color=Status2))+labs(title="PGDM", x="Measurement", y="Urinary PGDM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, TxM, group=Status2,color=Status2))+labs(title="TxM", x="Measurement", y="Urinary TxM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, TxM.percent, group=Status2,color=Status2))+labs(title="TxM", x="Measurement", y="Urinary TxM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, Ibuprofen, group=Status2,color=Status2))+labs(title="Ibuprofen", x="Measurement", y="Plasma Ibuprofen (mircomolar)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, APAP, group=Status2,color=Status2))+labs(title="APAP", x="Measurement", y="Plasma APAP (micromolar)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PTGS1, group=Status2,color=Status2))+labs(title="PTGS1", x="Measurement", y="PTGS1/GAPDH")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PTGS1.percent, group=Status2,color=Status2))+labs(title="PTGS1", x="Measurement", y="PTGS1/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PTGS2, group=Status2,color=Status2))+labs(title="PTGS2", x="Measurement", y="PTGS2/GAPDH ")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PTGS2.percent, group=Status2,color=Status2))+labs(title="PTGS2", x="Measurement", y="PTGS2/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PACER, group=Status2,color=Status2))+labs(title="PACER", x="Measurement", y="PACER/GAPDH")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(luminex.all, aes(Event, PACER.percent, group=Status2,color=Status2))+labs(title="PACER", x="Measurement", y="PACER/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)


ggplot(rCBF_all_response, aes(dTime, Insula, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="Insula", x="Time", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(rCBF_all_response, aes(dTime, S1, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="S1", x="Time", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(rCBF_all_response, aes(dTime, S2, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="S2", x="Time", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(rCBF_all_response, aes(dTime, Thalamus, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="Thalamus", x="Time", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(rCBF_all_response, aes(dTime, ACC, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="ACC", x="Time", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(rCBF_all_response, aes(dTime, Amygdala, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="Amygdala", x="Time", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(rCBF_all_response, aes(dTime, Hippocampus, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="Hippocampus", x="Time", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(rCBF_all_response, aes(dTime, Parahippocampus, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="Parahippocampus", x="Time", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(rCBF_all_response, aes(dTime, Pain, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="Pain Score", x="Time", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))

ggplot(luminex.all, aes(Time, IL10, group=Status2,color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-10", x="Time", y="Serum IL-10 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, IL1b, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-1b", x="Time", y="Serum IL-1b (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, IL6, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-6", x="Time", y="Serum IL-6 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, IL8, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-8", x="Time", y="Serum IL-8 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, MCP1, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="MCP1", x="Time", y="Serum MCP1 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, TNFa, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="TNFa", x="Time", y="Serum TNFa (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, IL10.percent, group=Status2,color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-10", x="Time", y="Serum IL-10 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, IL1b.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-1b", x="Time", y="Serum IL-1b (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, IL6.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-6", x="Time", y="Serum IL-6 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, IL8.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-8", x="Time", y="Serum IL-8 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, MCP1.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="MCP1", x="Time", y="Serum MCP1 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, TNFa.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="TNFa", x="Time", y="Serum TNFa (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PTGS1, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PTGS1", x="Time", y="PTGS1/GAPDH")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PTGS1.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PTGS1", x="Time", y="PTGS1/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PTGS2, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PTGS2", x="Time", y="PTGS2/GAPDH")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PTGS2.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PTGS2", x="Time", y="PTGS2/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PACER, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PACER", x="Time", y="PACER/GAPDH")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PACER.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PACER", x="Time", y="PACER/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PGEM, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGEM", x="Time", y="PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PGEM.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGEM", x="Time", y="PGEM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PGIM, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGIM", x="Time", y="PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PGIM.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGIM", x="Time", y="PGIM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PGDM, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGDM", x="Time", y="PGDM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, PGDM.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGDM", x="Time", y="PGDM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, TxM, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="TxM", x="Time", y="TxM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))
ggplot(luminex.all, aes(Time, TxM.percent, color=Status2))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="TxM", x="Time", y="TxM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+geom_smooth(method = "lm", formula = y ~ splines::ns(x, df=3),aes(color=Status2))


ggplot(luminex.all, aes(Time, IL10, group=Status2,color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-10", x="Time", y="Serum IL-10 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, IL1b, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-1b", x="Time", y="Serum IL-1b (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, IL6, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-6", x="Time", y="Serum IL-6 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, IL8, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-8", x="Time", y="Serum IL-8 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, MCP1, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="MCP1", x="Time", y="Serum MCP1 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, TNFa, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="TNFa", x="Time", y="Serum TNFa (pg/ml)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, IL10.percent, group=Status2,color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-10", x="Time", y="Serum IL-10 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, IL1b.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-1b", x="Time", y="Serum IL-1b (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, IL6.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-6", x="Time", y="Serum IL-6 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, IL8.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="IL-8", x="Time", y="Serum IL-8 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, MCP1.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="MCP1", x="Time", y="Serum MCP1 (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, TNFa.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="TNFa", x="Time", y="Serum TNFa (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, TNFa.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="TNFa", x="Time", y="Serum TNFa (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_y_log10()
ggplot(luminex.all, aes(Time, PTGS1, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PTGS1", x="Time", y="PTGS1/GAPDH")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PTGS1.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PTGS1", x="Time", y="PTGS1/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PTGS2, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PTGS2", x="Time", y="PTGS2/GAPDH")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PTGS2.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PTGS2", x="Time", y="PTGS2/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PACER, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PACER", x="Time", y="PACER/GAPDH")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PACER.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PACER", x="Time", y="PACER/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PGEM, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGEM", x="Time", y="PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PGEM.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGEM", x="Time", y="PGEM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PGIM, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGIM", x="Time", y="PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PGIM.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGIM", x="Time", y="PGIM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PGDM, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGDM", x="Time", y="PGDM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, PGDM.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="PGDM", x="Time", y="PGDM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, TxM, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="TxM", x="Time", y="TxM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(luminex.all, aes(Time, TxM.percent, color=Status2))+geom_line(aes(group=Subject, size=0.5))+geom_point(aes(size=1))+geom_vline(xintercept = 0, linetype="dashed", size=1)+labs(title="TxM", x="Time", y="TxM (% baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")

ggplot(data_wide, aes(Status2, PTGS1.basal))+geom_boxplot(aes(fill=Status2))+labs(title="PTGS1: Baseline", x="Responder Status", y="PTGS1/GAPDH")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PTGS1.PS1))+geom_boxplot(aes(fill=Status2))+labs(title="PTGS1: Post-Surgery 1", x="Responder Status", y="PTGS1/GAPDH")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PTGS1.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PTGS1: Post-Surgery 1", x="Responder Status", y="PTGS1/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PTGS1.PS2))+geom_boxplot(aes(fill=Status2))+labs(title="PTGS1: Post-Surgery 2", x="Responder Status", y="PTGS1/GAPDH")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PTGS1.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PTGS1: Post-Surgery 2", x="Responder Status", y="PTGS1/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)

ggplot(data_wide, aes(Status2, PTGS2.basal))+geom_boxplot(aes(fill=Status2))+labs(title="PTGS2: Baseline", x="Responder Status", y="PTGS2/GAPDH")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PTGS2.PS1))+geom_boxplot(aes(fill=Status2))+labs(title="PTGS2: Post-Surgery 1", x="Responder Status", y="PTGS2/GAPDH")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PTGS2.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PTGS2: Post-Surgery 1", x="Responder Status", y="PTGS2/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PTGS2.PS2))+geom_boxplot(aes(fill=Status2))+labs(title="PTGS2: Post-Surgery 2", x="Responder Status", y="PTGS2/GAPDH")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PTGS2.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PTGS2: Post-Surgery 2", x="Responder Status", y="PTGS2/GAPDH (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)


dev.off()

pdf("Luminex.pdf")
ggplot(data_luminex, aes(Status2, IL6.basal))+geom_boxplot(aes(fill=Status2))+labs(title="IL-6:  Baseline", x="Responder Status", y="Serum IL-6 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL6.PS1))+geom_boxplot(aes(fill=Status2))+labs(title="IL-6:  Post-Surgery 1", x="Responder Status", y="Serum IL-6 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL6.PS2))+geom_boxplot(aes(fill=Status2))+labs(title="IL-6:  Post-Surgery 2", x="Responder Status", y="Serum IL-6 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL6.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="IL-6:  Post-Surgery 1", x="Responder Status", y="Serum IL-6 (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL6.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="IL-6:  Post-Surgery 2", x="Responder Status", y="Serum IL-6 (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)

ggplot(data_luminex, aes(Status2, IL8.basal))+geom_boxplot(aes(fill=Status2))+labs(title="IL-8:  Baseline", x="Responder Status", y="Serum IL-8 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL8.PS1))+geom_boxplot(aes(fill=Status2))+labs(title="IL-8:  Post-Surgery 1", x="Responder Status", y="Serum IL-8 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL8.PS2))+geom_boxplot(aes(fill=Status2))+labs(title="IL-8:  Post-Surgery 2", x="Responder Status", y="Serum IL-8 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL8.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="IL-8:  Post-Surgery 1", x="Responder Status", y="Serum IL-8 (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL8.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="IL-8:  Post-Surgery 2", x="Responder Status", y="Serum IL-8 (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)

ggplot(data_luminex, aes(Status2, IL10.basal))+geom_boxplot(aes(fill=Status2))+labs(title="IL-10:  Baseline", x="Responder Status", y="Serum IL-10 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL10.PS1))+geom_boxplot(aes(fill=Status2))+labs(title="IL-10:  Post-Surgery 1", x="Responder Status", y="Serum IL-10 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL10.PS2))+geom_boxplot(aes(fill=Status2))+labs(title="IL-10:  Post-Surgery 2", x="Responder Status", y="Serum IL-10 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL10.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="IL-10:  Post-Surgery 1", x="Responder Status", y="Serum IL-10 (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, IL10.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="IL-10:  Post-Surgery 2", x="Responder Status", y="Serum IL-10 (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)

ggplot(data_luminex, aes(Status2, MCP1.basal))+geom_boxplot(aes(fill=Status2))+labs(title="MCP1:  Baseline", x="Responder Status", y="Serum MCP1 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, MCP1.PS1))+geom_boxplot(aes(fill=Status2))+labs(title="MCP1:  Post-Surgery 1", x="Responder Status", y="Serum MCP1 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, MCP1.PS2))+geom_boxplot(aes(fill=Status2))+labs(title="MCP1:  Post-Surgery 2", x="Responder Status", y="Serum MCP1 (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, MCP1.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="MCP1:  Post-Surgery 1", x="Responder Status", y="Serum MCP1 (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, MCP1.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="MCP1:  Post-Surgery 2", x="Responder Status", y="Serum MCP1 (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)

ggplot(data_luminex, aes(Status2, TNFa.basal))+geom_boxplot(aes(fill=Status2))+labs(title="TNFa:  Baseline", x="Responder Status", y="Serum TNFa (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, TNFa.PS1))+geom_boxplot(aes(fill=Status2))+labs(title="TNFa:  Post-Surgery 1", x="Responder Status", y="Serum TNFa (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, TNFa.PS2))+geom_boxplot(aes(fill=Status2))+labs(title="TNFa:  Post-Surgery 2", x="Responder Status", y="Serum TNFa (pg/ml)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, TNFa.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="TNFa:  Post-Surgery 1", x="Responder Status", y="Serum TNFa (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, TNFa.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="TNFa:  Post-Surgery 2", x="Responder Status", y="Serum TNFa (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_luminex, aes(Status2, TNFa.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="TNFa:  Post-Surgery 1", x="Responder Status", y="Serum TNFa (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)+scale_y_log10()
ggplot(data_luminex, aes(Status2, TNFa.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="TNFa:  Post-Surgery 2", x="Responder Status", y="Serum TNFa (% baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)+scale_y_log10()

dev.off()

ggplot(measurement, aes(PTGS2.percent, PGEM.percent, shape=Demo_Gender, color=Status2))+geom_vline(xintercept = 0)+geom_point(aes(size=1))+labs(title="Pain Score", x="Measurement", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
Responders=measurement[which(measurement$Status2=="Responder"),]
cor(Responders$PTGS2.percent,Responders$PGIM.percent,use="complete.obs",method="spearman")

ggplot(measurement, aes(PTGS2.percent, COX.2.percent))+geom_contour(aes(fill=Ibuprofen))

##Median with min and max shaded all timepoints##
ggplot(insula_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Insula", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "median_hilow", size=2, geom="smooth")
ggplot(S1_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="S1", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "median_hilow", size=2, geom="smooth")
ggplot(S2_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="S2", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "median_hilow", size=2, geom="smooth")
ggplot(thalamus_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Thalamus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "median_hilow", size=2, geom="smooth")
ggplot(ACC_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="ACC", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "median_hilow", size=2, geom="smooth")
ggplot(amygdala_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Amygdala", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "median_hilow", size=2, geom="smooth")
ggplot(hippocampus_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Hippocampus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "median_hilow", size=2, geom="smooth")
ggplot(parahippocampus_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Parahippocampus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "median_hilow", size=2, geom="smooth")
ggplot(pain_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Pain Score", x="Measurement", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "median_hilow", size=2, geom="smooth")
ggplot(perc_pain_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Percent of Maximum Pain Score", x="Measurement", y="Percent of Maximum Pain Score")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "median_hilow", size=2, geom="smooth")

##Mean with 95% CI all timepoints##
ggplot(insula_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Insula", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "mean_cl_boot", size=2, geom="smooth")
ggplot(S1_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="S1", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "mean_cl_boot", size=2, geom="smooth")
ggplot(S2_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="S2", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "mean_cl_boot", size=2, geom="smooth")
ggplot(thalamus_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Thalamus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "mean_cl_boot", size=2, geom="smooth")
ggplot(ACC_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="ACC", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "mean_cl_boot", size=2, geom="smooth")
ggplot(amygdala_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Amygdala", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "mean_cl_boot", size=2, geom="smooth")
ggplot(hippocampus_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Hippocampus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "mean_cl_boot", size=2, geom="smooth")
ggplot(parahippocampus_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Parahippocampus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "mean_cl_boot", size=2, geom="smooth")
ggplot(pain_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Pain Score", x="Measurement", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "mean_cl_boot", size=2, geom="smooth")
ggplot(perc_pain_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Percent of Maximum Pain Score", x="Measurement", y="Percent of Maximum Pain Score")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+stat_summary(fun.data = "mean_cl_boot", size=2, geom="smooth")

library(survminer)
pdf("PENTACON_graphs.pdf")
##Median with IQR, Pre-Post2##
Q1=function(x){
	quantile(x,0.25, na.rm=TRUE)}
Q3=function(x){
	quantile(x,0.75, na.rm=TRUE)}

ggplot(insula_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Insula", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(S1_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="S1", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(S2_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="S2", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(thalamus_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Thalamus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(ACC_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="ACC", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(amygdala_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Amygdala", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(hippocampus_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Hippocampus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(parahippocampus_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Parahippocampus", x="Measurement", y="rCBF")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(pain_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Pain Score", x="Measurement", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)
ggplot(perc_pain_resp_melt, aes(Time, rCBF, group=Status2,color=Status2))+labs(title="Percent of Pre-Medication Pain Score", x="Measurement", y="Pain Score (% pre-medication pain score)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Status2), alpha=0.3)

ggplot(data_wide, aes(Status2, Pain.relief.score))+geom_boxplot(aes(fill=Status2))+labs(title="Pain Relief Score", x="Responder Status", y="Pain Relief Score")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, End1.pain.score))+geom_boxplot(aes(fill=Status2))+labs(title="Pain Score Before Study Drug Administration", x="Responder Status", y="Pain Score")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.1.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="COX-1 Activity:  Post-Surgery 1", x="Responder Status", y="COX-1 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.2.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="COX-2 Activity:  Post-Surgery 1", x="Responder Status", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.2.basal))+geom_boxplot(aes(fill=Status2))+labs(title="COX-2 Activity:  Baseline", x="Responder Status", y=expression(PGE[2] (ng/ml)))+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.1.basal))+geom_boxplot(aes(fill=Status2))+labs(title="COX-1 Activity:  Baseline", x="Responder Status", y=expression(TxB[2] (ng/ml)))+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_noAPAP1, aes(Status2, COX.2.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="COX-2 Activity:  Post-Surgery 1 \nNo APAP", x="Responder Status", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.1.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="COX-1 Activity:  Post-Surgery 2", x="Responder Status", y="COX-1 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, COX.2.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="COX-2 Activity:  Post-Surgery 2", x="Responder Status", y="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, deltaTdrug.rescue))+geom_boxplot(aes(fill=Status2))+labs(title="Time to Rescue", x="Responder Status", y="Hours")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+annotate("text", x=3,y=2.75,label="No rescue medication")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, Ibuprofen.PS1))+geom_boxplot(aes(fill=Status2))+labs(title="Ibuprofen Plasma Concentrations:\nPost-Surgery1", x="Responder Status", y="Ibuprofen Plasma Concentrations (micromolar)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, (Ibuprofen.PS1/Weight)))+geom_boxplot(aes(fill=Status2))+labs(title="Weight-Normalized Ibuprofen Plasma Concentrations:\nPost-Surgery1", x="Responder Status", y="Ibuprofen Plasma Concentrations (micromolar/kg)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, half.life))+geom_boxplot(aes(fill=Status2))+labs(title="Ibuprofen Half-Life", x="Responder Status", y="Ibuprofen Half-Life (h)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGEM.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PGEM:  Post-Surgery 1", x="Responder Status", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGEM.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PGEM:  Post-Surgery 2", x="Responder Status", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGIM.PS1.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PGIM:  Post-Surgery 1", x="Responder Status", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGIM.PS2.percent))+geom_boxplot(aes(fill=Status2))+labs(title="PGIM:  Post-Surgery 2", x="Responder Status", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGEM.basal))+geom_boxplot(aes(fill=Status2))+labs(title="PGEM:  Baseline", x="Responder Status", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGIM.basal))+geom_boxplot(aes(fill=Status2))+labs(title="PGIM:  Baseline", x="Responder Status", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")+geom_jitter(width=0.2)
ggplot(data_wide, aes(Status2, PGEM.PS1.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM by Gender:  Post-Surgery 1", x="Responder Status", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")
ggplot(data_wide, aes(Status2, PGEM.PS2.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM by Gender:  Post-Surgery 2", x="Responder Status", y="Urinary PGEM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")
ggplot(data_wide, aes(Status2, PGIM.PS1.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM by Gender:  Post-Surgery 1", x="Responder Status", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")
ggplot(data_wide, aes(Status2, PGIM.PS2.percent))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM by Gender:  Post-Surgery 2", x="Responder Status", y="Urinary PGIM (%baseline)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")
ggplot(data_wide, aes(Status2, PGEM.basal))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGEM by Gender:  Baseline", x="Responder Status", y="Urinary PGEM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")
ggplot(data_wide, aes(Status2, PGIM.basal))+geom_boxplot(aes(fill=Demo_Gender))+labs(title="PGIM by Gender:  Baseline", x="Responder Status", y="Urinary PGIM (ng/mg creatinine)")+pub_specs+theme(legend.position="none")+scale_fill_brewer(palette="Set1")

ggplot(data_noAPAP1, aes(COX.2.PS1.percent, PS1.Pain))+geom_point(aes(color=Status2,size=3))+labs(title="Pain Score vs. COX-2 Activity:  Post-Surgery 1 \nNo APAP", y="Pain Score", x="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(data_i, aes(Ibuprofen.PS1, PS1.Pain))+geom_point(aes(color=Status2,size=3))+labs(title="Pain Score vs. Ibuprofen Levels:  Post-Surgery 1 \nNo APAP", y="Pain Score", x="Ibuprofen Plasma Concentration (micromolar)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(data_noAPAP1, aes(COX.2.PS1.percent, Pain.relief.score))+geom_point(aes(color=Status2,size=3))+labs(title="Pain Relief Score vs. COX-2 Activity:  Post-Surgery 1 \nNo APAP", y="Pain Relief Score", x="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(data_noAPAP1, aes(COX.2.PS1.percent, (PS1.Pain*100/End1.pain.score)))+geom_point(aes(color=Status2,size=3))+labs(title="Pain Score vs. COX-2 Activity:  Post-Surgery 1 \nNo APAP", y="Pain Score (% pre-medication pain score)", x="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(data_noAPAP1, aes(COX.2.PS1.percent, ((End1.pain.score-PS1.Pain)*100/End1.pain.score)))+geom_point(aes(color=Status2,size=3))+labs(title="Pain Relief vs. COX-2 Activity:  Post-Surgery 1 \nNo APAP", y="Pain Relief (% change pre-medication pain score)", x="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")
ggplot(data_i, aes(COX.2.PS1.percent, (PS1.Pain*100/End1.pain.score)))+geom_point(aes(color=Status2,size=3))+labs(title="Pain Score vs. COX-2 Activity:  Post-Surgery 1 \nNo APAP", y="Pain Score (% pre-medication pain score)", x="COX-2 Activity (%baseline)")+pub_specs+theme(legend.position="none")+scale_color_brewer(palette="Set1")

##Survival curve##
# Fit survival curves
require("survival")
fit<- survfit(Surv(End.Time) ~ Status2, data = data_wide)
fit2<- survfit(Surv(End.Time2) ~ Status2, data = data_wide)
# Drawing survival curves
ggsurvplot(fit,main="Time to Rescue or Study Discharge", xlab="Time (h)",palette="Set1",legend = "bottom", legend.title = "Responder Status", legend.labs = c("Placebo", "Non-Responder","Responder"))
ggsurvplot(fit2,main="Time to Rescue", xlab="Time (h)",palette="Set1",legend = "bottom", legend.title = "Responder Status", legend.labs = c("Placebo", "Non-Responder","Responder"))

dev.off()

ggplot(data_wide,aes(COX.1.PS1.percent,Pain.relief.score, color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(x="COX-1 Activity (%baseline)", y="Pain Relief Score", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(COX.2.PS1.percent,Pain.relief.score, color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Pain Relief Score", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_wide,aes(COX.1.PS2.percent,Pain.relief.score, color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(x="COX-1 Activity (%baseline)", y="Pain Relief Score", title="Post-Surgery 2")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(COX.2.PS2.percent,Pain.relief.score, color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Pain Relief Score", title="Post-Surgery 2")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_wide,aes(PTGS1.basal,COX.1.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="COX-1 Activity", x="PTGS1 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PTGS1.basal,COX.2.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="COX-2 Activity", x="PTGS1 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PTGS1.basal,PGEM.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="Urinary PGEM (ng/mg creatinine)", x="PTGS1 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PTGS1.basal,PGIM.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="Urinary PGIM (ng/mg creatinine)", x="PTGS1 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PTGS1.basal,PGDM.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="Urinary PGDM (ng/mg creatinine)", x="PTGS1 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PTGS1.basal,TxM.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="Urinary TxM (ng/mg creatinine)", x="PTGS1 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_wide,aes(PTGS2.basal,COX.1.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="COX-1 Activity", x="PTGS2 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PTGS2.basal,COX.2.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="COX-2 Activity", x="PTGS2 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PTGS2.basal,PGEM.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="Urinary PGEM (ng/mg creatinine)", x="PTGS2 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PTGS2.basal,PGIM.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="Urinary PGIM (ng/mg creatinine)", x="PTGS2 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PTGS2.basal,PGDM.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="Urinary PGDM (ng/mg creatinine)", x="PTGS2 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PTGS2.basal,TxM.basal, color=Status))+geom_point(size=3)+pub_specs + labs(y="Urinary TxM (ng/mg creatinine)", x="PTGS2 Expression", title="Baseline")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_wide,aes(COX.2.PS1.percent,Insula, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Relative CBF", title="Insula")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(COX.2.PS1.percent,S1, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Relative CBF", title="S1")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(COX.2.PS1.percent,S2, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Relative CBF", title="S2")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(COX.2.PS1.percent,ACC, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Relative CBF", title="ACC")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(COX.2.PS1.percent,Amygdala, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Relative CBF", title="Amygdala")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(COX.2.PS1.percent,Thalamus, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Relative CBF", title="Thalamus")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(COX.2.PS1.percent,Hippocampus, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Relative CBF", title="Hippocampus")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(COX.2.PS1.percent,Parahippocampus, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Relative CBF", title="Parahippocampus")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_wide,aes(Pain.relief.score,Insula, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="Pain Relief Score", y="Relative CBF", title="Insula")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(Pain.relief.score,S1, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="Pain Relief Score", y="Relative CBF", title="S1")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(Pain.relief.score,S2, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="Pain Relief Score)", y="Relative CBF", title="S2")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(Pain.relief.score,ACC, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="Pain Relief Score", y="Relative CBF", title="ACC")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(Pain.relief.score,Amygdala, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="Pain Relief Score", y="Relative CBF", title="Amygdala")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(Pain.relief.score,Thalamus, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="Pain Relief Score", y="Relative CBF", title="Thalamus")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(Pain.relief.score,Hippocampus, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="Pain Relief Score", y="Relative CBF", title="Hippocampus")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(Pain.relief.score,Parahippocampus, color=Status))+geom_point(size=3)+geom_hline(yintercept=1)+pub_specs + labs(x="Pain Relief Score", y="Relative CBF", title="Parahippocampus")+theme(legend.position="bottom",legend.box="horizontal")


ggplot(data_i,aes(COX.1.PS1.percent,Pain.relief.score, color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(x="COX-1 Activity (%baseline)", y="Pain Relief Score", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_i,aes(COX.2.PS1.percent,Pain.relief.score, color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Pain Relief Score", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_i,aes(Ibuprofen.PS1,Pain.relief.score, color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(x="Ibuprofen Plasma Concentration (micromolar)", y="Pain Relief Score", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_i,aes(COX.1.PS2.percent,Pain.relief.score, color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(x="COX-1 Activity (%baseline)", y="Pain Relief Score", title="Post-Surgery 2")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_i,aes(COX.2.PS2.percent,Pain.relief.score, color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(x="COX-2 Activity (%baseline)", y="Pain Relief Score", title="Post-Surgery 2")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_i,aes(Ibuprofen.PS2,Pain.relief.score, color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(x="Ibuprofen Plasma Concentration (micromolar)", y="Pain Relief Score", title="Post-Surgery 2")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_i,aes(Ibuprofen.PS1,COX.1.PS1, color=Status))+geom_point(size=3)+pub_specs + labs(x="Ibuprofen Plasma Concentration (micromolar)", y="COX-1 Activity(%baseline)", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_i,aes(Ibuprofen.PS1,COX.2.PS1, color=Status))+geom_point(size=3)+pub_specs + labs(x="Ibuprofen Plasma Concentration (micromolar)", y="COX-2 Activity(%baseline)", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_i,aes(Ibuprofen.PS2,COX.1.PS2, color=Status))+geom_point(size=3)+pub_specs + labs(x="Ibuprofen Plasma Concentration (micromolar)", y="COX-1 Activity(%baseline)", title="Post-Surgery 2")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_i,aes(Ibuprofen.PS2,COX.2.PS2, color=Status))+geom_point(size=3)+pub_specs + labs(x="Ibuprofen Plasma Concentration (micromolar)", y="COX-2 Activity(%baseline)", title="Post-Surgery 2")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_long,aes(Time,COX.1.PC, color=Status2, group=Subject))+geom_point(size=3)+geom_line()+geom_vline(xintercept=0)+pub_specs + labs(y="COX-1 Activity (%baseline)", x="Time Relative to Study Drug Treatment (hours)", title="COX-1 Activity")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_long,aes(Time,COX.2.PC, color=Status2, group=Subject))+geom_point(size=3)+geom_line()+geom_vline(xintercept=0)+pub_specs + labs(y="COX-2 Activity (%baseline)", x="Time Relative to Study Drug Treatment (hours)", title="COX-2 Activity")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_long,aes(Time,PGEM.PC, color=Status2, group=Subject))+geom_point(size=3)+geom_line()+geom_vline(xintercept=0)+pub_specs + labs(y="Urinary PGEM (%baseline)", x="Time Relative to Study Drug Treatment (hours)", title="PGEM")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_long,aes(Time,PGIM.PC, color=Status2, group=Subject))+geom_point(size=3)+geom_line()+geom_vline(xintercept=0)+pub_specs + labs(y="Urinary PGIM (%baseline)", x="Time Relative to Study Drug Treatment (hours)", title="PGIM")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_long,aes(Time,PGDM.PC, color=Status2, group=Subject))+geom_point(size=3)+geom_line()+geom_vline(xintercept=0)+pub_specs + labs(y="Urinary PGDM (%baseline)", x="Time Relative to Study Drug Treatment (hours)", title="PGDM")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_long,aes(Time,TxM.PC, color=Status2, group=Subject))+geom_point(size=3)+geom_line()+geom_vline(xintercept=0)+pub_specs + labs(y="Urinary TxM (%baseline)", x="Time Relative to Study Drug Treatment (hours)", title="TxM")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_long_i,aes(Time,Ibuprofen, color=Status2, group=Subject))+geom_point(size=3)+geom_line()+pub_specs + labs(y="Ibuprofen Plasma Concentration (micromolar)", x="Time Relative to Study Drug Treatment (hours)", title="Ibuprofen")+theme(legend.position="bottom",legend.box="horizontal")+xlim(0,5)

dev.off()

##PK modeling

data_ibu_long=subset(data_long, Group=="Ibuprofen")
data_ibu_long$Ibuprofen_mgL=data_ibu_long$Ibuprofen*0.20629
data_ibu_long$Weight=data_ibu_long$Demo_Wt/2.2
data_ibu_long$Height=data_ibu_long$Height_Inch*0.0254



library(nlme)
data_ibu=groupedData(data=data_ibu_long, formula=Ibuprofen_mgL~Time|Subject)

Ibu=nlme(Ibuprofen_mgL~SSfol(Dose,Time,lKe,lKa,lCl),
	data=data_ibu,
	fixed=lKe+lKa+lCl~1,
	random=pdDiag(lKe+lKa+lCl~1),
	na.action=na.pass,
	control=list(maxiter=500, pnlsTol=1e-6, tolerance=2e-1))

Ibu=nlme(Ibuprofen_mgL~((400/(exp(lV)))*exp(-exp(lCl-lV)*Time)),
	data=data_ibu,
	fixed=lV+lCl~1,
	random=pdDiag(lV+lCl~1),
	start=c(lCl=-1,lV=20),
	na.action=na.exclude,
	verbose=TRUE,
	control=list(maxiter=500, pnlsTol=1e-5, tolerance=2e-3))

Ibu=nlme(Ibuprofen_mgL~((400/V))*exp(-Cl*Time/V),
	data=data_ibu,
	fixed=V+Cl~1,
	random=V+Cl~1,
	start=c(Cl=4,V=10),
	na.action=na.exclude,
	verbose=TRUE,
	control=list(maxiter=500, pnlsTol=1e-5, tolerance=2e-5))

pheno=nlme(conc~phenoModel(Subject,time,dose,lCl,lV), data=data_ibu,fixed=lCl+lV~1,random=pdDiag(lCl+lV~1),start=c(-5,0),na.action=na.pass,naPattern=~!is.na(conc))


data_r_long=subset(data_long, Status2=="Responder")
ggplot(data_long,aes(Time, PGEM.PC,color=Status))+geom_smooth(aes(group=Status))+geom_point(size=3)+geom_vline(xintercept=0)+pub_specs + labs(x="Time relative to Study Medication (h)", y="Urinary PGEM(%baseline)", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_wide,aes(PGEM.PS1.percent,Pain.relief.score,color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(y="Pain Relief Score", x="Urinary PGEM(%baseline)", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(PGEM.PS1.percent,PS1.Pain,color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(y="Pain Relief Score", x="Urinary PGEM(%baseline)", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_wide,aes(COX.2.PS1.percent,PS1.pain.percent,color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(y="Pain Relief Score", x="Urinary PGEM(%baseline)", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_i,aes(COX.2.PS2.percent,PS2.pain.percent,color=Status))+geom_point(size=3)+geom_hline(yintercept=0)+pub_specs + labs(y="Pain Relief Score", x="Urinary PGEM(%baseline)", title="Post-Surgery 1")+theme(legend.position="bottom",legend.box="horizontal")













