##Read in data##
data_all=read.csv("CleanThrombosis_all.csv")
data_all$KO_Group=ifelse(data_all$Cre=="-", "control", as.character(data_all$TM_dose))
data_all$Mouse.rep=with(data_all, interaction(Mouse_ID,Rep))

write.csv(data_all,"originalPlus.csv")

library(reshape)
data_thrombosis2=melt(data_all,id.vars=c("Mouse_ID","Sex","DOB","Cre", "TM_dose","Treatment","Exp_Date", "KO_Group","Renal_medulla_Ptgs1", "Renal_medulla_Ptgs2", "Lung_Ptgs1", "Lung_Ptgs2", "Aorta_Ptgs1", "Aorta_Ptgs2","Celecoxib_trough","PGEM_basal","PGDM_basal","PGIM_basal","TxM_basal", "PGEM_NSAID","PGDM_NSAID","PGIM_NSAID","TxM_NSAID","Rep", "Mouse.rep"))

write.csv (data_thrombosis2,"Melted.csv")
##Convert time variable to numeric##
data_thrombosis2$time=as.numeric (data_thrombosis2$variable)-1

data_thrombosis2$PGIM_foldchange=data_thrombosis2$PGIM_NSAID/data_thrombosis2$PGIM_basal
data_thrombosis2$TxM_foldchange=data_thrombosis2$TxM_NSAID/data_thrombosis2$TxM_basal
data_thrombosis2$PGEM_foldchange=data_thrombosis2$PGEM_NSAID/data_thrombosis2$PGEM_basal
data_thrombosis2$PGDM_foldchange=data_thrombosis2$PGDM_NSAID/data_thrombosis2$PGDM_basal
data_thrombosis2$Group=with(data_thrombosis2, interaction(KO_Group,Treatment))
library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))
ggplot(data_thrombosis2, aes(variable,value,color=Mouse_ID))+geom_point()+pub_specs + labs(x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")
ggplot(data_thrombosis2, aes(PGIM_NSAID,value,color=Mouse_ID))+geom_point()+pub_specs
ggplot(data_thrombosis2, aes(PGIM_foldchange,value,color=Mouse_ID))+geom_point()+pub_specs

ggplot(data_thrombosis2, aes(variable,value,color=Cre))+geom_point()+pub_specs
ggplot(data_thrombosis2, aes(time,value,color=Treatment))+geom_point()+pub_specs
ggplot(data_thrombosis2, aes(variable,value,color=Group))+geom_point()+pub_specs

ggplot(data_thrombosis2, aes(variable,value,color=Group))+geom_point()+pub_specs +scale_y_log10()

library(MESS)
library(dplyr)
by_rep=group_by(data_thrombosis2,Mouse.rep) 
AUC=summarize(by_rep,auc=auc(time,value))
agg.AUC=summarize(by_rep,agg.auc=auc(time,value, to=which.max(value)))
dis.AUC=summarize(by_rep,dis.auc=auc(time,value, from=which.max(value)))
TMAX=summarize(by_rep, tmax=which.max(value))
VMAX=summarize(by_rep, vmax=max(value))

data_1=merge(AUC, agg.AUC, all=TRUE)
data_2=merge(dis.AUC, TMAX, all=TRUE)
data_3=merge(data_1,VMAX,all=TRUE)
data_4=merge(data_3,data_2,all=TRUE)

data_thrombosis3=merge(data_4,data_all, all=TRUE)
data_thrombosis3$PGIM_foldchange=data_thrombosis3$PGIM_NSAID/data_thrombosis3$PGIM_basal
data_thrombosis3$TxM_foldchange=data_thrombosis3$TxM_NSAID/data_thrombosis3$TxM_basal
data_thrombosis3$PGEM_foldchange=data_thrombosis3$PGEM_NSAID/data_thrombosis3$PGEM_basal
data_thrombosis3$PGDM_foldchange=data_thrombosis3$PGDM_NSAID/data_thrombosis3$PGDM_basal
data_thrombosis3$Group=with(data_thrombosis3, interaction(Cre,Treatment))
data_thrombosis3$Kidney_tertile= with(data_thrombosis3, cut (Renal_medulla_Ptgs2, breaks=quantile(Renal_medulla_Ptgs2, probs=seq(0,1,by=1/3), na.rm=TRUE),include.lowest=TRUE))
data_thrombosis3$Lung_tertile= with(data_thrombosis3, cut (Lung_Ptgs2, breaks=quantile(Lung_Ptgs2, probs=seq(0,1,by=1/3), na.rm=TRUE),include.lowest=TRUE))
data_thrombosis3$Aorta_tertile= with(data_thrombosis3, cut (Aorta_Ptgs2, breaks=quantile(Aorta_Ptgs2, probs=seq(0,1,by=1/3), na.rm=TRUE),include.lowest=TRUE))
data_thrombosis3$KO_Group=ifelse(data_thrombosis3$Cre=="-", "control", as.character(data_thrombosis3$TM_dose))
data_thrombosis3$Group=with(data_thrombosis3, interaction(KO_Group,Treatment))

Q1=function(x){
	quantile(x,0.25, na.rm=TRUE)}
Q3=function(x){
	quantile(x,0.75, na.rm=TRUE)}


ggplot(data_thrombosis3, aes(Group, vmax, color=Mouse_ID))+geom_point()+pub_specs+theme(legend.position="none")+scale_y_log10()
ggplot(data_thrombosis3, aes(Group, tmax))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Group, auc))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Group, agg.auc))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Kidney_tertile, auc, color=Group))+geom_boxplot()+pub_specs+scale_y_log10()
ggplot(data_thrombosis3, aes(PGIM_NSAID, tmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGIM_NSAID, vmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGIM_NSAID, auc, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGIM_foldchange, tmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGIM_foldchange, vmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGIM_foldchange, auc, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(TxM_NSAID, tmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(TxM_NSAID, vmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(TxM_NSAID, auc, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(TxM_foldchange, tmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(TxM_foldchange, vmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(TxM_foldchange, auc, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGEM_NSAID, tmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGEM_NSAID, vmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGEM_NSAID, auc, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGEM_foldchange, tmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGEM_foldchange, vmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGEM_foldchange, auc, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGDM_NSAID, tmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGDM_NSAID, vmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGDM_NSAID, auc, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGDM_foldchange, tmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGDM_foldchange, vmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(PGDM_foldchange, auc, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Celecoxib_trough, tmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Celecoxib_trough, vmax, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Celecoxib_trough, auc, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Celecoxib_trough, agg.auc, color=Group))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Celecoxib_trough, dis.auc, color=Group))+geom_point()+pub_specs

data_324.3=data_thrombosis2[which(data_thrombosis2$Mouse_ID=="324-3"),]
data_328.2=data_thrombosis2[which(data_thrombosis2$Mouse_ID=="328-2"),]

data_320.4=data_thrombosis2[which(data_thrombosis2$Mouse_ID=="320-4"),]
data_326.1=data_thrombosis2[which(data_thrombosis2$Mouse_ID=="326-1"),]

data_328.3=data_thrombosis2[which(data_thrombosis2$Mouse_ID=="328-3"),]
data_320.3=data_thrombosis2[which(data_thrombosis2$Mouse_ID=="320-3"),]

pdf("DC_Thrombosis_individual_graphs.pdf")

ggplot(data_thrombosis3, aes(Mouse_ID, vmax, color=Treatment, fill=KO_Group))+geom_boxplot(alpha=0.5)+pub_specs + labs(x="Mouse", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"), fill=guide_legend("KO Group"))+theme(legend.position="bottom",legend.box="horizontal")+geom_jitter()+coord_fixed(2/10e5)

ggplot(data_324.3, aes(time,value,color=Rep))+geom_point()+pub_specs + labs(title="Full KO: 324-3",x="Time (msec)", y="Platelet Aggregation\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=2/10000)+theme(legend.position="none")
ggplot(data_328.2, aes(time,value,color=Rep))+geom_point()+pub_specs + labs(title="Full KO: 328-2",x="Time (msec)", y="Platelet Aggregation\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=3/1000)+theme(legend.position="none")

ggplot(data_320.3, aes(time,value,color=Rep))+geom_point()+pub_specs + labs(title="Control: 320-3",x="Time (msec)", y="Platelet Aggregation\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=7.5/10000)+theme(legend.position="none")
ggplot(data_328.3, aes(time,value,color=Rep))+geom_point()+pub_specs + labs(title="Control: 328-3",x="Time (msec)", y="Platelet Aggregation\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=1.5/1000)+theme(legend.position="none")

ggplot(data_320.4, aes(time,value,color=Rep))+geom_point()+pub_specs + labs(title="Partial KO: 320-4",x="Time (msec)", y="Platelet Aggregation\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=2.75/10000)+theme(legend.position="none")
ggplot(data_326.1, aes(time,value,color=Rep))+geom_point()+pub_specs + labs(title="Partial KO: 326-1",x="Time (msec)", y="Platelet Aggregation\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=5/10000)+theme(legend.position="none")


ggplot(data_thrombosis3, aes(Celecoxib_trough, vmax, color=KO_Group))+geom_point(size=3)+pub_specs + labs(x="Celecoxib Plasma Concentration (micromolar)", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+scale_x_log10()+ guides(colour=guide_legend("KO Group"))+theme(legend.position="bottom",legend.box="horizontal")+coord_fixed(5/10e6)

dev.off()

ggplot(data_thrombosis3, aes(PGIM_foldchange, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(x="Urinary PGIM\n(fold change relative to baseline)", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(PGIM_foldchange, vmax, color=KO_Group))+geom_point(size=3)+pub_specs + labs(x="Urinary PGIM\n(fold change relative to baseline)", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("KO Group"))+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_thrombosis3, aes(PGIM_NSAID, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(x="Urinary PGIM\n(ng/mg creatinine)", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=12/10e5)+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(PGIM_NSAID, vmax, color=KO_Group))+geom_point(size=3)+pub_specs + labs(x="Urinary PGIM\n(ng/mg creatinine)", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("KO Group"))+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, auc, color=Treatment))+scale_y_log10()+geom_point(size=3)+pub_specs + labs(title="Renal Medulla",x="Renal Medulla Ptgs2 Expression", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+geom_smooth()
ggplot(data_thrombosis3, aes(Lung_Ptgs2, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Lung",x="Lung Ptgs2 Expression", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+geom_smooth()
ggplot(data_thrombosis3, aes(Aorta_Ptgs2, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Aorta",x="Aorta Ptgs2 Expression", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+geom_smooth()

ggplot(data_thrombosis3, aes(Kidney_tertile, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Renal Medulla",x="Renal Medulla Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(Lung_tertile, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Lung",x="Lung Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(Aorta_tertile, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Aorta",x="Aorta Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_thrombosis3, aes(Kidney_tertile, vmax, color=Treatment))+geom_boxplot()+pub_specs + labs(title="Renal Medulla",x="Renal Medulla Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(Lung_tertile, vmax, color=Treatment))+geom_boxplot()+pub_specs + labs(title="Lung",x="Lung Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(Aorta_tertile, vmax, color=Treatment))+geom_boxplot()+pub_specs + labs(title="Aorta",x="Aorta Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")


ggplot(data_thrombosis3, aes(KO_Group, auc, color=Treatment))+scale_y_log10()+geom_boxplot()+pub_specs + labs(x="KO Group", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")

pdf("Lab_meeting_4202017.pdf")

ggplot(data_thrombosis2, aes(time, value, group=Group,color=Group))+scale_y_log10()+labs(title="Platelet Aggregation", x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")+pub_specs+theme(legend.position="bottom")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Group), alpha=0.1)
ggplot(data_thrombosis2, aes(time, value, group=Treatment,color=Treatment))+scale_y_log10()+labs(title="Platelet Aggregation by Treatment", x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")+pub_specs+theme(legend.position="bottom")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Treatment), alpha=0.1)
ggplot(data_thrombosis2[which(data_thrombosis2$Treatment=="control"),], aes(time, value, group=KO_Group,color=KO_Group))+scale_y_log10()+labs(title="Platelet Aggregation (Control Diet)", x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")+pub_specs+theme(legend.position="bottom")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=KO_Group), alpha=0.1)
ggplot(data_thrombosis2[which(data_thrombosis2$Treatment=="celecoxib"),], aes(time, value, group=KO_Group,color=KO_Group))+scale_y_log10()+labs(title="Platelet Aggregation (Celecoxib Diet)", x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")+pub_specs+theme(legend.position="bottom")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=KO_Group), alpha=0.1)
ggplot(data_thrombosis2[which(data_thrombosis2$KO_Group=="control"),], aes(time, value, group=Treatment,color=Treatment))+scale_y_log10()+labs(title="Platelet Aggregation (Cre- Mice)", x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")+pub_specs+theme(legend.position="bottom")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Treatment), alpha=0.1)
ggplot(data_thrombosis2[which(data_thrombosis2$KO_Group=="20x3"),], aes(time, value, group=Treatment,color=Treatment))+scale_y_log10()+labs(title="Platelet Aggregation (Partial KO Mice)", x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")+pub_specs+theme(legend.position="bottom")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Treatment), alpha=0.1)
ggplot(data_thrombosis2[which(data_thrombosis2$KO_Group=="100x5"),], aes(time, value, group=Treatment,color=Treatment))+scale_y_log10()+labs(title="Platelet Aggregation (Full KO Mice)", x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")+pub_specs+theme(legend.position="bottom")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Treatment), alpha=0.1)


ggplot(data_thrombosis3, aes(KO_Group, Renal_medulla_Ptgs2, fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="Renal Medulla",x="KO Group", y="Ptgs2/Gapdh")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_thrombosis3, aes(KO_Group, Lung_Ptgs2, fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="Lung",x="KO Group", y="Ptgs2/Gapdh")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")

ggplot(data_thrombosis2, aes(time, value, group=Treatment,color=Treatment))+scale_y_log10()+labs(title="Signal over time", x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")+pub_specs+theme(legend.position="bottom")+scale_fill_brewer(palet


data_mean_by_mouse=summarize(group_by(data_thrombosis3,Mouse_ID),Treatment=last(Treatment),Cre=last(Cre),Group=last(Group),KO_Group=last(KO_Group),injuries=length(auc), 
	vmax_mean=mean(vmax, na.rm=TRUE),tmax_mean=mean(tmax, na.rm=TRUE),auc_mean=mean(auc, na.rm=TRUE),dis.auc_mean=mean(dis.auc, na.rm=TRUE),agg.auc_mean=mean(agg.auc, na.rm=TRUE),
	vmax_median=median(vmax, na.rm=TRUE),tmax_median=median(tmax, na.rm=TRUE),auc_median=median(auc, na.rm=TRUE),dis.auc_median=median(dis.auc, na.rm=TRUE),agg.auc_median=median(agg.auc, na.rm=TRUE), 
	Renal_medulla_Ptgs1=mean(Renal_medulla_Ptgs1, na.rm=TRUE),Renal_medulla_Ptgs2=mean(Renal_medulla_Ptgs2, na.rm=TRUE),Lung_Ptgs1=mean(Lung_Ptgs1, na.rm=TRUE),Lung_Ptgs2=mean(Lung_Ptgs2, na.rm=TRUE),
	PGIM_NSAID=mean(PGIM_NSAID, na.rm=TRUE),PGEM_NSAID=mean(PGEM_NSAID, na.rm=TRUE),PGDM_NSAID=mean(PGDM_NSAID, na.rm=TRUE),TxM_NSAID=mean(TxM_NSAID, na.rm=TRUE),PGIM_foldchange=mean(PGIM_foldchange, na.rm=TRUE), 
	PGEM_foldchange=mean(PGEM_foldchange, na.rm=TRUE),PGDM_foldchange=mean(PGDM_foldchange, na.rm=TRUE),TxM_foldchange=mean(TxM_foldchange, na.rm=TRUE),Celecoxib_trough=mean(Celecoxib_trough, na.rm=TRUE))


data_by_mouse=as.data.frame(data_mean_by_mouse)

data_mean_by_group=summarize(group_by(data_thrombosis3,Group),Treatment=last(Treatment),Cre=last(Cre),KO_Group=last(KO_Group),injuries=length(auc), 
	vmax_mean=mean(vmax, na.rm=TRUE),tmax_mean=mean(tmax, na.rm=TRUE),auc_mean=mean(auc, na.rm=TRUE),dis.auc_mean=mean(dis.auc, na.rm=TRUE),agg.auc_mean=mean(agg.auc, na.rm=TRUE),
	vmax_median=median(vmax, na.rm=TRUE),tmax_median=median(tmax, na.rm=TRUE),auc_median=median(auc, na.rm=TRUE),dis.auc_median=median(dis.auc, na.rm=TRUE),agg.auc_median=median(agg.auc, na.rm=TRUE), 
	Renal_medulla_Ptgs1=mean(Renal_medulla_Ptgs1, na.rm=TRUE),Renal_medulla_Ptgs2=mean(Renal_medulla_Ptgs2, na.rm=TRUE),Lung_Ptgs1=mean(Lung_Ptgs1, na.rm=TRUE),Lung_Ptgs2=mean(Lung_Ptgs2, na.rm=TRUE),
	PGEM_b=mean(PGEM_basal, na.rm=TRUE),PGEM_n=mean(PGEM_NSAID,na.rm=TRUE),PGIM_b=mean(PGIM_basal, na.rm=TRUE),PGIM_n=mean(PGIM_NSAID,na.rm=TRUE),PGDM_b=mean(PGDM_basal, na.rm=TRUE),PGDM_n=mean(PGDM_NSAID,na.rm=TRUE),TxM_b=mean(TxM_basal, na.rm=TRUE),TxM_n=mean(TxM_NSAID,na.rm=TRUE), auc_sd=sd(auc, na.rm=TRUE),auc_Q1=Q1(auc),auc_Q3=Q3(auc))

write.csv(data_mean_by_group,"thrombosis_summary.csv")

data_mean_by_group2=summarize(group_by(data_thrombosis3,Group),Treatment=last(Treatment),Cre=last(Cre),KO_Group=last(KO_Group),injuries=length(auc), 
	auc_mean=mean(auc, na.rm=TRUE),auc_sd=sd(auc, na.rm=TRUE),auc_logmean=mean(log10(auc), na.rm=TRUE),auc_logsd=sd(log10(auc), na.rm=TRUE),auc_median=median(auc, na.rm=TRUE),auc_Q1=Q1(auc),auc_Q3=Q3(auc))

write.csv(data_mean_by_group2,"thrombosis_summary2.csv")

ggplot(data_by_mouse, aes(KO_Group, vmax_median,fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="Maximal Thrombus Size",x="Group", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(KO_Group, tmax_median, fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="Time to Maximal Thrombus",x="Group", y="Time to Maximal Thrombus\n(msec)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(KO_Group, auc_median, fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="AUC",x="Group", y="AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(KO_Group, dis.auc_median, fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="Disaggregation Phase AUC",x="Group", y="Disaggregation Phase AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(KO_Group, agg.auc_median, fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="Aggregation Phase AUC",x="Group", y="Aggregation Phase AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")

ggplot(data_thrombosis3, aes(KO_Group, vmax,fill=Treatment))+geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge")+pub_specs+scale_y_log10() + labs(title="Maximal Thrombus Size",x="Group", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_thrombosis3, aes(KO_Group, tmax, fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="Time to Maximal Thrombus",x="Group", y="Time to Maximal Thrombus\n(msec)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_thrombosis3, aes(KO_Group, auc, fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="AUC",x="Group", y="AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_thrombosis3, aes(KO_Group, dis.auc, fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="Disaggregation Phase AUC",x="Group", y="Disaggregation Phase AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_thrombosis3, aes(KO_Group, agg.auc, fill=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="Aggregation Phase AUC",x="Group", y="Aggregation Phase AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")

ggplot(data_by_mouse, aes(KO_Group, vmax_median,fill=Treatment))+geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge")+pub_specs+scale_y_log10() + labs(title="Maximal Thrombus Size",x="Group", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(KO_Group, tmax_median, fill=Treatment))+geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge")+pub_specs+scale_y_log10() + labs(title="Time to Maximal Thrombus",x="Group", y="Time to Maximal Thrombus\n(msec)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(KO_Group, auc_median, fill=Treatment))+geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge")+pub_specs+scale_y_log10() + labs(title="AUC",x="Group", y="AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(KO_Group, dis.auc_median, fill=Treatment))+geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge")+pub_specs+scale_y_log10() + labs(title="Disaggregation Phase AUC",x="Group", y="Disaggregation Phase AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(KO_Group, agg.auc_median, fill=Treatment))+geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge")+pub_specs+scale_y_log10() + labs(title="Aggregation Phase AUC",x="Group", y="Aggregation Phase AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")

ggplot(data_by_mouse, aes(Renal_medulla_Ptgs2, vmax_median, color=Treatment))+geom_point(size=3)+pub_specs+scale_y_log10() +scale_x_log10()+ labs(title="Maximal Thrombus Size",x="Renal Medulla Ptgs2/Gapdh", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(Renal_medulla_Ptgs2, tmax_median, color=Treatment))+geom_point(size=3)+pub_specs+scale_y_log10() +scale_x_log10()+ labs(title="Time to Maximal Thrombus",x="Renal Medulla Ptgs2/Gapdh", y="Time to Maximal Thrombus\n(msec)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(Renal_medulla_Ptgs2, auc_median, color=Treatment))+geom_point(size=3)+pub_specs+scale_y_log10() +scale_x_log10()+ labs(title="AUC",x="Renal Medulla Ptgs2/Gapdh", y="AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(Renal_medulla_Ptgs2, dis.auc_median, color=Treatment))+geom_smooth()+geom_point(size=3)+pub_specs+scale_y_log10() +scale_x_log10()+ labs(title="Disaggregation Phase AUC",x="Renal Medulla Ptgs2/Gapdh", y="Disaggregation Phase AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(Renal_medulla_Ptgs2, agg.auc_median, color=Treatment))+geom_point(size=3)+pub_specs+scale_y_log10() +scale_x_log10()+ labs(title="Aggregation Phase AUC",x="Renal Medulla Ptgs2/Gapdh", y="Aggregation Phase AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")

ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, vmax, color=Treatment))+geom_point()+pub_specs+scale_y_log10() +scale_x_log10()+ labs(title="Maximal Thrombus Size",x="Renal Medulla Ptgs2/Gapdh", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, tmax, color=Treatment))+geom_point()+pub_specs+scale_y_log10() +scale_x_log10()+ labs(title="Time to Maximal Thrombus",x="Renal Medulla Ptgs2/Gapdh", y="Time to Maximal Thrombus\n(msec)")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, auc, color=Treatment))+geom_point()+pub_specs+scale_y_log10() +scale_x_log10()+ labs(title="AUC",x="Renal Medulla Ptgs2/Gapdh", y="AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, dis.auc, color=Treatment))+geom_smooth()+geom_point()+pub_specs+scale_y_log10() +scale_x_log10()+ labs(title="Disaggregation Phase AUC",x="Renal Medulla Ptgs2/Gapdh", y="Disaggregation Phase AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, agg.auc, color=Treatment))+geom_point()+pub_specs+scale_y_log10() +scale_x_log10()+ labs(title="Aggregation Phase AUC",x="Renal Medulla Ptgs2/Gapdh", y="Aggregation Phase AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")


dev.off()

wilcox.test(dis.auc_median~Treatment,data=data_by_mouse[which(data_by_mouse$KO_Group=="control"),])
t.test(dis.auc_median~Treatment,data=data_by_mouse[which(data_by_mouse$KO_Group=="control"),])

summary(wilcox.test(dis.auc~Treatment,data=data_thrombosis3[which(data_thrombosis3$KO_Group=="control"),]))
t.test(log(dis.auc)~Treatment,data=data_thrombosis3[which(data_thrombosis3$KO_Group=="control"),])

data_by_mouse$Renal_Ptgs2_norm=data_by_mouse$Renal_medulla_Ptgs2/as.numeric(mean(subset(data_by_mouse$Renal_medulla_Ptgs2, data_by_mouse$KO_Group=="control")))
data_by_mouse$Lung_Ptgs2_norm=data_by_mouse$Lung_Ptgs2/as.numeric(mean(subset(data_by_mouse$Lung_Ptgs2, data_by_mouse$KO_Group=="control")))

ggplot(data_by_mouse, aes(Renal_medulla_Ptgs1, dis.auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()
ggplot(data_by_mouse, aes(Renal_medulla_Ptgs2, dis.auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()

ggplot(data_by_mouse, aes(Lung_Ptgs1, dis.auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()
ggplot(data_by_mouse, aes(Lung_Ptgs2, dis.auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()


ggplot(data_by_mouse, aes(Renal_medulla_Ptgs1, vmax_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()
ggplot(data_by_mouse, aes(Renal_medulla_Ptgs2, vmax_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()

ggplot(data_by_mouse, aes(Lung_Ptgs1, vmax_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()
ggplot(data_by_mouse, aes(Lung_Ptgs2, vmax_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()

ggplot(data_by_mouse, aes(Renal_medulla_Ptgs1, tmax_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()
ggplot(data_by_mouse, aes(Renal_medulla_Ptgs2, tmax_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()

ggplot(data_by_mouse, aes(Lung_Ptgs1, tmax_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()
ggplot(data_by_mouse, aes(Lung_Ptgs2, tmax_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()

ggplot(data_by_mouse, aes(Renal_medulla_Ptgs1, auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()
ggplot(data_by_mouse, aes(Renal_medulla_Ptgs2, auc_median, color=Treatment))+geom_point()+pub_specs+ labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()

ggplot(data_by_mouse, aes(Lung_Ptgs1, auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()
ggplot(data_by_mouse, aes(Lung_Ptgs2, auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()

ggplot(data_by_mouse, aes(Renal_medulla_Ptgs1, agg.auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()
ggplot(data_by_mouse[which(data_by_mouse$KO_Group=="control"),], aes(Renal_Ptgs2_norm, dis.auc_mean, color=Treatment))+geom_point()+pub_specs+ labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()

ggplot(data_by_mouse, aes(Lung_Ptgs1, agg.auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()
ggplot(data_by_mouse, aes(Lung_Ptgs2_norm, agg.auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()

ggplot(data_by_mouse, aes(KO_Group, vmax_median, color=Treatment))+geom_boxplot()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs1/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")
ggplot(data_by_mouse, aes(Renal_medulla_Ptgs2, dis.auc_median, color=Treatment))+geom_point()+pub_specs+scale_y_log10() + labs(title="Expression by disaggregation",x="Ptgs2/Gapdh", y="Disaggregation AUC")+ guides(colour=guide_legend("Treatment"))+theme(legend.position="bottom",legend.box="horizontal")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+geom_smooth()



library(nlme)
data_all=groupedData(data=data_thrombosis2, formula=value~time|Group/Mouse_ID)

data_summary=groupedData(data=data_thrombosis3, formula=auc~Renal_medulla_Ptgs2|Group/Mouse_ID)
data_summary_con=groupedData(data=data_thrombosis3[which(data_thrombosis3$KO_Group=="control"),], formula=auc~Renal_medulla_Ptgs2|Mouse_ID)


auc.aov=anova(lm(log(auc)~Treatment*Renal_medulla_Ptgs2,data=data_thrombosis3[which(data_thrombosis3$KO_Group=="control"),]))

lmesettings=lmeControl(maxIter=500,msMaxIter=500)
base=nlme(value~SSfol(time,random=~1|Mouse_ID/Rep,data=data_all, na.action=na.exclude,control=lmesettings)

base=nlsList(value~SSfol(1,time,lKdis,lKagg,lCl)|Group,data=data_all, na.action=na.exclude,control=list(maxiter=500, tol=0.15, minFactor = 1/10000000000))


base_nlme=nlme(base,verbose=TRUE,control=list(maxIter=500, pnlsTol=1e-20, pnlsMaxIter=100,tolerance=0.15))

auc=lme(log(auc)~Treatment*KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
dis.auc=lme(log(dis.auc)~Treatment*KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
agg.auc=lme(agg.auc~Treatment*KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
vmax=lme(vmax~Treatment*KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
tmax=lme(tmax~Treatment*KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)

auc=lme(auc~KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
dis.auc=lme(log(dis.auc)~KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
agg.auc=lme(agg.auc~Treatment*KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
vmax=lme(vmax~Treatment*KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
tmax=lme(tmax~Treatment*KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)

auc.con=lme(log(auc)~Treatment*Renal_medulla_Ptgs2,random=~1|Mouse_ID,data=data_summary_con, na.action=na.exclude,control=lmesettings)
dis.auc.con=lme(log(dis.auc)~Treatment*Renal_medulla_Ptgs2,random=~1|Mouse_ID,data=data_summary_con, na.action=na.exclude,control=lmesettings)
agg.auc.con=lme(agg.auc~Treatment,random=~1|Mouse_ID,data=data_summary_con, na.action=na.exclude,control=lmesettings)
vmax.con=lme(vmax~Treatment,random=~1|Mouse_ID,data=data_summary_con, na.action=na.exclude,control=lmesettings)
tmax.con=lme(tmax~Treatment,random=~1|Mouse_ID,data=data_summary_con, na.action=na.exclude,control=lmesettings)

ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, dis.auc, group=Treatment,color=Treatment))+scale_y_log10()+labs(title="Signal by expression", x="Medulla Ptgs2", y="FITC")+pub_specs+theme(legend.position="bottom")+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")+stat_summary(fun.y="median",fun.ymin="Q1",fun.ymax="Q3", size=2, geom="smooth", aes(fill=Treatment), alpha=0.1)

auc=lme(log(auc)~Treatment*KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
dis.auc=lme(log(dis.auc)~PGIM_foldchange+TxM_foldchange+Celecoxib_trough+PGEM_foldchange+PGDM_foldchange+Renal_medulla_Ptgs2+Lung_Ptgs2+Lung_Ptgs1+Renal_medulla_Ptgs1,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
agg.auc=lme(agg.auc~Treatment*KO_Group,random=~1|Mouse_ID,data=data_summary, na.action=na.exclude,control=lmesettings)
