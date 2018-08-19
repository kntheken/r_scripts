##Read in data##
data_all=read.csv("Thrombosis_all.csv")
data_all$KO_Group=ifelse(data_all$Cre=="-", "control", as.character(data_all$TM_dose))
data_all$Mouse.rep=with(data_all, interaction(Mouse_ID,Rep))

library(reshape)
data_thrombosis2=melt(data_all,id=c("Mouse_ID","Sex","DOB","Cre", "TM_dose","Treatment","Exp_Date", 
	"KO_Group","Renal_medulla_Ptgs1", "Renal_medulla_Ptgs2", "Lung_Ptgs1", "Lung_Ptgs2", "Aorta_Ptgs1", "Aorta_Ptgs2","Celecoxib_trough","PGEM_basal","PGDM_basal","PGIM_basal","TxM_basal", "PGEM_NSAID","PGDM_NSAID","PGIM_NSAID","TxM_NSAID", "Rep", "Mouse.rep"), na.rm=TRUE)

##Convert time variable to numeric##
data_thrombosis2$variable=as.numeric (data_thrombosis2$variable)-1
data_thrombosis2$value=as.numeric (levels(data_thrombosis2$value))[data_thrombosis2$value]

data_thrombosis2$PGIM_foldchange=data_thrombosis2$PGIM_NSAID/data_thrombosis2$PGIM_basal
data_thrombosis2$TxM_foldchange=data_thrombosis2$TxM_NSAID/data_thrombosis2$TxM_basal
data_thrombosis2$PGEM_foldchange=data_thrombosis2$PGEM_NSAID/data_thrombosis2$PGEM_basal
data_thrombosis2$PGDM_foldchange=data_thrombosis2$PGDM_NSAID/data_thrombosis2$PGDM_basal
data_thrombosis2$Group=with(data_thrombosis2, interaction(Cre,Treatment))
library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),
 axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))
ggplot(data_thrombosis2, aes(variable,value,color=Mouse_ID))+geom_point()+pub_specs + labs(x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")
ggplot(data_thrombosis2, aes(PGIM_NSAID,value,color=Mouse_ID))+geom_point()+pub_specs
ggplot(data_thrombosis2, aes(PGIM_foldchange,value,color=Mouse_ID))+geom_point()+pub_specs

ggplot(data_thrombosis2, aes(variable,value,color=Cre))+geom_point()+pub_specs
ggplot(data_thrombosis2, aes(variable,value,color=Treatment))+geom_point()+pub_specs
ggplot(data_thrombosis2, aes(variable,value,color=Group))+geom_point()+pub_specs

ggplot(data_thrombosis2, aes(variable,value,color=Group))+geom_point()+pub_specs +scale_y_log10()

data_thrombosis2$vmax=tapply(data_thrombosis2$value,data_thrombosis2$Mouse.rep, FUN=max, na.rm=TRUE)
data_thrombosis2$tmax=tapply(data_thrombosis2$value,data_thrombosis2$Mouse.rep, FUN=which.max)
library(MESS)
library(dplyr)
by_rep=group_by(data_thrombosis2,Mouse.rep) 
AUC=summarize(by_rep,auc=auc(variable,value))
agg.AUC=summarize(by_rep,agg.auc=auc(variable,value, to=tmax))
dis.AUC=summarize(by_rep,dis.auc=auc(variable,value, from=tmax))
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

ggplot(data_thrombosis3, aes(Group, vmax))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Group, tmax))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Group, auc))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Group, agg.auc))+geom_point()+pub_specs
ggplot(data_thrombosis3, aes(Group, dis.auc))+geom_point()+pub_specs
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

pdf("Study25_graphs.pdf")
ggplot(data_thrombosis2, aes(variable,value,color=Mouse_ID))+geom_point()+pub_specs + labs(x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")
ggplot(data_thrombosis2, aes(variable,value,color=Group))+geom_point()+pub_specs + labs(x="Time (msec)", y="Platelet Aggregation (FITC Fluorescence Intensity)")

ggplot(data_thrombosis3, aes(Group, auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Group", y="AUC")
ggplot(data_thrombosis3, aes(Group, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Group", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(Group, tmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Group", y="Time to Maximal Thrombus (msec)")
ggplot(data_thrombosis3, aes(Group, agg.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Group", y="Aggregation Phase (AUC)")
ggplot(data_thrombosis3, aes(Group, dis.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Group", y="Disaggregation Phase (AUC)")
ggplot(data_thrombosis3, aes(Group, PGIM_foldchange, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Group", y="PGIM (fold change from baseline)")
ggplot(data_thrombosis3, aes(Group, Renal_medulla_Ptgs2, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Group", y="Renal Medulla Ptgs2")
ggplot(data_thrombosis3, aes(Group, PGIM_NSAID, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Group", y="PGIM (ng/mg creatinine)")

ggplot(data_thrombosis3, aes(Kidney_tertile, auc, color=Treatment))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2 Expression Tertile", y="AUC")+coord_fixed(ratio=5/10e8)
ggplot(data_thrombosis3, aes(Kidney_tertile, vmax, color=Treatment))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=20/10e6)
ggplot(data_thrombosis3, aes(Kidney_tertile, agg.auc, color=Treatment))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2 Expression Tertile", y="Aggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)
ggplot(data_thrombosis3, aes(Kidney_tertile, dis.auc, color=Treatment))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2 Expression Tertile", y="Disaggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)


ggplot(data_thrombosis3, aes(Kidney_tertile, auc, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Renal Medulla Ptgs2 Expression Tertile", y="AUC")+coord_fixed(ratio=5/10e8)
ggplot(data_thrombosis3, aes(Kidney_tertile, vmax, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Renal Medulla Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=20/10e6)
ggplot(data_thrombosis3, aes(Kidney_tertile, agg.auc, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Renal Medulla Ptgs2 Expression Tertile", y="Aggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)
ggplot(data_thrombosis3, aes(Kidney_tertile, dis.auc, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Renal Medulla Ptgs2 Expression Tertile", y="Disaggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)

ggplot(data_thrombosis3, aes(Lung_tertile, auc, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Lung Ptgs2 Expression Tertile", y="AUC")+coord_fixed(ratio=5/10e8)
ggplot(data_thrombosis3, aes(Lung_tertile, vmax, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Lung Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=20/10e6)
ggplot(data_thrombosis3, aes(Lung_tertile, agg.auc, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Lung Ptgs2 Expression Tertile", y="Aggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)
ggplot(data_thrombosis3, aes(Lung_tertile, dis.auc, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Lung Ptgs2 Expression Tertile", y="Disaggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)

ggplot(data_thrombosis3, aes(Aorta_tertile, auc, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Aorta Ptgs2 Expression Tertile", y="AUC")+coord_fixed(ratio=5/10e8)
ggplot(data_thrombosis3, aes(Aorta_tertile, vmax, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Aorta Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=20/10e6)
ggplot(data_thrombosis3, aes(Aorta_tertile, agg.auc, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Aorta Ptgs2 Expression Tertile", y="Aggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)
ggplot(data_thrombosis3, aes(Aorta_tertile, dis.auc, color=Treatment))+geom_boxplot()+pub_specs+ labs(x="Aorta Ptgs2 Expression Tertile", y="Disaggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)


ggplot(data_thrombosis3, aes(Celecoxib_trough, auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="AUC")
ggplot(data_thrombosis3, aes(Celecoxib_trough, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(Celecoxib_trough, tmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Time to Maximal Thrombus (msec)")
ggplot(data_thrombosis3, aes(Celecoxib_trough, agg.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Aggregation Phase (AUC)")
ggplot(data_thrombosis3, aes(Celecoxib_trough, dis.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Disaggregation Phase (AUC)")

ggplot(data_thrombosis3, aes(Celecoxib_trough, auc, color=Kidney_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="AUC")+coord_fixed(ratio=5/10e8)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, vmax, color=Kidney_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")+coord_fixed(ratio=5/10e5)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, tmax, color=Kidney_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Time to Maximal Thrombus (msec)")+coord_fixed(ratio=1/100)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, agg.auc, color=Kidney_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Aggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, dis.auc, color=Kidney_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Disaggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)+scale_x_log10()

ggplot(data_thrombosis3, aes(Celecoxib_trough, auc, color=Lung_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="AUC")+coord_fixed(ratio=5/10e8)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, vmax, color=Lung_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")+coord_fixed(ratio=5/10e5)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, tmax, color=Lung_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Time to Maximal Thrombus (msec)")+coord_fixed(ratio=1/100)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, agg.auc, color=Lung_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Aggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, dis.auc, color=Lung_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Disaggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)+scale_x_log10()

ggplot(data_thrombosis3, aes(Celecoxib_trough, auc, color=Aorta_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="AUC")+coord_fixed(ratio=5/10e8)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, vmax, color=Aorta_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")+coord_fixed(ratio=5/10e5)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, tmax, color=Aorta_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Time to Maximal Thrombus (msec)")+coord_fixed(ratio=ratio=1/100)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, agg.auc, color=Aorta_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Aggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)+scale_x_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, dis.auc, color=Aorta_tertile))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Disaggregation Phase (AUC)")+coord_fixed(ratio=5/10e8)+scale_x_log10()


ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2", y="AUC")
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, tmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2", y="Time to Maximal Thrombus (msec)")
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, agg.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2", y="Aggregation Phase (AUC)")
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, dis.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2", y="Disaggregation Phase (AUC)")

ggplot(data_thrombosis3, aes(Lung_Ptgs2, auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs2", y="AUC")
ggplot(data_thrombosis3, aes(Lung_Ptgs2, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs2", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(Lung_Ptgs2, tmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs2", y="Time to Maximal Thrombus (msec)")
ggplot(data_thrombosis3, aes(Lung_Ptgs2, agg.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs2", y="Aggregation Phase (AUC)")
ggplot(data_thrombosis3, aes(Lung_Ptgs2, dis.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs2", y="Disaggregation Phase (AUC)")

ggplot(data_thrombosis3, aes(Aorta_Ptgs2, auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs2", y="AUC")
ggplot(data_thrombosis3, aes(Aorta_Ptgs2, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs2", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(Aorta_Ptgs2, tmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs2", y="Time to Maximal Thrombus (msec)")
ggplot(data_thrombosis3, aes(Aorta_Ptgs2, agg.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs2", y="Aggregation Phase (AUC)")
ggplot(data_thrombosis3, aes(Aorta_Ptgs2, dis.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs2", y="Disaggregation Phase (AUC)")

ggplot(data_thrombosis3, aes(Celecoxib_trough, auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="AUC")+ylim(0,4e08)
ggplot(data_thrombosis3, aes(Celecoxib_trough, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")+ylim(0,750000)
ggplot(data_thrombosis3, aes(Celecoxib_trough, tmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Time to Maximal Thrombus (msec)")
ggplot(data_thrombosis3, aes(Celecoxib_trough, agg.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Aggregation Phase (AUC)")+ylim(0,1e08)
ggplot(data_thrombosis3, aes(Celecoxib_trough, dis.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Disaggregation Phase (AUC)")+scale_x_log10()+scale_y_log10()
ggplot(data_thrombosis3, aes(Celecoxib_trough, PGIM_foldchange, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="PGIM (fold change from baseline)")+scale_x_log10()


ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2", y="AUC")+ylim(0,4e08)
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")+ylim(0,750000)
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, tmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2", y="Time to Maximal Thrombus (msec)")
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, agg.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2", y="Aggregation Phase (AUC)")+ylim(0,1e08)
ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, dis.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs2", y="Disaggregation Phase (AUC)")+ylim(0,3e08)

ggplot(data_thrombosis3, aes(Lung_Ptgs2, auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs2", y="AUC")+ylim(0,4e08)
ggplot(data_thrombosis3, aes(Lung_Ptgs2, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs2", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")+ylim(0,750000)
ggplot(data_thrombosis3, aes(Lung_Ptgs2, tmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs2", y="Time to Maximal Thrombus (msec)")
ggplot(data_thrombosis3, aes(Lung_Ptgs2, agg.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs2", y="Aggregation Phase (AUC)")+ylim(0,1e08)
ggplot(data_thrombosis3, aes(Lung_Ptgs2, dis.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs2", y="Disaggregation Phase (AUC)")+ylim(0,3e08)

ggplot(data_thrombosis3, aes(Aorta_Ptgs2, auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs2", y="AUC")+ylim(0,4e08)
ggplot(data_thrombosis3, aes(Aorta_Ptgs2, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs2", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")+ylim(0,750000)
ggplot(data_thrombosis3, aes(Aorta_Ptgs2, tmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs2", y="Time to Maximal Thrombus (msec)")
ggplot(data_thrombosis3, aes(Aorta_Ptgs2, agg.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs2", y="Aggregation Phase (AUC)")+ylim(0,1e08)
ggplot(data_thrombosis3, aes(Aorta_Ptgs2, dis.auc, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs2", y="Disaggregation Phase (AUC)")+ylim(0,3e08)

ggplot(data_thrombosis3, aes(PGIM_foldchange, vmax, color=KO_Group))+geom_point(size=3)+pub_specs+ labs(title="PGIM",x="PGIM (fold-change from baseline)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(TxM_foldchange, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(title="TxM",x="TxM (fold-change from baseline)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(PGEM_foldchange, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(title="PGEM",x="PGEM (fold-change from baseline)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(PGDM_foldchange, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(title="PGDM",x="PGDM (fold-change from baseline)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")

ggplot(data_thrombosis3, aes(PGIM_NSAID, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(title="PGIM",x="PGIM (ng/mg creatinine)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(TxM_NSAID, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(title="TxM",x="TxM (ng/mg creatinine)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(PGEM_NSAID, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(title="PGEM",x="PGEM (ng/mg creatinine)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(PGDM_NSAID, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(title="PGDM",x="PGDM (ng/mg creatinine)", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")

ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs1, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Renal Medulla Ptgs1", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(Lung_Ptgs1, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Lung Ptgs1", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
ggplot(data_thrombosis3, aes(Aorta_Ptgs1, vmax, color=Group))+geom_point(size=3)+pub_specs+ labs(x="Aorta Ptgs1", y="Maximal Thrombus Size (FITC Fluorescence Intensity)")
dev.off()

pdf("DC_Thrombosis_graphs.pdf")
ggplot(data_thrombosis2, aes(variable,value,color=Group))+geom_point()+pub_specs + labs(x="Time (msec)", y="Platelet Aggregation\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=1/1000)+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_thrombosis3, aes(Celecoxib_trough, vmax, color=KO_Group))+geom_point(size=3)+pub_specs + labs(x="Celecoxib Plasma Concentration (micromolar)", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+scale_x_log10()+ guides(colour=guide_legend("KO Group"))+coord_fixed(ratio=3/10e5)+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_thrombosis3, aes(PGIM_foldchange, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(x="Urinary PGIM\n(fold change relative to baseline)", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=12/10e6)+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(PGIM_foldchange, vmax, color=KO_Group))+geom_point(size=3)+pub_specs + labs(x="Urinary PGIM\n(fold change relative to baseline)", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("KO Group"))+coord_fixed(ratio=12/10e6)+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_thrombosis3, aes(PGIM_NSAID, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(x="Urinary PGIM\n(ng/mg creatinine)", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=12/10e5)+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(PGIM_NSAID, vmax, color=KO_Group))+geom_point(size=3)+pub_specs + labs(x="Urinary PGIM\n(ng/mg creatinine)", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("KO Group"))+coord_fixed(ratio=12/10e5)+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_thrombosis3, aes(Renal_medulla_Ptgs2, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Renal Medulla",x="Renal Medulla Ptgs2 Expression", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=15/10e9)+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(Lung_Ptgs2, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Lung",x="Lung Ptgs2 Expression", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=8/10e8)+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(Aorta_Ptgs2, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Aorta",x="Aorta Ptgs2 Expression", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=12/10e9)+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_thrombosis3, aes(Kidney_tertile, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Renal Medulla",x="Renal Medulla Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=25/10e6)+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(Lung_tertile, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Lung",x="Lung Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=25/10e6)+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(Aorta_tertile, vmax, color=Treatment))+geom_point(size=3)+pub_specs + labs(title="Aorta",x="Aorta Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=25/10e6)+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_thrombosis3, aes(Kidney_tertile, vmax, color=Treatment))+geom_boxplot()+pub_specs + labs(title="Renal Medulla",x="Renal Medulla Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=25/10e6)+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(Lung_tertile, vmax, color=Treatment))+geom_boxplot()+pub_specs + labs(title="Lung",x="Lung Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=25/10e6)+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(Aorta_tertile, vmax, color=Treatment))+geom_boxplot()+pub_specs + labs(title="Aorta",x="Aorta Ptgs2 Expression Tertile", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=25/10e6)+theme(legend.position="bottom",legend.box="horizontal")

ggplot(data_thrombosis3, aes(KO_Group, vmax, color=Treatment))+geom_boxplot()+pub_specs + labs(x="KO Group", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+ guides(colour=guide_legend("Treatment"))+coord_fixed(ratio=25/10e6)+theme(legend.position="bottom",legend.box="horizontal")
ggplot(data_thrombosis3, aes(KO_Group, vmax))+geom_boxplot()+pub_specs + labs(x="KO Group", y="Maximal Thrombus Size\n(FITC Fluorescence Intensity)")+coord_fixed(ratio=25/10e6)

dev.off()