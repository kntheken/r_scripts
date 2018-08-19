data=as.data.frame(read.table("COX2.txt"))
data2=as.data.frame(read.table("COX1.txt"))
ids=as.data.frame(read.table("tissuelist.txt", sep="\t"))
library(reshape)
COX2=data[c(2:ncol(data))]
COX1=data2[c(2:ncol(data2))]
COX2.1=as.data.frame(t(COX2))

COX1.1=as.data.frame(t(COX1))
COX2_all=merge(COX2.1,ids, by.x="V1", by.y="V1", all=TRUE)
names(COX2_all)[names(COX2_all)=="V2.y"] <- "Tissue"
names(COX2_all)[names(COX2_all)=="V2.x"] <- "PTGS2"
names(COX2_all)[names(COX2_all)=="V1"] <- "Sample"
COX2_all=COX2_all[-1,]
COX2_all$PTGS2=as.numeric(levels(COX2_all$PTGS2))[COX2_all$PTGS2]
COX2_all$Sample=as.character(COX2_all$Sample)
library(stringr)
COX2_ID_split=as.data.frame(str_split_fixed(COX2_all$Sample,"-",5))
COX2_all$Subject=COX2_ID_split$V2
COX1_all=merge(COX1.1,ids, by.x="V1", by.y="V1", all=TRUE)
names(COX1_all)[names(COX1_all)=="V2.y"] <- "Tissue"
names(COX1_all)[names(COX1_all)=="V2.x"] <- "PTGS1"
names(COX1_all)[names(COX1_all)=="V1"] <- "Sample"
COX1_all=COX1_all[-1,]
COX1_all$PTGS1=as.numeric(levels(COX1_all$PTGS1))[COX1_all$PTGS1]
COX1_all$Sample=as.character(COX1_all$Sample)
COX1_ID_split=as.data.frame(str_split_fixed(COX1_all$Sample,"-",5))
COX1_all$Subject=COX1_ID_split$V2

COX2_LCL=COX2_all[which(COX2_all$Tissue=="Cells - EBV-transformed lymphocytes"),]
COX2_LCL$Rank=rank(COX2_LCL$PTGS2)
COX2_blood=COX2_all[which(COX2_all$Tissue=="Whole Blood"),]
COX2_blood$Rank=rank(COX2_blood$PTGS2)
COX2_coronary=COX2_all[which(COX2_all$Tissue=="Artery - Coronary"),]
COX2_coronary$Rank=rank(COX2_coronary$PTGS2)
COX2_aorta=COX2_all[which(COX2_all$Tissue=="Artery - Aorta"),]
COX2_aorta$Rank=rank(COX2_aorta$PTGS2)
COX2_lung=COX2_all[which(COX2_all$Tissue=="Lung"),]
COX2_lung$Rank=rank(COX2_lung$PTGS2)
COX1_LCL=COX1_all[which(COX1_all$Tissue=="Cells - EBV-transformed lymphocytes"),]
COX1_LCL$Rank=rank(COX1_LCL$PTGS1)
COX1_blood=COX1_all[which(COX1_all$Tissue=="Whole Blood"),]
COX1_blood$Rank=rank(COX1_blood$PTGS1)
COX1_coronary=COX1_all[which(COX1_all$Tissue=="Artery - Coronary"),]
COX1_coronary$Rank=rank(COX1_coronary$PTGS1)
COX1_aorta=COX1_all[which(COX1_all$Tissue=="Artery - Aorta"),]
COX1_aorta$Rank=rank(COX1_aorta$PTGS1)
COX1_lung=COX1_all[which(COX1_all$Tissue=="Lung"),]
COX1_lung$Rank=rank(COX1_lung$PTGS1)

summary(COX2_LCL$PTGS2)
sd(COX2_LCL$PTGS2, na.rm=TRUE)
IQR(COX2_LCL$PTGS2, na.rm=TRUE)
COX2_CV=sd(COX2_LCL$PTGS2, na.rm=TRUE)/mean(COX2_LCL$PTGS2, na.rm=TRUE)
summary(COX1_LCL$PTGS1)
sd(COX1_LCL$PTGS1, na.rm=TRUE)
IQR(COX1_LCL$PTGS1, na.rm=TRUE)
COX1_CV=sd(COX1_LCL$PTGS1, na.rm=TRUE)/mean(COX1_LCL$PTGS1, na.rm=TRUE)


COX2_LCL2=COX2_LCL[c("Subject","PTGS2")]
names(COX2_LCL2)[names(COX2_LCL2)=="PTGS2"] <- "LCL"
COX2_lung2=COX2_lung[c("Subject","PTGS2")]
names(COX2_lung2)[names(COX2_lung2)=="PTGS2"] <- "Lung"
COX2_blood2=COX2_blood[c("Subject","PTGS2")]
names(COX2_blood2)[names(COX2_blood2)=="PTGS2"] <- "Blood"
COX2_coronary2=COX2_coronary[c("Subject","PTGS2")]
names(COX2_coronary2)[names(COX2_coronary2)=="PTGS2"] <- "Coronary"
COX2_aorta2=COX2_aorta[c("Subject","PTGS2")]
names(COX2_aorta2)[names(COX2_aorta2)=="PTGS2"] <- "Aorta"
COX2_LCL_Lung=merge(COX2_LCL2,COX2_lung2, by="Subject", all=TRUE)
COX2_vessels=merge(COX2_coronary2,COX2_aorta2, by="Subject", all=TRUE)
COX2_tissues=merge(COX2_LCL_Lung,COX2_vessels, by="Subject", all=TRUE)
COX2_all2=merge(COX2_tissues,COX2_blood2, by="Subject", all=TRUE)

summary(COX2_blood$Rank)
library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))

pdf("GTex_COX2_by_tissue.pdf")
ggplot(COX2_LCL, aes(Rank,PTGS2))+geom_point()+labs(x="Rank",y="PTGS2 RPKM", title="EBV-transformed Lymphocytes")+pub_specs+scale_y_log10()+coord_fixed(50/1)
ggplot(COX2_blood, aes(Rank,PTGS2))+geom_point()+labs(x="Rank",y="PTGS2 RPKM", title="Whole Blood")+pub_specs+scale_y_log10()+xlim(0,400)+coord_fixed(100/1)
ggplot(COX2_coronary, aes(Rank,PTGS2))+scale_y_log10()+geom_point()+labs(x="Rank",y="PTGS2 RPKM", title="Coronary Artery")+pub_specs+scale_y_log10()+coord_fixed(50/1)
ggplot(COX2_aorta, aes(Rank,PTGS2))+geom_point()+labs(x="Rank",y="PTGS2 RPKM", title="Aorta")+pub_specs+scale_y_log10()+coord_fixed(75/1)
ggplot(COX2_lung, aes(Rank,PTGS2))+geom_point()+labs(x="Rank",y="PTGS2 RPKM", title ="Lung")+pub_specs+scale_y_log10()+coord_fixed(125/1)+xlim(0,350)
dev.off()

pdf("GTex_COX1_by_tissue.pdf")
ggplot(COX1_LCL, aes(Rank,PTGS1))+geom_point()+labs(x="Rank",y="PTGS1 RPKM",title="EBV-transformed Lymphocytes")+pub_specs+scale_y_log10()+coord_fixed(75/1)
ggplot(COX1_blood, aes(Rank,PTGS1))+geom_point()+labs(x="Rank",y="PTGS1 RPKM", title="Whole Blood")+pub_specs+scale_y_log10()+xlim(0,400)+coord_fixed(200/1)
ggplot(COX1_coronary, aes(Rank,PTGS1))+geom_point()+labs(x="Rank",y="PTGS1 RPKM", title="Coronary Artery")+pub_specs+scale_y_log10()+coord_fixed(75/1)
ggplot(COX1_aorta, aes(Rank,PTGS1))+geom_point()+labs(x="Rank",y="PTGS1 RPKM", title="Aorta")+pub_specs+scale_y_log10()+coord_fixed(150/1)
ggplot(COX1_lung, aes(Rank,PTGS1))+geom_point()+labs(x="Rank",y="PTGS1 RPKM", title="Lung")+pub_specs+scale_y_log10()+coord_fixed(400/1)+xlim(0,350)
dev.off()

cor(COX2_LCL_Lung$LCL,COX2_LCL_Lung$Lung, use="complete.obs",method="spearman")
cor(COX2_all2$Blood,COX2_all2$Lung, use="complete.obs",method="spearman")

pdf("GTex_COX2_tissue_correlations.pdf")
ggplot(COX2_all2, aes(Blood,Lung))+geom_point()+labs(x="Whole Blood",y="Lung", title="PTGS2: Whole BLood vs. Lung")+pub_specs+scale_x_log10()+scale_y_log10()+coord_fixed(1.25/1)
ggplot(COX2_all2, aes(LCL,Lung))+geom_point()+labs(x="EBV-transformed Lymphocytes",y="Lung", title="PTGS2: EBV-transformed Lymphocytes vs. Lung")+pub_specs+scale_x_log10()+scale_y_log10()+coord_fixed(1/1.25)
ggplot(COX2_all2, aes(LCL,Blood))+geom_point()+labs(x="EBV-transformed Lymphocytes",y="Whole Blood", title="PTGS2: EBV-transformed Lymphocytes vs. Whole Blood")+pub_specs+scale_x_log10()+scale_y_log10()+coord_fixed(1/1.75)
ggplot(COX2_all2, aes(Blood,Coronary))+geom_point()+labs(x="Whole Blood",y="Coronary Artery", title="PTGS2: Whole Blood vs. Coronary Artery")+pub_specs+scale_x_log10()+scale_y_log10()+coord_fixed(1.35/1)
ggplot(COX2_all2, aes(Blood,Aorta))+geom_point()+labs(x="Whole Blood",y="Aorta", title="PTGS2: Whole Blood vs. Aorta")+pub_specs+scale_x_log10()+scale_y_log10()+coord_fixed(1.15/1)
ggplot(COX2_all2, aes(LCL,Coronary))+geom_point()+labs(x="EBV-transformed Lymphocytes",y="Coronary Artery", title="PTGS2: EBV-transformed Lymphocytes vs. Coronary Artery")+pub_specs+scale_x_log10()+scale_y_log10()+coord_fixed(1/1.15)
ggplot(COX2_all2, aes(LCL,Aorta))+geom_point()+labs(x="EBV-transformed Lymphocytes",y="Aorta", title="PTGS2: EBV-transformed Lymphocytes vs. Aorta")+pub_specs+scale_x_log10()+scale_y_log10()+coord_fixed(1/1.45)
dev.off()

