##Read in data##
data_basal_LPS=read.csv("COX2_PartialKO_basal_LPS.csv")
##Dropping outlier 81-1##
data_basal_LPS=data_basal_LPS[!(data_basal_LPS$Mouse_ID =="081-1 "),]

##Creating a variable to combine the Cre and LPS status##
data_basal_LPS$Group=with(data_basal_LPS, interaction(Cre,Stimulus))
data_basal_LPS$Group.Sex=with(data_basal_LPS, interaction(Group,Sex))
data_basal_LPS$KO_Group=ifelse(data_basal_LPS$Cre=="-", "control", as.character(data_basal_LPS$Tamoxifen_dose))
data_basal_LPS$Kidney_tertile= with(data_basal_LPS, cut (Renal_medulla_Ptgs2, breaks=quantile(Renal_medulla_Ptgs2, probs=seq(0,1,by=0.33), na.rm=TRUE),include.lowest=TRUE))
data_basal_LPS$Lung_tertile= with(data_basal_LPS, cut (Lung_Ptgs2, breaks=quantile(Renal_medulla_Ptgs2, probs=seq(0,1,by=0.33), na.rm=TRUE),include.lowest=TRUE))
data_basal_LPS$Celecoxib.LPS_trough=ifelse(data_basal_LPS$Treatment=="none", as.numeric(0), as.numeric(data_basal_LPS$Celecoxib.LPS_trough))

##Creating subsets##
data_basal_LPS_TM4levels=data_basal_LPS[!(data_basal_LPS$Tamoxifen_dose=="40x3"|data_basal_LPS$Tamoxifen_dose=="10x3"),]
data_neg=data_basal_LPS[which(data_basal_LPS$Cre=="-"),]
data_neg_basal=data_neg[which(data_neg$Stimulus=="none"),]
data_neg_LPS=data_neg[which(data_neg$Stimulus=="LPS"),]
data_pos=data_basal_LPS[which(data_basal_LPS$Cre=="+"),]
data_pos_basal=data_pos[which(data_pos$Stimulus=="none"),]
data_pos_LPS=data_pos[which(data_pos$Stimulus=="LPS"),]
data_basal=data_basal_LPS[which(data_basal_LPS$Stimulus=="none"),]
data_basal_male=data_basal[which(data_basal$Sex=="M"),]
data_basal_female=data_basal[which(data_basal$Sex=="F"),]
data_LPS=data_basal_LPS[which(data_basal_LPS$Stimulus=="LPS"),]
data_LPS_male=data_LPS[which(data_LPS$Sex=="M"),]
data_LPS_female=data_LPS[which(data_LPS$Sex=="F"),]
data_basal_TM4levels=data_basal_LPS_TM4levels[which(data_basal_LPS_TM4levels$Stimulus=="none"),]
data_LPS_TM4levels=data_basal_LPS_TM4levels[which(data_basal_LPS_TM4levels$Stimulus=="LPS"),]
data_basal_TM4levels_male=data_basal_TM4levels[which(data_basal_LPS_TM4levels$Sex=="M"),]
data_basal_TM4levels_female=data_basal_TM4levels[which(data_basal_LPS_TM4levels$Sex=="F"),]
data_LPS_TM4levels_male=data_LPS_TM4levels[which(data_basal_LPS_TM4levels$Sex=="M"),]
data_LPS_TM4levels_female=data_LPS_TM4levels[which(data_basal_LPS_TM4levels$Sex=="F"),]
basal_corr_vars=c("Renal_medulla_Ptgs2","Renal_medulla_Ptgs1","Lung_Ptgs2","Lung_Ptgs1", "PGDM_basal", "PGEM_basal", "PGIM_basal", "TxB2_basal")
LPS_corr_vars=c("Renal_medulla_Ptgs2","Renal_medulla_Ptgs1","Lung_Ptgs2","Lung_Ptgs1", "PGDM_stimulated","PGEM_stimulated","PGIM_stimulated","TxB2_stimulated")
data_basal_corr=data_basal[basal_corr_vars]
data_basal_male_corr=data_basal_male[basal_corr_vars]
data_basal_female_corr=data_basal_female[basal_corr_vars]
data_LPS_corr=data_LPS[LPS_corr_vars]
data_LPS_male_corr=data_LPS_male[LPS_corr_vars]
data_LPS_female_corr=data_LPS_female[LPS_corr_vars]

##Correlation matrices##
library(Hmisc)
library(xlsx)

basal_corr=rcorr(as.matrix(data_basal_corr),type="spearman")

basal_male_corr=rcorr(as.matrix(data_basal_male_corr),type="spearman")

basal_female_corr=rcorr(as.matrix(data_basal_female_corr),type="spearman")

LPS_corr=rcorr(as.matrix(data_LPS_corr),type="spearman")

LPS_male_corr=rcorr(as.matrix(data_LPS_male_corr),type="spearman")

LPS_female_corr=rcorr(as.matrix(data_LPS_female_corr),type="spearman")

basal_corr
basal_male_corr
basal_female_corr
LPS_corr
LPS_male_corr
LPS_female_corr

corrout=function(x,title, filename){
	cat(title,file=filename, append=TRUE, fill=TRUE)
	capture.output(x,file=filename,append=TRUE)
	cat("\n",file=filename, append=TRUE)
	}

corrout(basal_corr,"Spearman Correlations Under Basal Conditions:  All", "correlations.txt")
corrout(basal_male_corr,"Spearman Correlations Under Basal Conditions:  Males", "correlations.txt")
corrout(basal_female_corr,"Spearman Correlations Under Basal Conditions:  Females", "correlations.txt")
corrout(LPS_corr,"Spearman Correlations Following LPS Stimulation:  All", "correlations.txt")
corrout(LPS_male_corr,"Spearman Correlations Following LPS Stimulation:  Males", "correlations.txt")
corrout(LPS_female_corr,"Spearman Correlations Following LPS Stimulation:  Females", "correlations.txt")

##Calculating age and time since TM##
data_basal_LPS$Age=as.Date(data_basal_LPS$Sac_date)-as.Date(data_basal_LPS$DOB)
data_basal_LPS$TM_time=as.Date(data_basal_LPS$Sac_date)-as.Date(data_basal_LPS$TM_date)

##Descriptive stats by group and group.sex##
library(psych)
describe.by(data_basal_LPS, data_basal_LPS$Group)
describe.by(data_basal_LPS, data_basal_LPS$Group.Sex)

##Density plots for Ptgs2 expression by Cre and LPS##
library(ggplot2)
library(gridExtra)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"))
pdf("Density_plots.pdf")
ggplot(data_basal_LPS, aes(Lung_Ptgs1,fill=Group,color=Group))+ geom_density(alpha=0.1)+labs(title="All",x="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Lung_Ptgs2,fill=Group,color=Group))+ geom_density(alpha=0.1)+labs(title="All",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Renal_medulla_Ptgs1,fill=Group,color=Group))+ geom_density(alpha=0.1)+labs(title="All",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Renal_medulla_Ptgs2,fill=Group,color=Group))+ geom_density(alpha=0.1)+labs(title="All",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Lung_Ptgs1,fill=Stimulus,color=Stimulus))+ geom_density(alpha=0.1)+labs(title="All",x="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Lung_Ptgs2,fill=Stimulus,color=Stimulus))+ geom_density(alpha=0.1)+labs(title="All",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Renal_medulla_Ptgs1,fill=Stimulus,color=Stimulus))+ geom_density(alpha=0.1)+labs(title="All",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Renal_medulla_Ptgs2,fill=Stimulus,color=Stimulus))+ geom_density(alpha=0.1)+labs(title="All",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Renal_medulla_Ptgs2,color=Group))+ stat_ecdf()+labs(title="All",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Renal_medulla_Ptgs1,color=Group))+ stat_ecdf()+labs(title="All",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Lung_Ptgs2,color=Group))+ stat_ecdf()+labs(title="All",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Lung_Ptgs1,color=Group))+ stat_ecdf()+labs(title="All",x="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Renal_medulla_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="All",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Renal_medulla_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="All",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Lung_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="All",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_LPS, aes(Lung_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="All",x="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="Basal",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="Basal",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal, aes(Lung_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="Basal",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal, aes(Lung_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="Basal",x="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="LPS",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="LPS",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_LPS, aes(Lung_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="LPS",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_LPS, aes(Lung_Ptgs1,color=KO_Group))+ stat_ecdf()+labstitle="LPS",(x="Lung Ptgs1/Gapdh") + pub_specs
ggplot(data_basal_LPS_TM4levels, aes(Renal_medulla_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="All",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_LPS_TM4levels, aes(Renal_medulla_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="All",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_LPS_TM4levels, aes(Lung_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="All",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_LPS_TM4levels, aes(Lung_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="All",x="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels, aes(Renal_medulla_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="Basal",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels, aes(Renal_medulla_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="Basal",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels, aes(Lung_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="Basal",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels, aes(Lung_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="Basal",x="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels, aes(Renal_medulla_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="LPS",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels, aes(Renal_medulla_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="LPS",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels, aes(Lung_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="LPS",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels, aes(Lung_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="LPS",x="Lung Ptgs1/Gapdh") + pub_specs
ggplot(data_basal_TM4levels_female, aes(Renal_medulla_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="Basal: Females",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels_female, aes(Renal_medulla_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="Basal: Females",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels_female, aes(Lung_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="Basal: Females",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels_female, aes(Lung_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="Basal: Females",x="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels_female, aes(Renal_medulla_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="LPS: Females",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels_female, aes(Renal_medulla_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="LPS: Females",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels_female, aes(Lung_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="LPS: Females",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels_female, aes(Lung_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="LPS: Females",x="Lung Ptgs1/Gapdh") + pub_specs
ggplot(data_basal_TM4levels_male, aes(Renal_medulla_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="Basal: Males",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels_male, aes(Renal_medulla_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="Basal: Males",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels_male, aes(Lung_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="Basal: Males",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels_male, aes(Lung_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="Basal: Males",x="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels_male, aes(Renal_medulla_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="LPS: Males",x="Renal Medulla Ptgs2/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels_male, aes(Renal_medulla_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="LPS: Males",x="Renal Medulla Ptgs1/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels_male, aes(Lung_Ptgs2,color=KO_Group))+ stat_ecdf()+labs(title="LPS: Males",x="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels_male, aes(Lung_Ptgs1,color=KO_Group))+ stat_ecdf()+labs(title="LPS: Males",x="Lung Ptgs1/Gapdh") + pub_specs
dev.off()

##Checking effect of TM dose on Expression##
anova(lm(data_neg_basal$Renal_medulla_Ptgs2~data_neg_basal$Tamoxifen_dose))
pairwise.t.test(data_neg_basal$Renal_medulla_Ptgs2,data_neg_basal$Tamoxifen_dose, p.adj="none")
anova(lm(data_pos_basal$Renal_medulla_Ptgs2~data_pos_basal$Tamoxifen_dose))
anova(lm(data_neg_LPS$Renal_medulla_Ptgs2~data_neg_LPS$Tamoxifen_dose))
anova(lm(data_pos_LPS$Renal_medulla_Ptgs2~data_pos_LPS$Tamoxifen_dose))
anova(lm(data_neg_basal$Renal_medulla_Ptgs1~data_neg_basal$Tamoxifen_dose))
anova(lm(data_pos_basal$Renal_medulla_Ptgs1~data_pos_basal$Tamoxifen_dose))
anova(lm(data_neg_LPS$Renal_medulla_Ptgs1~data_neg_LPS$Tamoxifen_dose))
anova(lm(data_pos_LPS$Renal_medulla_Ptgs1~data_pos_LPS$Tamoxifen_dose))
anova(lm(data_neg_basal$Lung_Ptgs2~data_neg_basal$Tamoxifen_dose))
anova(lm(data_pos_basal$Lung_Ptgs2~data_pos_basal$Tamoxifen_dose))
anova(lm(data_neg_LPS$Lung_Ptgs2~data_neg_LPS$Tamoxifen_dose))
anova(lm(data_pos_LPS$Lung_Ptgs2~data_pos_LPS$Tamoxifen_dose))
anova(lm(data_neg_basal$Lung_Ptgs1~data_neg_basal$Tamoxifen_dose))
anova(lm(data_pos_basal$Lung_Ptgs1~data_pos_basal$Tamoxifen_dose))
anova(lm(data_neg_LPS$Lung_Ptgs1~data_neg_LPS$Tamoxifen_dose))
anova(lm(data_pos_LPS$Lung_Ptgs1~data_pos_LPS$Tamoxifen_dose))

##Graphs of Expression by TM dose and Cre##
pdf("Expression_by_TM_dose_Cre.pdf")
ggplot(data_neg_basal, aes(Tamoxifen_dose, Renal_medulla_Ptgs2))+ geom_boxplot()+labs(title="Cre-, basal")+ pub_specs
ggplot(data_pos_basal, aes(Tamoxifen_dose, Renal_medulla_Ptgs2))+ geom_boxplot()+labs(title="Cre+, basal")+ pub_specs
ggplot(data_neg_LPS, aes(Tamoxifen_dose, Renal_medulla_Ptgs2))+ geom_boxplot()+labs(title="Cre-, LPS")+ pub_specs
ggplot(data_pos_LPS, aes(Tamoxifen_dose, Renal_medulla_Ptgs2))+ geom_boxplot()+labs(title="Cre+, LPS")+ pub_specs
ggplot(data_neg_basal, aes(Tamoxifen_dose, Renal_medulla_Ptgs1))+ geom_boxplot()+labs(title="Cre-, basal")+ pub_specs
ggplot(data_pos_basal, aes(Tamoxifen_dose, Renal_medulla_Ptgs1))+ geom_boxplot()+labs(title="Cre+, basal")+ pub_specs
ggplot(data_neg_LPS, aes(Tamoxifen_dose, Renal_medulla_Ptgs1))+ geom_boxplot()+labs(title="Cre-, LPS")+ pub_specs
ggplot(data_pos_LPS, aes(Tamoxifen_dose, Renal_medulla_Ptgs1))+ geom_boxplot()+labs(title="Cre+, LPS")+ pub_specs
ggplot(data_neg_basal, aes(Tamoxifen_dose, Lung_Ptgs2))+ geom_boxplot()+labs(title="Cre-, basal")+ pub_specs
ggplot(data_pos_basal, aes(Tamoxifen_dose, Lung_Ptgs2))+ geom_boxplot()+labs(title="Cre+, basal")+ pub_specs
ggplot(data_neg_LPS, aes(Tamoxifen_dose, Lung_Ptgs2))+ geom_boxplot()+labs(title="Cre-, LPS")+ pub_specs
ggplot(data_pos_LPS, aes(Tamoxifen_dose, Lung_Ptgs2))+ geom_boxplot()+labs(title="Cre+, LPS")+ pub_specs
ggplot(data_neg_basal, aes(Tamoxifen_dose, Lung_Ptgs1))+ geom_boxplot()+labs(title="Cre-, basal")+ pub_specs
ggplot(data_pos_basal, aes(Tamoxifen_dose, Lung_Ptgs1))+ geom_boxplot()+labs(title="Cre+, basal")+ pub_specs
ggplot(data_neg_LPS, aes(Tamoxifen_dose, Lung_Ptgs1))+ geom_boxplot()+labs(title="Cre-, LPS")+ pub_specs
ggplot(data_pos_LPS, aes(Tamoxifen_dose, Lung_Ptgs1))+ geom_boxplot()+labs(title="Cre+, LPS")+ pub_specs
dev.off()

ggplot(data_basal_TM4levels, aes(Tamoxifen_dose, Renal_medulla_Ptgs2, color=Cre))+ geom_boxplot()+labs(title="Basal: Renal Medulla Ptgs2", y="Renal Medulla Ptgs2/Gapdh", x="Group")+ pub_specs

pdf("Expression_by_TM_dose.pdf")
ggplot(data_basal_TM4levels, aes(KO_Group, Renal_medulla_Ptgs2))+ geom_boxplot()+labs(title="Basal: Renal Medulla Ptgs2", y="Renal Medulla Ptgs2/Gapdh", x="Group")+ pub_specs
ggplot(data_basal_TM4levels, aes(KO_Group, Renal_medulla_Ptgs1))+ geom_boxplot()+labs(title="Basal: Renal Medulla Ptgs1", y="Renal Medulla Ptgs1/Gapdh", x="Group")+ pub_specs
ggplot(data_basal_TM4levels, aes(KO_Group, Lung_Ptgs2))+ geom_boxplot()+labs(title="Basal: Lung Ptgs2", x="Group", y="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels, aes(KO_Group, Lung_Ptgs1))+ geom_boxplot()+labs(title="Basal: Lung Ptgs1", x="Group", y="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels, aes(KO_Group, Renal_medulla_Ptgs2, color=Sex))+ geom_boxplot()+labs(title="Basal: Renal Medulla Ptgs2", y="Renal Medulla Ptgs2/Gapdh", x="Group")+ pub_specs
ggplot(data_basal_TM4levels, aes(KO_Group, Renal_medulla_Ptgs1, color=Sex))+ geom_boxplot()+labs(title="Basal: Renal Medulla Ptgs1", y="Renal Medulla Ptgs1/Gapdh", x="Group")+ pub_specs
ggplot(data_basal_TM4levels, aes(KO_Group, Lung_Ptgs2, color=Sex))+ geom_boxplot()+labs(title="Basal: Lung Ptgs2", x="Group", y="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_basal_TM4levels, aes(KO_Group, Lung_Ptgs1, color=Sex))+ geom_boxplot()+labs(title="Basal: Lung Ptgs1", x="Group", y="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels, aes(KO_Group, Renal_medulla_Ptgs2))+ geom_boxplot()+labs(title="LPS: Renal Medulla Ptgs2", y="Renal Medulla Ptgs2/Gapdh", x="Group")+ pub_specs
ggplot(data_LPS_TM4levels, aes(KO_Group, Renal_medulla_Ptgs1))+ geom_boxplot()+labs(title="LPS: Renal Medulla Ptgs1", y="Renal Medulla Ptgs1/Gapdh", x="Group")+ pub_specs
ggplot(data_LPS_TM4levels, aes(KO_Group, Lung_Ptgs2))+ geom_boxplot()+labs(title="LPS: Lung Ptgs2", x="Group", y="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels, aes(KO_Group, Lung_Ptgs1))+ geom_boxplot()+labs(title="LPS: Lung Ptgs1", x="Group", y="Lung Ptgs1/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels, aes(KO_Group, Renal_medulla_Ptgs2, color=Sex))+ geom_boxplot()+labs(title="LPS: Renal Medulla Ptgs2", y="Renal Medulla Ptgs2/Gapdh", x="Group")+ pub_specs
ggplot(data_LPS_TM4levels, aes(KO_Group, Renal_medulla_Ptgs1, color=Sex))+ geom_boxplot()+labs(title="LPS: Renal Medulla Ptgs1", y="Renal Medulla Ptgs1/Gapdh", x="Group")+ pub_specs
ggplot(data_LPS_TM4levels, aes(KO_Group, Lung_Ptgs2, color=Sex))+ geom_boxplot()+labs(title="LPS: Lung Ptgs2", x="Group", y="Lung Ptgs2/Gapdh")+ pub_specs
ggplot(data_LPS_TM4levels, aes(KO_Group, Lung_Ptgs1, color=Sex))+ geom_boxplot()+labs(title="LPS: Lung Ptgs1", x="Group", y="Lung Ptgs1/Gapdh")+ pub_specs
dev.off()

##Graphs of Expression vs. Urinary Metabolites##
pdf("Expression_vs_PGs.pdf")
ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGDM_basal))+geom_point()+labs(title="Basal: Renal medulla Ptgs2 vs. PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGEM_basal))+geom_point()+labs(title="Basal: Renal medulla Ptgs2 vs. PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGIM_basal))+geom_point()+labs(title="Basal: Renal medulla Ptgs2 vs. PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs2,TxB2_basal))+geom_point()+labs(title="Basal: Renal medulla Ptgs2 vs. TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGDM_basal))+geom_point()+labs(title="Basal: Renal medulla Ptgs1 vs. PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGEM_basal))+geom_point()+labs(title="Basal: Renal medulla Ptgs1 vs. PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGIM_basal))+geom_point()+labs(title="Basal: Renal medulla Ptgs1 vs. PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs1,TxB2_basal)))+geom_point()+labs(title="Basal: Renal medulla Ptgs1 vs. TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_basal, aes(Lung_Ptgs2,PGDM_basal))+geom_point()+labs(title="Basal: Lung Ptgs2 vs. PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs2,PGEM_basal))+geom_point()+labs(title="Basal: Lung Ptgs2 vs. PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs2,PGIM_basal))+geom_point()+labs(title="Basal: Lung Ptgs2 vs. PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs2,TxB2_basal))+geom_point()+labs(title="Basal: Lung Ptgs2 vs. TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_basal, aes(Lung_Ptgs1,PGDM_basal))+geom_point()+labs(title="Basal: Lung Ptgs1 vs. PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs1,PGEM_basal))+geom_point()+labs(title="Basal: Lung Ptgs1 vs. PGEM",x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs1,PGIM_basal))+geom_point()+labs(title="Basal: Lung Ptgs1 vs. PGIM", x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs1,TxB2_basal))+geom_point()+labs(title="Basal: Lung Ptgs1 vs. TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGDM_stimulated))+geom_point()+labs(title="LPS: Renal medulla Ptgs2 vs. PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGEM_stimulated))+geom_point()+labs(title="LPS: Renal medulla Ptgs2 vs. PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGIM_stimulated))+geom_point()+labs(title="LPS: Renal medulla Ptgs2 vs. PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs2,TxB2_stimulated))+geom_point()+labs(title="LPS: Renal medulla Ptgs2 vs. TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGDM_stimulated))+geom_point()+labs(title="LPS: Renal medulla Ptgs1 vs. PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGEM_stimulated))+geom_point()+labs(title="LPS: Renal medulla Ptgs1 vs. PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGIM_stimulated))+geom_point()+labs(title="LPS: Renal medulla Ptgs1 vs. PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs1,TxB2_stimulated)))+geom_point()+labs(title="LPS: Renal medulla Ptgs1 vs. TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_LPS, aes(Lung_Ptgs2,PGDM_stimulated))+geom_point()+labs(title="LPS: Lung Ptgs2 vs. PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs2,PGEM_stimulated))+geom_point()+labs(title="LPS: Lung Ptgs2 vs. PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs2,PGIM_stimulated))+geom_point()+labs(title="LPS: Lung Ptgs2 vs. PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs2,TxB2_stimulated))+geom_point()+labs(title="LPS: Lung Ptgs2 vs. TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_LPS, aes(Lung_Ptgs1,PGDM_stimulated))+geom_point()+labs(title="LPS: Lung Ptgs1 vs. PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs1,PGEM_stimulated))+geom_point()+labs(title="LPS: Lung Ptgs1 vs. PGEM",x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs1,PGIM_stimulated))+geom_point()+labs(title="LPS: Lung Ptgs1 vs. PGIM", x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs1,TxB2_stimulated))+geom_point()+labs(title="LPS: Lung Ptgs1 vs. TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs
dev.off()

pdf("Expression_vs_PGs_by_Sex.pdf")
ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGDM_basal, color=Sex))+geom_point()+labs(title="Basal: Renal medulla Ptgs2 vs. PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGEM_basal, color=Sex))+geom_point()+labs(title="Basal: Renal medulla Ptgs2 vs. PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGIM_basal, color=Sex))+geom_point()+labs(title="Basal: Renal medulla Ptgs2 vs. PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs2,TxB2_basal, color=Sex))+geom_point()+labs(title="Basal: Renal medulla Ptgs2 vs. TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGDM_basal, color=Sex))+geom_point()+labs(title="Basal: Renal medulla Ptgs1 vs. PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGEM_basal, color=Sex))+geom_point()+labs(title="Basal: Renal medulla Ptgs1 vs. PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGIM_basal, color=Sex))+geom_point()+labs(title="Basal: Renal medulla Ptgs1 vs. PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Renal_medulla_Ptgs1,TxB2_basal, color=Sex)))+geom_point()+labs(title="Basal: Renal medulla Ptgs1 vs. TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_basal, aes(Lung_Ptgs2,PGDM_basal, color=Sex))+geom_point()+labs(title="Basal: Lung Ptgs2 vs. PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs2,PGEM_basal, color=Sex))+geom_point()+labs(title="Basal: Lung Ptgs2 vs. PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs2,PGIM_basal, color=Sex))+geom_point()+labs(title="Basal: Lung Ptgs2 vs. PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs2,TxB2_basal, color=Sex))+geom_point()+labs(title="Basal: Lung Ptgs2 vs. TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_basal, aes(Lung_Ptgs1,PGDM_basal, color=Sex))+geom_point()+labs(title="Basal: Lung Ptgs1 vs. PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs1,PGEM_basal, color=Sex))+geom_point()+labs(title="Basal: Lung Ptgs1 vs. PGEM",x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs1,PGIM_basal, color=Sex))+geom_point()+labs(title="Basal: Lung Ptgs1 vs. PGIM", x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_basal, aes(Lung_Ptgs1,TxB2_basal, color=Sex))+geom_point()+labs(title="Basal: Lung Ptgs1 vs. TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGDM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Renal medulla Ptgs2 vs. PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGEM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Renal medulla Ptgs2 vs. PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGIM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Renal medulla Ptgs2 vs. PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs2,TxB2_stimulated, color=Sex))+geom_point()+labs(title="LPS: Renal medulla Ptgs2 vs. TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGDM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Renal medulla Ptgs1 vs. PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGEM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Renal medulla Ptgs1 vs. PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGIM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Renal medulla Ptgs1 vs. PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Renal_medulla_Ptgs1,TxB2_stimulated, color=Sex)))+geom_point()+labs(title="LPS: Renal medulla Ptgs1 vs. TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_LPS, aes(Lung_Ptgs2,PGDM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Lung Ptgs2 vs. PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs2,PGEM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Lung Ptgs2 vs. PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs2,PGIM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Lung Ptgs2 vs. PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs2,TxB2_stimulated, color=Sex))+geom_point()+labs(title="LPS: Lung Ptgs2 vs. TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs

ggplot(data_LPS, aes(Lung_Ptgs1,PGDM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Lung Ptgs1 vs. PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs1,PGEM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Lung Ptgs1 vs. PGEM",x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs1,PGIM_stimulated, color=Sex))+geom_point()+labs(title="LPS: Lung Ptgs1 vs. PGIM", x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
ggplot(data_LPS, aes(Lung_Ptgs1,TxB2_stimulated, color=Sex))+geom_point()+labs(title="LPS: Lung Ptgs1 vs. TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs
dev.off()

pdf("Expression_vs_PGs_with_lines.pdf")
PGDM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGDM_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
PGEM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGEM_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
PGIM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGIM_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
TxM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,TxB2_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGDM_stimulated))+geom_smooth(se=FALSE, method="lm")+geom_point()+labs(title="PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
PGEM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGEM_stimulated))+geom_smooth(se=FALSE, method="lm")+geom_point()+labs(title="PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
PGIM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGIM_stimulated))+geom_smooth(se=FALSE, method="lm")+geom_point()+labs(title="PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
TxM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,TxB2_stimulated))+geom_smooth(se=FALSE, method="lm")+geom_point()+labs(title="TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Lung_Ptgs2,PGDM_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
PGEM=ggplot(data_LPS, aes(Lung_Ptgs2,PGEM_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
PGIM=ggplot(data_LPS, aes(Lung_Ptgs2,PGIM_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
TxM=ggplot(data_LPS, aes(Lung_Ptgs2,TxB2_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Lung_Ptgs1,PGDM_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine)")+pub_specs
PGEM=ggplot(data_LPS, aes(Lung_Ptgs1,PGEM_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine)")+pub_specs
PGIM=ggplot(data_LPS, aes(Lung_Ptgs1,PGIM_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine)")+pub_specs
TxM=ggplot(data_LPS, aes(Lung_Ptgs1,TxB2_stimulated))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine)")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGDM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGEM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGIM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,TxB2_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGDM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGEM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGIM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,TxB2_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Lung_Ptgs2,PGDM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Lung_Ptgs2,PGEM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Lung_Ptgs2,PGIM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Lung_Ptgs2,TxB2_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Lung_Ptgs1,PGDM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Lung_Ptgs1,PGEM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM", x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Lung_Ptgs1,PGIM_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM",x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Lung_Ptgs1,TxB2_basal))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGDM_stimulated))+geom_point()+geom_smooth()+labs(title="PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGEM_stimulated))+geom_point()+geom_smooth()+labs(title="PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGIM_stimulated))+geom_point()+geom_smooth()+labs(title="PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,TxB2_stimulated))+geom_point()+geom_smooth()+labs(title="TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGDM_stimulated))+geom_smooth()+geom_point()+labs(title="PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGEM_stimulated))+geom_smooth()+geom_point()+labs(title="PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGIM_stimulated))+geom_smooth()+geom_point()+labs(title="PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,TxB2_stimulated))+geom_smooth()+geom_point()+labs(title="TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Lung_Ptgs2,PGDM_stimulated))+geom_point()+geom_smooth()+labs(title="PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Lung_Ptgs2,PGEM_stimulated))+geom_point()+geom_smooth()+labs(title="PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Lung_Ptgs2,PGIM_stimulated))+geom_point()+geom_smooth()+labs(title="PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Lung_Ptgs2,TxB2_stimulated))+geom_point()+geom_smooth()+labs(title="TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Lung_Ptgs1,PGDM_stimulated))+geom_point()+geom_smooth()+labs(title="PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Lung_Ptgs1,PGEM_stimulated))+geom_point()+geom_smooth()+labs(title="PGEM",x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Lung_Ptgs1,PGIM_stimulated))+geom_point()+geom_smooth()+labs(title="PGIM", x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Lung_Ptgs1,TxB2_stimulated))+geom_point()+geom_smooth()+labs(title="TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGDM_basal))+geom_point()+geom_smooth()+labs(title="PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGEM_basal))+geom_point()+geom_smooth()+labs(title="PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGIM_basal))+geom_point()+geom_smooth()+labs(title="PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,TxB2_basal))+geom_point()+geom_smooth()+labs(title="TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGDM_basal))+geom_point()+geom_smooth()+labs(title="PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGEM_basal))+geom_point()+geom_smooth()+labs(title="PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGIM_basal))+geom_point()+geom_smooth()+labs(title="PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,TxB2_basal))+geom_point()+geom_smooth()+labs(title="TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Lung_Ptgs2,PGDM_basal))+geom_point()+geom_smooth()+labs(title="PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Lung_Ptgs2,PGEM_basal))+geom_point()+geom_smooth()+labs(title="PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Lung_Ptgs2,PGIM_basal))+geom_point()+geom_smooth()+labs(title="PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Lung_Ptgs2,TxB2_basal))+geom_point()+geom_smooth()+labs(title="TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Lung_Ptgs1,PGDM_basal))+geom_point()+geom_smooth()+labs(title="PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs=ggplot(data_basal, aes(Lung_Ptgs1,PGEM_basal,color=Sex))+geom_point()+labs(title="PGEM",x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Lung_Ptgs1,PGEM_basal))+geom_point()+geom_smooth()+labs(title="PGEM", x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Lung_Ptgs1,PGIM_basal))+geom_point()+geom_smooth()+labs(title="PGIM",x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Lung_Ptgs1,TxB2_basal))+geom_point()+geom_smooth()+labs(title="TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGDM_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGEM_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGIM_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,TxB2_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGDM_stimulated,color=Sex))+geom_smooth(se=FALSE, method="lm")+geom_point()+labs(title="PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGEM_stimulated,color=Sex))+geom_smooth(se=FALSE, method="lm")+geom_point()+labs(title="PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGIM_stimulated,color=Sex))+geom_smooth(se=FALSE, method="lm")+geom_point()+labs(title="PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,TxB2_stimulated,color=Sex))+geom_smooth(se=FALSE, method="lm")+geom_point()+labs(title="TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Lung_Ptgs2,PGDM_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Lung_Ptgs2,PGEM_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Lung_Ptgs2,PGIM_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Lung_Ptgs2,TxB2_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Lung_Ptgs1,PGDM_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Lung_Ptgs1,PGEM_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Lung_Ptgs1,PGIM_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Lung_Ptgs1,TxB2_stimulated,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGDM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGEM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGIM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,TxB2_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGDM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGEM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGIM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,TxB2_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Lung_Ptgs2,PGDM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Lung_Ptgs2,PGEM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Lung_Ptgs2,PGIM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Lung_Ptgs2,TxB2_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Lung_Ptgs1,PGDM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Lung_Ptgs1,PGEM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGEM", x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Lung_Ptgs1,PGIM_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="PGIM",x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Lung_Ptgs1,TxB2_basal,color=Sex))+geom_point()+geom_smooth(se=FALSE, method="lm")+labs(title="TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGDM_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGEM_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,PGIM_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Renal_medulla_Ptgs2,TxB2_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGDM_stimulated,color=Sex))+geom_smooth()+geom_point()+labs(title="PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGEM_stimulated,color=Sex))+geom_smooth()+geom_point()+labs(title="PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,PGIM_stimulated,color=Sex))+geom_smooth()+geom_point()+labs(title="PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Renal_medulla_Ptgs1,TxB2_stimulated,color=Sex))+geom_smooth()+geom_point()+labs(title="TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Lung_Ptgs2,PGDM_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Lung_Ptgs2,PGEM_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Lung_Ptgs2,PGIM_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Lung_Ptgs2,TxB2_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_LPS, aes(Lung_Ptgs1,PGDM_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_LPS, aes(Lung_Ptgs1,PGEM_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="PGEM",x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_LPS, aes(Lung_Ptgs1,PGIM_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="PGIM", x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_LPS, aes(Lung_Ptgs1,TxB2_stimulated,color=Sex))+geom_point()+geom_smooth()+labs(title="TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGDM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGDM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGEM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGEM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,PGIM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGIM", x="Renal Medulla Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Renal_medulla_Ptgs2,TxB2_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="TxM",x="Renal Medulla Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGDM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGDM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGEM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGEM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,PGIM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGIM", x="Renal Medulla Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Renal_medulla_Ptgs1,TxB2_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="TxM",x="Renal Medulla Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Lung_Ptgs2,PGDM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGDM",x="Lung Ptgs2/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Lung_Ptgs2,PGEM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGEM",x="Lung Ptgs2/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Lung_Ptgs2,PGIM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGIM", x="Lung Ptgs2/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Lung_Ptgs2,TxB2_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="TxM",x="Lung Ptgs2/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
PGDM=ggplot(data_basal, aes(Lung_Ptgs1,PGDM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGDM",x="Lung Ptgs1/Gapdh",y="Urinary PGDM (ng/mg creatinine")+pub_specs
PGEM=ggplot(data_basal, aes(Lung_Ptgs1,PGEM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGEM", x="Lung Ptgs1/Gapdh",y="Urinary PGEM (ng/mg creatinine")+pub_specs
PGIM=ggplot(data_basal, aes(Lung_Ptgs1,PGIM_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="PGIM",x="Lung Ptgs1/Gapdh",y="Urinary PGIM (ng/mg creatinine")+pub_specs
TxM=ggplot(data_basal, aes(Lung_Ptgs1,TxB2_basal,color=Sex))+geom_point()+geom_smooth()+labs(title="TxM",x="Lung Ptgs1/Gapdh",y="Urinary TxM (ng/mg creatinine")+pub_specs
grid.arrange(PGDM, PGEM, PGIM, TxM, ncol=2)
dev.off()

data_LPS_cele$Kidney_tertile= with(data_LPS_cele, cut (Renal_medulla_Ptgs2, breaks=quantile(Renal_medulla_Ptgs2, probs=seq(0,1,by=0.33), na.rm=TRUE),include.lowest=TRUE))
