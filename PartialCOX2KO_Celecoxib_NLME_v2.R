##Read in data##
data_LPS_celediet=read.csv("COX2_PartialKO_LPS_LPScelediet.csv")
data_LPS_acutecele=read.csv("COX2_PartialKO_LPS_acutecele.csv")

data_LPS_celediet$Celecoxib.LPS_trough[is.na(data_LPS_celediet$Celecoxib.LPS_trough)]=0
data_LPS_acutecele$Celecoxib.LPS_trough[is.na(data_LPS_acutecele$Celecoxib.LPS_trough)]=0
data_LPS_celediet$Cage=as.factor(data_LPS_celediet$Cage)
data_LPS_acutecele$Cage=as.factor(data_LPS_acutecele$Cage)
data_LPS_celediet$KO_Group=ifelse(data_LPS_celediet$Cre=="-", "control", as.character(data_LPS_celediet$Tamoxifen_dose))
data_LPS_acutecele$KO_Group=ifelse(data_LPS_acutecele$Cre=="-", "control", as.character(data_LPS_acutecele$Tamoxifen_dose))
data_LPS_celediet$Age=as.numeric(as.Date(as.character(data_LPS_celediet$Sac_date),"%m/%d/%Y")-as.Date(as.character(data_LPS_celediet$DOB),"%m/%d/%Y"))
data_LPS_celediet$TM_time=as.numeric(as.Date(as.character(data_LPS_celediet$Sac_date),"%m/%d/%Y")-as.Date(as.character(data_LPS_celediet$TM_date),"%m/%d/%Y"))
data_LPS_celediet$Study=as.factor(data_LPS_celediet$Study)
data_LPS_acutecele$Age=as.numeric(as.Date(as.character(data_LPS_acutecele$Sac_date),"%m/%d/%Y")-as.Date(as.character(data_LPS_acutecele$DOB),"%m/%d/%Y"))
data_LPS_acutecele$TM_time=as.numeric(as.Date(as.character(data_LPS_acutecele$Sac_date),"%m/%d/%Y")-as.Date(as.character(data_LPS_acutecele$TM_date),"%m/%d/%Y"))
data_LPS_acutecele$Study=as.factor(data_LPS_acutecele$Study)

data_LPS_cele_all=merge(data_LPS_celediet, data_LPS_acutecele, all=TRUE)

data_LPS_cele_all$Kidney_tertile= with(data_LPS_cele_all, cut (Renal_medulla_Ptgs2, breaks=quantile(Renal_medulla_Ptgs2, probs=seq(0,1,by=1/3), na.rm=TRUE),include.lowest=TRUE))
data_LPS_cele_all$Lung_tertile= with(data_LPS_cele_all, cut (Lung_Ptgs2, breaks=quantile(Lung_Ptgs2, probs=seq(0,1,by=1/3), na.rm=TRUE),include.lowest=TRUE))
data_LPS_cele_all$KO_Group=ifelse(data_LPS_cele_all$Cre=="-", "control", as.character(data_LPS_cele_all$Tamoxifen_dose))
data_LPS_cele_all$Celecoxib.LPS_trough[is.na(data_LPS_cele_all$Celecoxib.LPS_trough)]=0

data_LPS_cele_all$NSAID_route=as.factor(ifelse(data_LPS_cele_all$Study==5|data_LPS_cele_all$Study==8 | data_LPS_cele_all$Study==9, "gavage", "diet"))
data_LPS_cele_all$Age=as.numeric(as.Date(as.character(data_LPS_cele_all$Sac_date),"%m/%d/%Y")-as.Date(as.character(data_LPS_cele_all$DOB),"%m/%d/%Y"))
data_LPS_cele_all$TM_time=as.numeric(as.Date(as.character(data_LPS_cele_all$Sac_date),"%m/%d/%Y")-as.Date(as.character(data_LPS_cele_all$TM_date),"%m/%d/%Y"))
data_LPS_cele_all$Study=as.factor(data_LPS_cele_all$Study)
data_LPS_cele_male=data_LPS_cele_all[which(data_LPS_cele_all$Sex=="M"),]
data_LPS_cele_female=data_LPS_cele_all[which(data_LPS_cele_all$Sex=="F"),]

##Splitting by study to calculate mean PGIM for control mice, normalizing, then re-merging##
Study4=data_LPS_cele_all[data_LPS_cele_all$Study==4,]
Study5=data_LPS_cele_all[data_LPS_cele_all$Study==5,]
Study6=data_LPS_cele_all[data_LPS_cele_all$Study==6,]
Study8=data_LPS_cele_all[data_LPS_cele_all$Study==8,]
Study9=data_LPS_cele_all[data_LPS_cele_all$Study==9,]
Study13=data_LPS_cele_all[data_LPS_cele_all$Study==13,]
Study14=data_LPS_cele_all[data_LPS_cele_all$Study==14,]
Study16=data_LPS_cele_all[data_LPS_cele_all$Study==16,]
Study4$PGIM_control_mean=as.numeric(mean(subset(Study4$PGIM_stimulated, Study4$Treatment=="none")))
Study5$PGIM_control_mean=as.numeric(mean(subset(Study5$PGIM_stimulated, Study5$Treatment=="vehicle"))) 
Study6$PGIM_control_mean=as.numeric(mean(subset(Study6$PGIM_stimulated, Study6$Treatment=="none")))
Study8$PGIM_control_mean=as.numeric(mean(subset(Study8$PGIM_stimulated, Study8$Treatment=="vehicle"))) 
Study9$PGIM_control_mean=as.numeric(mean(subset(Study9$PGIM_stimulated, Study9$Treatment=="vehicle"))) 
Study13$PGIM_control_mean=as.numeric(mean(subset(Study13$PGIM_stimulated, Study13$Treatment=="none")))
Study14$PGIM_control_mean=as.numeric(mean(subset(Study14$PGIM_stimulated, Study14$Treatment=="none")))
Study16$PGIM_control_mean=as.numeric(mean(subset(Study16$PGIM_stimulated, Study16$Treatment=="none")))

Study4.6=merge(Study4, Study6, all=TRUE)
Study13.14=merge(Study13, Study14, all=TRUE)
Study13.14.16=merge(Study13.14, Study16, all=TRUE)
data_LPS_celediet=merge(Study4.6, Study13.14.16, all=TRUE)

Study8.9=merge(Study8, Study9, all=TRUE)
data_LPS_acutecele=merge(Study5, Study8.9, all=TRUE)

data_LPS_celediet$PGIM_norm=data_LPS_celediet$PGIM_stimulated/data_LPS_celediet$PGIM_control_mean
data_LPS_acutecele$PGIM_norm=data_LPS_acutecele$PGIM_stimulated/data_LPS_acutecele$PGIM_control_mean

data_LPS_cele_all=merge(data_LPS_celediet, data_LPS_acutecele, all=TRUE)
data_LPS_cele_all$Tx=ifelse(data_LPS_cele_all$Treatment=="none"|data_LPS_cele_all$Treatment=="vehicle", "control", "celecoxib")


##Graphs##
library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))
ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough, PGIM_stimulated))+geom_point()+scale_x_log10()+ pub_specs
ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough, PGIM_stimulated, color=Lung_tertile))+geom_point()+scale_x_log10()+ pub_specs

pdf("Response_by_exp.pdf")
ggplot(subset(data_LPS_cele_all,!is.na(data_LPS_cele_all$Kidney_tertile)), aes(Kidney_tertile, PGIM_norm, color=Tx))+geom_boxplot()+ labs(x="Renal Medulla COX-2 Expression Tertile", y="Urinary PGIM\n(relative to control)")+pub_specs+coord_fixed()+guides(color=guide_legend("Treatment"))
ggplot(subset(data_LPS_cele_all,!is.na(data_LPS_cele_all$Lung_tertile)), aes(Lung_tertile, PGIM_norm, color=Tx))+geom_boxplot()+ labs(x="Lung COX-2 Expression Tertile", y="Urinary PGIM\n(relative to control)")+ pub_specs+coord_fixed()+guides(color=guide_legend("Treatment"))
ggplot(data_LPS_cele_all, aes(Renal_medulla_Ptgs2, PGIM_norm, color=Tx))+geom_point()+ labs(x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs + annotate("text", x = 0.012, y = 2.1, label = "Control: r=0.36, p<0.01")+ annotate("text", x = 0.012, y = 2, label = "Celecoxib: r=0.23, p<0.01")+coord_fixed(ratio=1/200)+guides(color=guide_legend("Treatment"))
ggplot(data_LPS_cele_all, aes(Lung_Ptgs2, PGIM_norm, color=Tx))+geom_point()+labs(x="Lung COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.04, y = 2.1, label = "Control: r=0.25, p=0.0106")+ annotate("text", x = 0.04, y = 2, label = "Celecoxib: r=0.26, p<0.01")+coord_fixed(ratio=1/50)+guides(color=guide_legend("Treatment"))
ggplot(data_LPS_cele_all, aes(Renal_medulla_Ptgs2, PGIM_norm, color=Tx))+geom_point()+ geom_smooth(method="lm", se=FALSE)+ labs(x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.012, y = 2.1, label = "Control: r=0.36, p<0.01")+ annotate("text", x = 0.012, y = 2, label = "Celecoxib: r=0.23, p<0.01")+coord_fixed(ratio=1/200)+guides(color=guide_legend("Treatment"))
ggplot(data_LPS_cele_all, aes(Lung_Ptgs2, PGIM_norm, color=Tx))+geom_point()+ geom_smooth(method="lm", se=FALSE)+labs(x="Lung COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.04, y = 2.1, label = "Control: r=0.25, p=0.0106")+ annotate("text", x = 0.04, y = 2, label = "Celecoxib: r=0.26, p<0.01")+coord_fixed(ratio=1/50)+guides(color=guide_legend("Treatment"))
ggplot(subset(data_LPS_cele_all,Tx=="celecoxib"), aes(Renal_medulla_Ptgs2, PGIM_norm))+geom_point()+ labs(title="Celecoxib",x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.012, y = 1.6, label = "r=0.23, p<0.01")+coord_fixed(ratio=1/200)
ggplot(subset(data_LPS_cele_all,Tx=="celecoxib"), aes(Lung_Ptgs2, PGIM_norm))+geom_point()+ labs(title="Celecoxib",x="Lung COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.04, y = 1.6, label = "r=0.26, p<0.01")+coord_fixed(ratio=1/50)
ggplot(subset(data_LPS_cele_all,Tx=="control"), aes(Renal_medulla_Ptgs2, PGIM_norm))+geom_point()+ labs(title="Control",x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.01, y = 2.1, label = "r=0.36, p<0.01")+coord_fixed(ratio=1/200)
ggplot(subset(data_LPS_cele_all,Tx=="control"), aes(Lung_Ptgs2, PGIM_norm))+geom_point()+ labs(title="Control",x="Lung COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.04, y = 2.1, label = "r=0.25, p=0.0106")+coord_fixed(ratio=1/50)
dev.off()

data_cele=subset(data_LPS_cele_all,Tx=="celecoxib")
data_control=subset(data_LPS_cele_all,Tx=="control")
cor(data_cele$PGIM_norm, data_cele$Renal_medulla_Ptgs2, method="spearman")
cor(data_cele$PGIM_norm, data_cele$Lung_Ptgs2, method="spearman")
cor(data_control$PGIM_norm, data_control$Renal_medulla_Ptgs2, method="spearman")
cor(data_control$PGIM_norm, data_control$Lung_Ptgs2, method="spearman")

library(Hmisc)
rcorr(data_cele$PGIM_norm, data_cele$Renal_medulla_Ptgs2, type="spearman")
rcorr(data_cele$PGIM_norm, data_cele$Lung_Ptgs2, type="spearman")
rcorr(data_control$PGIM_norm, data_control$Renal_medulla_Ptgs2, type="spearman")
rcorr(data_control$PGIM_norm, data_control$Lung_Ptgs2, type="spearman")

ggplot(subset(data_LPS_cele_all,data_LPS_cele_all$KO_Group=="control"|data_LPS_cele_all$KO_Group=="100x5"|data_LPS_cele_all$KO_Group=="20x3"), aes(KO_Group, PGIM_norm, color=Tx))+geom_boxplot()+ pub_specs+coord_fixed()+labs(x="KO Group", y="Urinary PGIM\n(relative to control)")+guides(color=guide_legend("Treatment"))

anova(lm(PGIM_norm~Tx*Kidney_tertile,subset(data_LPS_cele_all,!is.na(data_LPS_cele_all$Kidney_tertile))))
anova(lm(PGIM_norm~Tx*Lung_tertile,subset(data_LPS_cele_all,!is.na(data_LPS_cele_all$Lung_tertile))))
anova(lm(PGIM_norm~Tx*KO_Group,subset(data_LPS_cele_all,data_LPS_cele_all$KO_Group=="control"|data_LPS_cele_all$KO_Group=="100x5"|data_LPS_cele_all$KO_Group=="20x3")))
ggplot(data_LPS_cele_all, aes(Tx, PGIM_norm, color=Kidney_tertile, fill=Kidney_tertile))+geom_boxplot()+ pub_specs

##Modeling##
library(nlme)
data_all=groupedData(data=data_LPS_cele_all[which(!is.na(data_LPS_cele_all$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Mouse_ID, outer=~Study*Sex*KO_Group)
data_diet=groupedData(data=data_LPS_celediet[which(!is.na(data_LPS_celediet$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Mouse_ID, outer=~Study*Sex*KO_Group)
data_gavage=groupedData(data=data_LPS_acutecele[which(!is.na(data_LPS_acutecele$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Mouse_ID, outer=~Study*Sex*KO_Group)


##Base model (Hill coefficient=1) with no random effects ##
norm=nls(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	start=list(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxIter = 500))

##Base model adding in covariates,Power model##
exp.norm=nls(PGIM_norm~((PGIM*Renal_medulla_Ptgs2^beta1*Lung_Ptgs1^beta2)-(((Imax*Renal_medulla_Ptgs2^beta3)*Celecoxib.LPS_trough)/((IC50*Renal_medulla_Ptgs2^beta4)+Celecoxib.LPS_trough))),
	start=list(PGIM=1,beta1=0, beta2=0,beta3=0,Imax=0.8, IC50=0.5, beta4=0), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))

exp.norm1=nls(PGIM_norm~((PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))*Renal_medulla_Ptgs2^beta1*Renal_medulla_Ptgs1^beta2),
	start=list(PGIM=1,Imax=0.8, IC50=0.5, beta1=0.05, beta2=0.05), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))

##Base model adding in covariates,Additive model##
exp.norm2=nls(PGIM_norm~((PGIM+beta1*Renal_medulla_Ptgs2+beta2*Lung_Ptgs1)-(((Imax+Renal_medulla_Ptgs2*beta3)*Celecoxib.LPS_trough)/((IC50+Renal_medulla_Ptgs2*beta4)+Celecoxib.LPS_trough))),
	start=list(PGIM=1,beta1=100, beta2=-5,beta3=100,Imax=0.8, IC50=0.5, beta4=100), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))

exp.norm3=nls(PGIM_norm~((PGIM+beta1*Renal_medulla_Ptgs2+beta2*Lung_Ptgs1+beta3*(Sex=="M"))-(((Imax+Renal_medulla_Ptgs2*beta4)*Celecoxib.LPS_trough)/((IC50+Renal_medulla_Ptgs2*beta5)+Celecoxib.LPS_trough))),
	start=list(PGIM=1,beta1=100, beta2=-5,beta3=100,Imax=0.8, beta4=100,IC50=0.5, beta5=100), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))

	
##Model with random effects on PGIM, Hill coefficient=1##
norm.nlme1=nlme(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=PGIM~1,
	start=c(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude, 
	control=list(maxIter=500, pnlsTol=1e-6, tolerance=2e-5))

norm1.ranef=ranef(norm.nlme1, augFrame=TRUE, data=data_all)
plot(norm1.ranef, form=PGIM~Lung_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs1+Age+Sex+Renal_medulla_Ptgs2)
plot(norm1.ranef, form=PGIM~Renal_medulla_Ptgs2+Age+Sex+Lung_Ptgs1)

norm.nlme1.1=update(norm.nlme1,
	random=PGIM~1|Cage)

norm.nlme2=nlme(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=pdDiag(PGIM+Imax+IC50~1),
	start=c(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude, 
	verbose=TRUE,
	control=list(maxiter=500, pnlsTol=1e-6, tolerance=0.01))
norm2.ranef=ranef(norm.nlme2, augFrame=TRUE, data=data_all)
plot(norm2.ranef, form=PGIM~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Age+Sex+Lung_Ptgs1)
plot(norm2.ranef, form=Imax~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Age+Sex+Lung_Ptgs1)
plot(norm2.ranef, form=IC50~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Age+Sex+Lung_Ptgs1)
plot(norm2.ranef, form=Imax~Kidney_tertile+Lung_tertile)
plot(norm2.ranef, form=IC50~Kidney_tertile+Lung_tertile)

##Random effect on PGIM only##

exp.normnlme1=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Imax=0.8, IC50=0.5),
	verbose=TRUE,
	control = list(maxIter = 500, tolerance=1e-4))

exp.normnlme1.1=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxIter = 500, tolerance=1e-4))

exp.normnlme1.2=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8, IC50=0.1, Renal_medulla_Ptgs2=0),
	control = (maxIter = 500))

exp.normnlme1.2.1=update(exp.normnlme1.2, weights=varPower(),
	na.action=na.pass,
	control =list(maxIter = 500, tolerance=0.0015))

exp.normnlme1.3=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1|Cage,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8, IC50=0.1, Renal_medulla_Ptgs2=0),
	control = (maxIter = 500))

exp.normnlme1.3.1=update(exp.normnlme1.3, weights=varPower(),
	na.action=na.pass,
	control =list(maxIter = 500, tolerance=0.0015))


##Random effect on all parameters##
exp.normnlme2=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2, Imax~1,IC50~1),
	random=pdDiag(PGIM+Imax+IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=1000, Imax=0.8, IC50=0.5),
	control = list(maxIter = 500, tolerance=1e-4))

exp.normnlme2.1=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=pdDiag(PGIM+Imax+IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxIter = 500, tolerance=1e-4))

exp.normnlme2.2=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2,IC50~Renal_medulla_Ptgs2),
	random=pdDiag(PGIM+Imax+IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8, IC50=0.1, Renal_medulla_Ptgs2=0),
	control =list(maxIter = 500, tolerance=0.0015))

exp.normnlme2.2.1=update(exp.normnlme2.2, weights=varPower(),
	na.action=na.pass,
	start=c(PGIM=1,Renal_medulla_Ptgs2=10, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=10,Imax=0.8, IC50=0.1, Renal_medulla_Ptgs2=0),
	control =list(maxIter = 500, pnlsTol=1e-6, tolerance=0.0015))


##Adding in Hill coefficient parameter##
norm3=nls(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough^gamma)/(IC50^gamma+Celecoxib.LPS_trough^gamma))), 
	start=list(PGIM=1,Imax=0.8, IC50=0.5, gamma=1), 
	na.action=na.exclude, 
	data=data_all,
	control=list(maxiter=500))

norm.nlme3=nlme(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough^gamma)/(IC50^gamma+Celecoxib.LPS_trough^gamma))), 
	data=data_all,
	fixed=PGIM+Imax+IC50+gamma~1,
	random=PGIM~1,
	start=c(PGIM=1,Imax=0.8, IC50=0.1, gamma=0.5), 
	na.action=na.exclude, 
	verbose=TRUE,
	control=list(maxIter=500, pnlsTol=1e-6, tolerance=.1))

exp.normnlme3.1=update(norm.nlme3, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~1,IC50~1, gamma~1),
	random=PGIM~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Imax=0.8, IC50=0.1, gamma=1),
	verbose=TRUE,
	control = list(maxIter = 500, tolerance=0.01))

exp.normnlme3.2=update(norm.nlme3, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2,IC50~Renal_medulla_Ptgs2, gamma~1),
	random=PGIM~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8, IC50=0.1, Renal_medulla_Ptgs2=0, gamma=1),
	verbose=TRUE,
	control = list(maxIter = 500, tolerance=0.01))##will not converge##




pdf("NLME_Diagnostic_plots_v2.pdf")
ggplot(data_all, aes(Celecoxib.LPS_trough,PGIM_norm))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="Observed Concentration-Response",y="Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(norm)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="Base Nonlinear Model",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)
ggplot(data_all, aes(predict(norm), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="Base Nonlinear Model",x="Predicted Normalized PGIM", y="Observed Urinary PGIM\n(relative to control)")
ggplot(data_all, aes(predict(norm),resid(norm)))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed(1/5)+labs(title="Base Nonlinear Model",x="Predicted Normalized PGIM", y="Residuals")
ggplot(data_all, aes(sample=resid(norm, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+pub_specs+coord_fixed()+labs(title="QQ-plot: Base Nonlinear Model",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(norm3)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="Base Nonlinear Model with Hill Coefficient",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)
ggplot(data_all, aes(predict(norm3), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="Base Nonlinear Model with Hill Coefficient",x="Predicted Normalized PGIM", y="Observed Normalized PGIM")
ggplot(data_all, aes(predict(norm3),resid(norm3)))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed(ratio=1/5)+labs(title="Base Nonlinear Model with Hill Coefficient",x="Predicted Normalized PGIM", y="Residuals")
ggplot(data_all, aes(sample=resid(norm3, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+ pub_specs+coord_fixed()+labs(title="QQ-plot: Base Nonlinear Model with Hill Coefficient",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(exp.norm1)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="Nonlinear Model with Covariates\n(Power Model)",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)
ggplot(data_all, aes(predict(exp.norm1), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="Nonlinear Model with Covariates\n(Power Model)",x="Predicted Normalized PGIM", y="Observed Normalized PGIM")
ggplot(data_all, aes(predict(exp.norm1),resid(exp.norm1)))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed(ratio=1/5)+labs(title="Nonlinear Model with Covariates\n(Power Model)",x="Predicted Normalized PGIM", y="Residuals")
ggplot(data_all, aes(sample=resid(exp.norm1, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+ pub_specs+coord_fixed()+labs(title="QQ-plot: Nonlinear Model with Covariates\n(Power Model)",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(norm.nlme1)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="Base NLME Model (Hill Coefficient=1)",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)
ggplot(data_all, aes(predict(norm.nlme1), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="Base NLME Model (Hill Coefficient=1)",x="Predicted Normalized PGIM", y="Observed Normalized PGIM")
ggplot(data_all, aes(predict(norm.nlme1),resid(norm.nlme1, type="pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed()+labs(title="Base NLME Model (Hill Coefficient=1)",x="Predicted Normalized PGIM", y="Residuals")
ggplot(data_all, aes(sample=resid(norm.nlme1, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+ pub_specs+coord_fixed(2.5)+labs(title="QQ-plot: Base NLME Model (Hill Coefficient=1)",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(norm.nlme3)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="Base NLME Model with Hill Coefficient",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)
ggplot(data_all, aes(predict(norm.nlme3), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="Base NLME Model with Hill Coefficient",x="Predicted Normalized PGIM", y="Observed Normalized PGIM")
ggplot(data_all, aes(predict(norm.nlme3),resid(norm.nlme3, type="pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed()+labs(title="Base NLME Model with Hill Coefficient",x="Predicted Normalized PGIM", y="Residuals")
ggplot(data_all, aes(sample=resid(norm.nlme3, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+ pub_specs+coord_fixed(2.5)+labs(title="QQ-plot: Base NLME Model with Hill Coefficient",x="Theoretical Quantile", y="Residuals")

plot(norm1.ranef, form=PGIM~Lung_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs1+Age+Sex+Renal_medulla_Ptgs2)

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(exp.normnlme1)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="NLME Model+Covariates on PGIM\n(Hill Coefficient=1)",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)
ggplot(data_all, aes(predict(exp.normnlme1), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="NLME Model+Covariates on PGIM\n(Hill Coefficient=1)",x="Predicted Normalized PGIM", y="Observed Normalized PGIM")
ggplot(data_all, aes(predict(exp.normnlme1),resid(exp.normnlme1, type="pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed()+labs(title="NLME Model+Covariates on PGIM\n(Hill Coefficient=1)",x="Predicted Normalized PGIM", y="Residuals")
ggplot(data_all, aes(sample=resid(exp.normnlme1, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+ pub_specs+coord_fixed(2)+labs(title="QQ-plot: NLME Model+Covariates on PGIM\n(Hill Coefficient=1)",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(exp.normnlme3.1)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="NLME Model+Covariates on PGIM with Hill Coefficient",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)
ggplot(data_all, aes(predict(exp.normnlme3.1), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="NLME Model+Covariates on PGIM with Hill Coefficient",x="Predicted Normalized PGIM", y="Observed Normalized PGIM")
ggplot(data_all, aes(predict(exp.normnlme3.1),resid(exp.normnlme3.1, type="pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed()+labs(title="NLME Model+Covariates on PGIM with Hill Coefficient",x="Predicted Normalized PGIM", y="Residuals")
ggplot(data_all, aes(sample=resid(exp.normnlme3.1, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+ pub_specs+coord_fixed()+labs(title="QQ-plot: NLME Model+Covariates on PGIM with Hill Coefficient",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(exp.normnlme1.2)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="NLME Model+Covariates on PGIM, Imax, & IC50\n(Hill Coefficient=1)",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)
ggplot(data_all, aes(predict(exp.normnlme1.2), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="NLME Model+Covariates on PGIM, Imax, & IC50\n(Hill Coefficient=1)",x="Predicted Normalized PGIM", y="Observed Normalized PGIM")
ggplot(data_all, aes(predict(exp.normnlme1.2),resid(exp.normnlme1.2, type="pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed()+labs(title="NLME Model+Covariates on PGIM, Imax, & IC50\n(Hill Coefficient=1)",x="Predicted Urinary PGIM\n(relative to control)", y="Residuals")
ggplot(data_all, aes(sample=resid(exp.normnlme1.2, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+ pub_specs+coord_fixed()+labs(title="QQ-plot: NLME Model+Covariates on PGIM, Imax, & IC50\n(Hill Coefficient=1)",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(exp.normnlme1.2.1)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="Weighted NLME Model+Covariates on PGIM, Imax, & IC50\n(Hill Coefficient=1)",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)+coord_fixed()
ggplot(data_all, aes(predict(exp.normnlme1.2.1), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="Weighted NLME Model+Covariates on PGIM, Imax, & IC50\n(Hill Coefficient=1)",x="Predicted Normalized PGIM", y="Observed Normalized PGIM")
ggplot(data_all, aes(predict(exp.normnlme1.2.1),residuals(exp.normnlme1.2.1,type= "pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed(1/4)+labs(title="Weighted NLME Model+Covariates on PGIM, Imax, & IC50\n(Hill Coefficient=1)",x="Predicted Normalized PGIM", y="Residuals")
ggplot(data_all, aes(sample=resid(exp.normnlme1.2.1, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+ pub_specs+coord_fixed()+labs(title="QQ-plot: Weighted NLME Model+Covariates on PGIM, Imax, & IC50\n(Hill Coefficient=1)",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(exp.normnlme1.3)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="NLME Model+Covariates on PGIM, Imax, & IC50\n Random Effects by Cage (Hill Coefficient=1)",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)
ggplot(data_all, aes(predict(exp.normnlme1.3), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="NLME Model+Covariates on PGIM, Imax, & IC50\nRandom Effects by Cage (Hill Coefficient=1)",x="Predicted Normalized PGIM", y="Observed Normalized PGIM")
ggplot(data_all, aes(predict(exp.normnlme1.3),resid(exp.normnlme1.3, type="pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed()+labs(title="NLME Model+Covariates on PGIM, Imax, & IC50\nRandom Effects by Cage (Hill Coefficient=1)",x="Predicted Urinary PGIM\n(relative to control)", y="Residuals")
ggplot(data_all, aes(sample=resid(exp.normnlme1.3, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+ pub_specs+coord_fixed()+labs(title="QQ-plot: NLME Model+Covariates on PGIM, Imax, & IC50\nRandom Effects by Cage (Hill Coefficient=1)",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(exp.normnlme1.3.1)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="Weighted NLME Model+Covariates on PGIM, Imax, & IC50\nRandom Effects by Cage (Hill Coefficient=1)",y="Predicted Normalized PGIM", x="Celecoxib Plasma Concentration\n(micromolar)")+ylim(0,3)+coord_fixed()
ggplot(data_all, aes(predict(exp.normnlme1.3.1), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="Weighted NLME Model+Covariates on PGIM, Imax, & IC50\nRandom Effects by Cage (Hill Coefficient=1)",x="Predicted Normalized PGIM", y="Observed Normalized PGIM")
ggplot(data_all, aes(predict(exp.normnlme1.3.1),residuals(exp.normnlme1.3.1,type= "pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed(1/4)+labs(title="Weighted NLME Model+Covariates on PGIM, Imax, & IC50\nRandom Effects by Cage (Hill Coefficient=1)",x="Predicted Normalized PGIM", y="Residuals")
ggplot(data_all, aes(sample=resid(exp.normnlme1.3.1, type="pearson")))+stat_qq()+geom_abline(slope=1, intercept=0, size=1)+ pub_specs+coord_fixed()+labs(title="QQ-plot: Weighted NLME Model+Covariates on PGIM, Imax, & IC50\nRandom Effects by Cage (Hill Coefficient=1)",x="Theoretical Quantile", y="Residuals")

dev.off()

qqnorm(resid(exp.normnlme1.2.1, type="pearson"))
shapiro.test(resid(exp.normnlme1.2.1, type="pearson"))
shapiro.test(resid(exp.normnlme1, type="pearson"))
shapiro.test(resid(norm, type="pearson"))

anova(norm,norm3,norm.nlme1,norm.nlme3)
anova(exp.normnlme1,exp.normnlme3.1,exp.normnlme1.2,exp.normnlme1.2.1)

cele_conc_sim=exp(rnorm(100,0,1.5))
cele_sim=as.data.frame(matrix(data=cele_conc_sim,ncol=100, nrow=239, byrow=TRUE))
coefficients=as.data.frame(coef(exp.normnlme1.2.1)[1:8])
sim=cbind(coefficients,cele_sim)

mice=data_all[c("Mouse_ID", "Sex", "Renal_medulla_Ptgs2", "Lung_Ptgs1","Kidney_tertile", "Celecoxib.LPS_trough")]
mice$Sex2=ifelse(mice$Sex=="F", as.numeric("0"), as.numeric("1"))
mice_sim=cbind(mice,sim,all=TRUE)

mice_sim$PGIM=mice_sim[8]+(mice_sim$PGIM.Renal_medulla_Ptgs2*mice_sim$Renal_medulla_Ptgs2)+(mice_sim$PGIM.Lung_Ptgs1*mice_sim$Lung_Ptgs1)+(mice_sim$PGIM.SexM*mice_sim$Sex2)
mice_sim$Imax=mice_sim[12]+mice_sim$Imax.Renal_medulla_Ptgs2*mice_sim$Renal_medulla_Ptgs2
mice_sim$IC50=mice_sim[14]+mice_sim$IC50.Renal_medulla_Ptgs2*mice_sim$Renal_medulla_Ptgs2

library(reshape)
mice_sim2=melt(mice_sim,id.vars=c("Mouse_ID", "Sex", "Renal_medulla_Ptgs2", "Lung_Ptgs1","Kidney_tertile","Sex2","PGIM.(Intercept)","PGIM.Renal_medulla_Ptgs2","PGIM.Lung_Ptgs1","PGIM.SexM","Imax.(Intercept)","Imax.Renal_medulla_Ptgs2","IC50.(Intercept)","IC50.Renal_medulla_Ptgs2")) 
mice_sim2$PGIM=mice_sim2[7]+(mice_sim2$PGIM.Renal_medulla_Ptgs2*mice_sim2$Renal_medulla_Ptgs2)+(mice_sim2$PGIM.Lung_Ptgs1*mice_sim2$Lung_Ptgs1)+(mice_sim2$PGIM.SexM*mice_sim2$Sex2)
mice_sim2$Imax=mice_sim2[11]+mice_sim2$Imax.Renal_medulla_Ptgs2*mice_sim2$Renal_medulla_Ptgs2
mice_sim2$IC50=mice_sim2[13]+mice_sim2$IC50.Renal_medulla_Ptgs2*mice_sim2$Renal_medulla_Ptgs2
##Inhibition=function(PGIM,Imax,IC50,C){
	for (i in C){
	PGIM_sim=PGIM-((Imax*i)/(IC50+i))
	i=i+1
	}}
##mice_sim2$PGIM_sim=Inhibition(mice_sim$PGIM,mice_sim$Imax,mice_sim$IC50,C=(1:10))###

mice_sim2$PGIM_sim=mice_sim2$PGIM-((mice_sim2$Imax*mice_sim2$value)/(mice_sim2$IC50+mice_sim2$value))

pdf("Simulated_curves.pdf")
ggplot(mice_sim2, aes(value,PGIM_sim, color=Kidney_tertile))+geom_point(alpha=0.3)+ pub_specs+scale_x_log10()+coord_fixed(2)+labs(title="Simulated Concentration-Response",y="Predicted Normalized PGIM", x="Simulated Celecoxib Plasma Concentration\n(micromolar)")+theme(legend.position="none")+ylim(0,2)
ggplot(sim_kidneylow, aes(value,PGIM_sim))+geom_point(alpha=0.3, color="red")+ pub_specs+scale_x_log10()+coord_fixed(2)+labs(title="Simulated Concentration-Response",y="Predicted Normalized PGIM", x="Simulated Celecoxib Plasma Concentration\n(micromolar)")+theme(legend.position="none")+ylim(0,2)
ggplot(sim_kidneymid, aes(value,PGIM_sim))+geom_point(alpha=0.3, color="green")+ pub_specs+scale_x_log10()+coord_fixed(2)+labs(title="Simulated Concentration-Response",y="Predicted Normalized PGIM", x="Simulated Celecoxib Plasma Concentration\n(micromolar)")+theme(legend.position="none")+ylim(0,2)
ggplot(sim_kidneyhigh, aes(value,PGIM_sim))+geom_point(alpha=0.3, color="blue")+ pub_specs+scale_x_log10()+coord_fixed(2)+labs(title="Simulated Concentration-Response",y="Predicted Normalized PGIM", x="Simulated Celecoxib Plasma Concentration\n(micromolar)")+theme(legend.position="none")+ylim(0,2)
ggplot(mice_sim2, aes(value,PGIM_sim, color=Kidney_tertile))+geom_smooth()+ pub_specs+scale_x_log10()+coord_fixed(2)+labs(title="Simulated Concentration-Response",y="Predicted Normalized PGIM", x="Simulated Celecoxib Plasma Concentration\n(micromolar)")+theme(legend.position="none")+ylim(0,2)
ggplot(IC70_all,aes(Kidney_tertile,inhibition, color=Kidney_tertile))+geom_boxplot(aes(fill=Kidney_tertile,alpha=0.01))+pub_specs+ylim(0,100)+theme(legend.position="none")+labs(title="Percent Inhibition at IC70", x="Kidney Ptgs2 Expression Tertile", y="Percent Inhibition")
ggplot(IC70_all,aes(Kidney_tertile,PGIM_sim$PGIM_sim, color=Kidney_tertile))+geom_boxplot(aes(fill=Kidney_tertile,alpha=0.01))+pub_specs+ylim(0,1)+theme(legend.position="none")+labs(title="Percent Inhibition at IC70", x="Kidney Ptgs2 Expression Tertile", y="Predicted Normalized PGIM")
ggplot(mice_sim2,aes(value,inhibition,color=Kidney_tertile))+geom_point(alpha=0.3)+scale_x_log10()+theme(legend.position="none")+pub_specs+ylim(0,100)+labs(title="Percent Inhibition of PGIM",x="Simulated Celecoxib Plasma Concentration\n(micromolar)",y="Percent Inhibition")
ggplot(sim_kidneylow,aes(value,inhibition,color=Kidney_tertile))+geom_point(alpha=0.3, color="red")+scale_x_log10()+theme(legend.position="none")+pub_specs+ylim(0,100)+labs(title="Percent Inhibition of PGIM",x="Simulated Celecoxib Plasma Concentration\n(micromolar)",y="Percent Inhibition")
ggplot(sim_kidneymid,aes(value,inhibition,color=Kidney_tertile))+geom_point(alpha=0.3, color="green")+scale_x_log10()+theme(legend.position="none")+pub_specs+ylim(0,100)+labs(title="Percent Inhibition of PGIM",x="Simulated Celecoxib Plasma Concentration\n(micromolar)",y="Percent Inhibition")
ggplot(sim_kidneyhigh,aes(value,inhibition,color=Kidney_tertile))+geom_point(alpha=0.3, color="blue")+scale_x_log10()+theme(legend.position="none")+pub_specs+ylim(0,100)+labs(title="Percent Inhibition of PGIM",x="Simulated Celecoxib Plasma Concentration\n(micromolar)",y="Percent Inhibition")
ggplot(mice_sim2,aes(value,inhibition,color=Kidney_tertile))+geom_smooth()+scale_x_log10()+theme(legend.position="none")+pub_specs+ylim(0,100)+labs(title="Percent Inhibition of PGIM",x="Simulated Celecoxib Plasma Concentration\n(micromolar)",y="Percent Inhibition")
dev.off()

names(mice_sim2$PGIM_sim)[names(mice_sim2$PGIM_sim)=="PGIM.(Intercept)"] = "PGIM_sim"
names(mice_sim2$PGIM)[names(mice_sim2$PGIM)=="PGIM.(Intercept)"] = "PGIM"
mice_sim2$percent=mice_sim2$PGIM_sim*100/mice_sim2$PGIM
mice_sim2$inhibition=(mice_sim2$PGIM$PGIM-mice_sim2$PGIM_sim$PGIM_sim)*100/mice_sim2$PGIM$PGIM
sim_kidneylow=mice_sim2[which(as.numeric(mice_sim2$Kidney_tertile)=="1"),]
sim_kidneymid=mice_sim2[which(as.numeric(mice_sim2$Kidney_tertile)=="2"),]
sim_kidneyhigh=mice_sim2[which(as.numeric(mice_sim2$Kidney_tertile)=="3"),]

IC70upper=sim_kidneyhigh[which(sim_kidneyhigh$inhibition<=72),]
IC70=IC70upper[which(IC70upper$inhibition>=68),]
summary(IC70$value)

IC70upper2=sim_kidneyhigh[which(sim_kidneyhigh$PGIM_sim$PGIM_sim<=0.72),]
IC702=IC70upper2[which(IC70upper$PGIM_sim$PGIM_sim>=0.68),]
summary(IC702$value)

IC70_allupper=mice_sim2[which(mice_sim2$value<=6),]
IC70_all=IC70_allupper[which(IC70_allupper$value>=3),]

IC70_allupper2=mice_sim2[which(mice_sim2$value<=4.1),]
IC70_all2=IC70_allupper[which(IC70_allupper$value>=0.8),]

names(IC70_all$inhibition)[names(IC70_all$inhibition)=="PGIM.(Intercept)"] = "Inhibition"
names(IC70_all$PGIM_sim)[names(IC70_all$PGIM_sim)=="PGIM.(Intercept)"] = "PGIM_sim"
names(IC70_all2$PGIM_sim)[names(IC70_all2$PGIM_sim)=="PGIM.(Intercept)"] = "PGIM_sim"