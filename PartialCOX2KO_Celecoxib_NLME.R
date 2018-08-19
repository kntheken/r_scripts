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
for (i in (1:279)){
	if (data_LPS_cele_all$KO_Group[i]=="100x5") {
		data_LPS_cele_all$PGIM_control[i]=mean(subset(data_LPS_cele_all$PGIM_norm, data_LPS_cele_all$KO_Group=="100x5" & data_LPS_cele_all$Tx=="control" ))
		}
	else if (data_LPS_cele_all$KO_Group[i]=="20x1") {
		data_LPS_cele_all$PGIM_control[i]=mean(subset(data_LPS_cele_all$PGIM_norm, data_LPS_cele_all$KO_Group=="20x1" & data_LPS_cele_all$Tx=="control" ))
		}
	else if (data_LPS_cele_all$KO_Group[i]=="20x3") {
		data_LPS_cele_all$PGIM_control[i]=mean(subset(data_LPS_cele_all$PGIM_norm, data_LPS_cele_all$KO_Group=="20x3" & data_LPS_cele_all$Tx=="control" ))
		}
	else if (data_LPS_cele_all$KO_Group[i]=="20x5") {
		data_LPS_cele_all$PGIM_control[i]=mean(subset(data_LPS_cele_all$PGIM_norm, data_LPS_cele_all$KO_Group=="20x5" & data_LPS_cele_all$Tx=="control" ))
		}
	else if (data_LPS_cele_all$KO_Group[i]=="control") {
		data_LPS_cele_all$PGIM_control[i]=mean(subset(data_LPS_cele_all$PGIM_norm, data_LPS_cele_all$KO_Group=="control" & data_LPS_cele_all$Tx=="control" ))
		}
	else
		data_LPS_cele_all$PGIM_control[i]=NA	
	}
data_LPS_cele_all$PGIM_control_neg=mean(subset(data_LPS_cele_all$PGIM_norm, data_LPS_cele_all$KO_Group=="control" & data_LPS_cele_all$Tx=="control" ))
data_LPS_cele_all$inhib=data_LPS_cele_all$PGIM_norm/data_LPS_cele_all$PGIM_control
data_LPS_cele_all$inhib_norm=data_LPS_cele_all$PGIM_norm/data_LPS_cele_all$PGIM_control_neg
summary(subset(data_LPS_cele_all$inhib,data_LPS_cele_all$KO_Group=="control" & data_LPS_cele_all$Tx=="control" ))
summary(subset(data_LPS_cele_all$inhib,data_LPS_cele_all$KO_Group=="control" & data_LPS_cele_all$Tx=="celecoxib" ))
summary(subset(data_LPS_cele_all$inhib,data_LPS_cele_all$KO_Group=="20x1" & data_LPS_cele_all$Tx=="control" ))
summary(subset(data_LPS_cele_all$inhib,data_LPS_cele_all$KO_Group=="20x1" & data_LPS_cele_all$Tx=="celecoxib" ))
summary(subset(data_LPS_cele_all$inhib,data_LPS_cele_all$KO_Group=="20x3" & data_LPS_cele_all$Tx=="control" ))
summary(subset(data_LPS_cele_all$inhib,data_LPS_cele_all$KO_Group=="20x3" & data_LPS_cele_all$Tx=="celecoxib" ))
summary(subset(data_LPS_cele_all$inhib,data_LPS_cele_all$KO_Group=="20x5" & data_LPS_cele_all$Tx=="control" ))
summary(subset(data_LPS_cele_all$inhib,data_LPS_cele_all$KO_Group=="20x5" & data_LPS_cele_all$Tx=="celecoxib" ))
summary(subset(data_LPS_cele_all$inhib,data_LPS_cele_all$KO_Group=="100x5" & data_LPS_cele_all$Tx=="control" ))
summary(subset(data_LPS_cele_all$inhib,data_LPS_cele_all$KO_Group=="100x5" & data_LPS_cele_all$Tx=="celecoxib" ))

library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))

ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough, PGIM_stimulated))+geom_point()+scale_x_log10()+ pub_specs
ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough, PGIM_stimulated, color=Lung_tertile))+geom_point()+scale_x_log10()+ pub_specs
inhibEmax=PGIM*(1-((Imax*C)/(IC50+C)))
ggplot(data_LPS_cele_all, aes(KO_Group, inhib, color=Tx))+geom_boxplot()+ pub_specs
ggplot(data_LPS_cele_all, aes(KO_Group, inhib_norm, color=Tx))+geom_boxplot()+ pub_specs
ggplot(data_LPS_cele_all, aes(KO_Group, PGIM_norm, color=Tx))+geom_boxplot()+ pub_specs
ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough, inhib, color=KO_Group))+geom_point()+geom_smooth()+scale_x_log10()+ pub_specs


pdf("Response_by_exp.pdf")
ggplot(subset(data_LPS_cele_all,!is.na(data_LPS_cele_all$Kidney_tertile)), aes(Kidney_tertile, PGIM_norm, color=Tx))+geom_boxplot()+ labs(x="Renal Medulla COX-2 Expression Tertile", y="Urinary PGIM\n(relative to control)")+pub_specs+coord_fixed()+theme(legend.position="bottom")+ guides(colour=guide_legend("Treatment"))

ggplot(subset(data_LPS_cele_all,!is.na(data_LPS_cele_all$Lung_tertile)), aes(Lung_tertile, PGIM_norm, color=Tx))+geom_boxplot()+ labs(x="Lung COX-2 Expression Tertile", y="Urinary PGIM\n(relative to control)")+ pub_specs+coord_fixed()+theme(legend.position="bottom")+ guides(colour=guide_legend("Treatment"))

ggplot(data_LPS_cele_all, aes(Renal_medulla_Ptgs2, PGIM_norm, color=Tx))+geom_point()+ labs(x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs + annotate("text", x = 0.012, y = 2.1, label = "Control: r=0.36, p<0.01")+ annotate("text", x = 0.012, y = 2, label = "Celecoxib: r=0.23, p<0.01")+coord_fixed(1/200)+theme(legend.position="bottom")+ guides(colour=guide_legend("Treatment"))

ggplot(data_LPS_cele_all, aes(Lung_Ptgs2, PGIM_norm, color=Tx))+geom_point()+labs(x="Lung COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.04, y = 2.1, label = "Control: r=0.25, p=0.0106")+ annotate("text", x = 0.04, y = 2, label = "Celecoxib: r=0.26, p<0.01")+coord_fixed(1/50)+theme(legend.position="bottom")+ guides(colour=guide_legend("Treatment"))

ggplot(data_LPS_cele_all, aes(Renal_medulla_Ptgs2, PGIM_norm, color=Tx))+geom_point()+ geom_smooth(method="lm", se=FALSE)+ labs(x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.012, y = 2.1, label = "Control: r=0.36, p<0.01")+ annotate("text", x = 0.012, y = 2, label = "Celecoxib: r=0.23, p<0.01")+coord_fixed(1/200)+theme(legend.position="bottom")+ guides(colour=guide_legend("Treatment"))

ggplot(data_LPS_cele_all, aes(Lung_Ptgs2, PGIM_norm, color=Tx))+geom_point()+ geom_smooth(method="lm", se=FALSE)+labs(x="Lung COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.04, y = 2.1, label = "Control: r=0.25, p=0.0106")+ annotate("text", x = 0.04, y = 2, label = "Celecoxib: r=0.26, p<0.01")+coord_fixed(1/50)+theme(legend.position="bottom")+ guides(colour=guide_legend("Treatment"))

ggplot(subset(data_LPS_cele_all,Tx=="celecoxib"), aes(Renal_medulla_Ptgs2, PGIM_norm))+geom_point()+ labs(title="Celecoxib",x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.012, y = 1.6, label = "r=0.23, p<0.01")+coord_fixed(1/200)

ggplot(subset(data_LPS_cele_all,Tx=="celecoxib"), aes(Lung_Ptgs2, PGIM_norm))+geom_point()+ labs(title="Celecoxib",x="Lung COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.04, y = 1.6, label = "r=0.26, p<0.01")+coord_fixed(1/50)

ggplot(subset(data_LPS_cele_all,Tx=="control"), aes(Renal_medulla_Ptgs2, PGIM_norm))+geom_point()+ labs(title="Control",x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.01, y = 2.1, label = "r=0.36, p<0.01")+coord_fixed(1/200)

ggplot(subset(data_LPS_cele_all,Tx=="control"), aes(Lung_Ptgs2, PGIM_norm))+geom_point()+ labs(title="Control",x="Lung COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.04, y = 2.1, label = "r=0.25, p=0.0106")+coord_fixed(1/50)

dev.off()

data_cele=subset(data_LPS_cele_all,Tx=="celecoxib")
data_control=subset(data_LPS_cele_all,Tx=="control")
cor(data_cele$PGIM_norm, log10(data_cele$Renal_medulla_Ptgs2), method="spearman")
cor(data_cele$PGIM_norm, data_cele$Lung_Ptgs2, method="spearman")
cor(data_control$PGIM_norm, log10(data_control$Renal_medulla_Ptgs2), method="spearman")
cor(data_control$PGIM_norm, data_control$Lung_Ptgs2, method="spearman")

library(Hmisc)
rcorr(data_cele$PGIM_norm, data_cele$Renal_medulla_Ptgs2, type="spearman")
rcorr(data_cele$PGIM_norm, data_cele$Lung_Ptgs2, type="spearman")
rcorr(data_control$PGIM_norm, data_control$Renal_medulla_Ptgs2, type="spearman")
rcorr(data_control$PGIM_norm, data_control$Lung_Ptgs2, type="spearman")

ggplot(subset(data_LPS_cele_all,data_LPS_cele_all$KO_Group=="control"|data_LPS_cele_all$KO_Group=="100x5"|data_LPS_cele_all$KO_Group=="20x3"), aes(KO_Group, PGIM_norm, color=Tx))+geom_boxplot()+ pub_specs

anova(lm(PGIM_norm~Tx*Kidney_tertile,subset(data_LPS_cele_all,!is.na(data_LPS_cele_all$Kidney_tertile))))
anova(lm(PGIM_norm~Tx*Lung_tertile,subset(data_LPS_cele_all,!is.na(data_LPS_cele_all$Lung_tertile))))
anova(lm(PGIM_norm~Tx*KO_Group,subset(data_LPS_cele_all,data_LPS_cele_all$KO_Group=="control"|data_LPS_cele_all$KO_Group=="100x5"|data_LPS_cele_all$KO_Group=="20x3")))
ggplot(data_LPS_cele_all, aes(Tx, PGIM_norm, color=Kidney_tertile, fill=Kidney_tertile))+geom_boxplot()+ pub_specs

library(nlme)
data_all=groupedData(data=data_LPS_cele_all[which(!is.na(data_LPS_cele_all$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Mouse_ID, outer=~Study*Sex*KO_Group)
data_diet=groupedData(data=data_LPS_celediet[which(!is.na(data_LPS_celediet$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Mouse_ID, outer=~Study*Sex*KO_Group)
data_gavage=groupedData(data=data_LPS_acutecele[which(!is.na(data_LPS_acutecele$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Mouse_ID, outer=~Study*Sex*KO_Group)

base=nls(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))), 
	start=list(PGIM=50,Imax=0.8, IC50=0.5), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))

base_diet=nls(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))), 
	start=list(PGIM=50,Imax=0.8, IC50=0.5), 
	na.action=na.exclude,
	data=data_diet, 
	control = list(maxiter = 500))

base_gavage=nls(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))), 
	start=list(PGIM=50,Imax=0.8, IC50=0.5), 
	na.action=na.exclude,
	data=data_gavage, 
	control = list(maxiter = 500))

baselog=nls(PGIM_stimulated~(PGIM*(1-(((exp(lImax))*Celecoxib.LPS_trough)/((exp(lIC50))+Celecoxib.LPS_trough)))), 
	start=list(PGIM=50,lImax=-.2, lIC50=-.7), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))

norm=nls(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	start=list(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))

base.nlme1=nlme(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=pdDiag(PGIM~1),
	start=c(PGIM=50,Imax=0.8, IC50=0.5), 
	na.action=na.exclude, 
	verbose=TRUE,
	control=list(maxiter=500))

base1.ranef=ranef(base.nlme1, augFrame=TRUE, data=data_all)
plot(base1.ranef, form=PGIM~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Age+Sex+Lung_Ptgs1)
plot(base1.ranef, form=PGIM~Renal_medulla_Ptgs2+Age+Sex+Lung_Ptgs1)

base.nlme2=update(base.nlme1,
	random=pdDiag(PGIM+Imax+IC50~1))
base2.ranef=ranef(base.nlme2, augFrame=TRUE, data=data_all)
plot(base2.ranef, form=PGIM~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Age+Sex+Lung_Ptgs1)
plot(base2.ranef, form=Imax~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Age+Sex+Lung_Ptgs1)
plot(base2.ranef, form=IC50~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Age+Sex+Lung_Ptgs1)

base.nlme3=update(base.nlme1,
	random=PGIM~1|Study)

exp.base.nlme1=update(base.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~1),
	random=PGIM~1|Study,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1),
	control = list(maxiter = 500))
exp.base.nlme2=update(base.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~1,IC50~1),
	random=PGIM~1|Study,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Imax=0.8, IC50=0.1),
	control = list(maxiter = 500))

exp.base.nlme3=update(base.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~1,IC50~1),
	random=PGIM~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Imax=0.8, IC50=0.1),
	control = list(maxiter = 500, tolerance=1e-4))

norm.nlme1=nlme(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=pdDiag(PGIM~1),
	start=c(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude, 
	control=list(maxiter=500, pnlsTol=1e-6, tolerance=2e-5))

norm1.ranef=ranef(norm.nlme1, augFrame=TRUE, data=data_all)
plot(norm1.ranef, form=PGIM~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Age+Sex+Lung_Ptgs1)
plot(norm1.ranef, form=PGIM~Renal_medulla_Ptgs2+Age+Sex+Lung_Ptgs1)

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

norm.nlme3=nlme(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=PGIM~1|Cage,
	start=c(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude, 
	verbose=TRUE,
	control=list(maxiter=500))
norm3.ranef=ranef(norm.nlme3, augFrame=TRUE)
plot(norm3.ranef, form=PGIM~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Age+Sex+Lung_Ptgs1)
plot

base.diet.nlme1=nlme(PGIM_stimulated~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	data=data_diet,
	fixed=PGIM+Imax+IC50~1,
	random=pdDiag(PGIM~1),
	start=c(PGIM=50,Imax=40, IC50=0.5), 
	na.action=na.exclude, 
	control=list(maxiter=500))
basediet1.ranef=ranef(base.diet.nlme1, augFrame=TRUE,data=data_diet)
plot(basediet1.ranef, form=PGIM~Renal_medulla_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs2+Lung_Ptgs1)

norm.diet.nlme1=nlme(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	data=data_diet,
	fixed=PGIM+Imax+IC50~1,
	random=pdDiag(PGIM~1),
	start=c(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude, 
	control=list(maxiter=500))
normdiet1.ranef=ranef(norm.diet.nlme1, augFrame=TRUE,data=data_diet)
plot(normdiet1.ranef, form=PGIM~Renal_medulla_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs2+Lung_Ptgs1)

base.diet.nlme2=update(base.diet.nlme1,
	random=pdDiag(PGIM+Imax+IC50~1))
basediet2.ranef=ranef(base.diet.nlme2, augFrame=TRUE,data=data_diet)
plot(basediet2.ranef, form=Imax~Renal_medulla_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs2+Lung_Ptgs1)

base.diet.nlme3=update(base.diet.nlme1,
	random=pdDiag(PGIM+Imax~1))
basediet3.ranef=ranef(base.diet.nlme3, augFrame=TRUE,data=data_diet)
plot(basediet3.ranef, form=PGIM~Renal_medulla_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs2+Lung_Ptgs1)

exp.diet.nlme1=update(base.diet.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2, Imax~1,IC50~1),
	start=c(PGIM=100,Renal_medulla_Ptgs2=10000,Imax=100, IC50=1))
exp.normdiet.nlme1=update(norm.diet.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100,Imax=0.8, IC50=0.5))
exp.normdiet.nlme2=update(norm.diet.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs2, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100,Lung_Ptgs2=100,Imax=0.8, IC50=0.5))
exp.normdiet.nlme3=update(norm.diet.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100,Lung_Ptgs1=0,Imax=0.8, IC50=0.5))
exp.normdiet.nlme4=update(norm.diet.nlme1, 
	fixed=list(PGIM~Lung_Ptgs2+Lung_Ptgs1, Imax~1,IC50~1),
	start=c(PGIM=1,Lung_Ptgs2=10,Lung_Ptgs1=0,Imax=0.8, IC50=0.5))

exp.diet.nlme2=update(base.diet.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2, Imax~Renal_medulla_Ptgs2,IC50~1),
	start=c(PGIM=100,Renal_medulla_Ptgs2=10000, Imax=100,Renal_medulla_Ptgs2=10, IC50=1))

exp.diet.nlme3=update(base.diet.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2, Imax~Renal_medulla_Ptgs2,IC50~1),
	start=c(PGIM=100,Renal_medulla_Ptgs2=10000, Imax=100,Renal_medulla_Ptgs2=10, IC50=1))
exp.diet.nlme4=update(base.diet.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2, Imax~Renal_medulla_Ptgs2,IC50~1),
	random=PGIM~1|KO_Group,
	start=c(PGIM=100,Renal_medulla_Ptgs2=10000, Imax=100,Renal_medulla_Ptgs2=10, IC50=1))

exp.diet.nlme5=update(base.diet.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2, Imax~1,IC50~1),
	random=PGIM~1|KO_Group,
	start=c(PGIM=100,Renal_medulla_Ptgs2=10000, Imax=100, IC50=1))

baselog.nlme=nlme(PGIM_stimulated~(PGIM*(1-(((exp(lImax))*Celecoxib.LPS_trough)/((exp(lIC50))+Celecoxib.LPS_trough)))), 
	data=data_all, 	
	fixed=PGIM+lImax+lIC50~1,
	random=pdDiag(PGIM~1),
	start=c(PGIM=10,lImax=-.2, lIC50=-.7), 
	na.action=na.exclude,
	verbose=TRUE,
	control = list(maxiter = 500))


base1.ranef=ranef(base.nlme1, augFrame=TRUE, level=1,data=data_all)
plot(base1.ranef, form=PGIM~Renal_medulla_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs2+Lung_Ptgs1)

base.nlme.norm=nlme(PGIM_norm~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=PGIM~1,
	start=c(PGIM=1,Imax=-1, IC50=-1), 
	na.action=na.exclude, 
	control=list( maxiter=500, pnlsTol=10, minScale=1e-6))

base4.ranef=ranef(base.nlme4, augFrame=TRUE, data=data_all)

exp.nlme1=update(base.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs2, Imax~1,IC50~1),
	start=c(PGIM=50,Renal_medulla_Ptgs2=0, Lung_Ptgs2=0,Imax=0.8, IC50=0.5),
	control=list(maxiter=500))
exp.normnlme1=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=1000, Imax=0.8, IC50=0.5),
	control = list(maxiter = 500, tolerance=1e-4))
exp.normnlme2=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Imax=0.8, IC50=0.5),
	control = list(maxiter = 500, tolerance=5e-4))

exp.normnlme3=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Age, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Age=0,Imax=0.8, IC50=0.5),
	verbose=TRUE,
	control = list(maxiter = 500, tolerance=0.001))

exp.normnlme4=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Age+Sex, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Age=0, Sex=0,Imax=0.8, IC50=0.1),
	verbose=TRUE,
	control = list(maxiter = 500,tolerance=1e-4))

exp.normnlme5=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1, Imax~Renal_medulla_Ptgs2,IC50~Renal_medulla_Ptgs2),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Imax=0.8,Renal_medulla_Ptgs2=100, IC50=0.5,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=2e-5))

exp.normnlme6=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Age, Imax~Renal_medulla_Ptgs2,IC50~Renal_medulla_Ptgs2),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Age=0,Renal_medulla_Ptgs2=100,Imax=0.8, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=1e-4))

exp.normnlme7=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Age+Sex, Imax~Renal_medulla_Ptgs2,IC50~Renal_medulla_Ptgs2),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Age=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=1e-4))

exp.normnlme8=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Age+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2+Sex),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Age=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0,Sex=0),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme9=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Age+Sex, Imax~Renal_medulla_Ptgs2+Lung_Ptgs1,IC50~Renal_medulla_Ptgs2+Lung_Ptgs1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Age=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Lung_Ptgs1=0, IC50=0.1,Renal_medulla_Ptgs2=0,Lung_Ptgs1=0),
	control = list(maxiter = 500, tolerance=.1))

exp.normnlme10=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2+Sex),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0,Sex=0),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme11=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=1e-4))

exp.normnlme12=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8, IC50=0.5),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme13=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=pdDiag(PGIM+Imax~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme14=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=pdDiag(PGIM~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=1e-2))
exp.normnlme14b=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1|Cage,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=1e-2))
exp.normnlme14c=update(exp.normnlme14b, weights=varPower(), na.action=na.pass)

exp.normnlme15=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~1),
	random=PGIM~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme16=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~1),
	random=PGIM~1|Study,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme17=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1|KO_Group,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500))

exp.normnlme18=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~1),
	random=PGIM~1|KO_Group,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.5),
	control = list(maxiter = 500))

exp.normnlme19=update(norm.nlme2, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1,
	weights=~PGIM_norm,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=0.01))

exp.normnlme20=update(norm.nlme3, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1|Cage,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=5e-5))

exp.normnlme21=update(exp.normnlme20, weights=~PGIM_norm,
	control = list(maxiter = 500, tolerance=0.01))

exp.normnlme22=update(exp.normnlme14, weights=varPower(),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))
exp.normnlme22b=update(exp.normnlme15, weights=varPower(),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))

exp.normnlme23=update(exp.normnlme14, weights=varExp(),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))

exp.normnlme24=update(exp.normnlme14, weights=varFixed(~Renal_medulla_Ptgs2),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))

norm2=nls(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	fixed=PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1),
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))

exp.diet.nlme1=update(base.diet.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs2, Imax~1,IC50~1),
	start=c(PGIM=50,Renal_medulla_Ptgs2=0, Lung_Ptgs2=0,Imax=0.8, IC50=0.5),
	control=list(maxiter=500))

norm1.ranef=ranef(norm.nlme1, augFrame=TRUE, level=1,data=data_all)
plot(norm1.ranef, form=PGIM~Renal_medulla_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs2+Lung_Ptgs1)



exp.nlme3=update(base.nlme3, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs2, Imax~1,IC50~1),
	start=c(PGIM=50,Renal_medulla_Ptgs2=0, Lung_Ptgs2=0,Imax=0.8, IC50=0.5),
	control=list(maxiter=500))

base.nlme4=nlme(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=PGIM~1|Study,
	start=c(PGIM=50,Imax=0.8, IC50=0.5), 
	na.action=na.exclude, 
	control=list(maxiter=500))
base.nlme5=nlme(PGIM_stimulated~(PGIM*(1-(((exp(lImax))*Celecoxib.LPS_trough)/((exp(lIC50))+Celecoxib.LPS_trough)))), 
	data=data_all,
	fixed=PGIM+lImax+lIC50~1,
	random=PGIM~1|Mouse_ID/Study,
	start=c(PGIM=50,lImax=-.2, lIC50=-.7), 
	na.action=na.exclude, 
	control=list(maxiter=500))

base2=nls(PGIM_stimulated~(PGIM*(1-(((exp(lImax))*Celecoxib.LPS_trough)/((exp(lIC50))+Celecoxib.LPS_trough)))), 
	start=list(PGIM=50,lImax=-.2, lIC50=-.7), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))

base3=nls(PGIM_NSAID~(PGIM*(1-(((exp(lImax))*Celecoxib_trough)/((exp(lIC50))+Celecoxib_trough)))), 
	start=list(PGIM=1,lImax=-.2, lIC50=-.7), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))

ggplot(data_all, aes(PGIM_norm, PGIM_pred, color=Sex))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))
ggplot(data_all, aes(PGIM_pred,resid, color=Lung_tertile))+geom_point()+scale_x_log10()+geom_abline(slope=0, intercept=0)+pub_specs
ggplot(data_all, aes(PGIM_pred,wres, color=Lung_tertile))+geom_point()+scale_x_log10()+geom_abline(slope=0, intercept=0)+pub_specs
ggplot(data_all, aes(Celecoxib.LPS_trough,PGIM_pred, color=Lung_tertile))+geom_point()+ pub_specs+scale_x_log10()

ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough,PGIM_norm,z=Renal_medulla_Ptgs2 ))+geom_density_2d(binwidth=0.01, aes(color=..level..))+ pub_specs+scale_x_log10()
ggplot(data_LPS_cele_all, aes(Renal_medulla_Ptgs2,PGIM_norm,z=Celecoxib.LPS_trough ))+geom_density_2d(aes(color=..level..))+ pub_specs
ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough,Renal_medulla_Ptgs2,z=PGIM_norm ))+geom_density_2d( aes(color=..level..))+ pub_specs+scale_x_log10()