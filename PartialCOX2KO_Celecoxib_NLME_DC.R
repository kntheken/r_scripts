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

pdf("Response_by_exp_v2.pdf")
ggplot(subset(data_LPS_cele_all,!is.na(data_LPS_cele_all$Kidney_tertile)), aes(Kidney_tertile, PGIM_norm, color=Tx))+geom_boxplot(aes(fill=Tx, alpha=0.3))+ labs(x="Renal Medulla COX-2 Expression Tertile", y="Urinary PGIM\n(relative to control)")+pub_specs+coord_fixed()+guides(fill=guide_legend("Treatment"), color=guide_legend("none"), alpha=guide_legend("none"))+theme(legend.position="bottom")

ggplot(data_LPS_cele_all, aes(Renal_medulla_Ptgs2, PGIM_norm, color=Tx))+geom_point(size=2)+ labs(x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs + annotate("text", x = 0.012, y = 2.1, label = "Control: r=0.36, p<0.01", fontface=2)+ annotate("text", x = 0.012, y = 2, label ="Celecoxib: r=0.23, p<0.01", fontface=2)+coord_fixed(ratio=1/200)+guides(color=guide_legend("Treatment"))+theme(legend.position="bottom")

ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough, PGIM_norm, color=Kidney_tertile))+geom_point()+ geom_smooth(se=FALSE)+ labs(x="Celecoxib Plasma Concentration (micromolar)", y="Urinary PGIM\n(relative to control)")+ pub_specs+ scale_x_log10()+coord_fixed()+guides(color=guide_legend("none"))

ggplot(subset(data_LPS_cele_all,Tx=="celecoxib"), aes(Renal_medulla_Ptgs2, PGIM_norm))+geom_point()+ labs(title="Celecoxib",x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.012, y = 1.6, label = "r=0.23, p<0.01")+coord_fixed(ratio=1/200)

ggplot(subset(data_LPS_cele_all,Tx=="control"), aes(Renal_medulla_Ptgs2, PGIM_norm))+geom_point()+ labs(title="Control",x="Renal Medulla COX-2 Expression", y="Urinary PGIM\n(relative to control)")+ pub_specs+ annotate("text", x = 0.01, y = 2.1, label = "r=0.36, p<0.01")+coord_fixed(ratio=1/200)

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

library(nlme)
data_all=groupedData(data=data_LPS_cele_all[which(!is.na(data_LPS_cele_all$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Mouse_ID, outer=~Study*Sex*KO_Group)
data_diet=groupedData(data=data_LPS_celediet[which(!is.na(data_LPS_celediet$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Mouse_ID, outer=~Study*Sex*KO_Group)
data_gavage=groupedData(data=data_LPS_acutecele[which(!is.na(data_LPS_acutecele$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Mouse_ID, outer=~Study*Sex*KO_Group)


norm=nls(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	start=list(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude,
	data=data_all, 
	control = list(maxiter = 500))
	

norm.nlme1=nlme(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=PGIM~1,
	start=c(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude, 
	control=list(maxiter=500, pnlsTol=1e-6, tolerance=2e-5))

norm1.ranef=ranef(norm.nlme1, augFrame=TRUE, data=data_all)
plot(norm1.ranef, form=PGIM~Lung_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs1+Age+Sex+Renal_medulla_Ptgs2)
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

exp.normnlme1=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=1000, Imax=0.8, IC50=0.5),
	control = list(maxiter = 500, tolerance=1e-4))
	
exp.normnlme2=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Imax=0.8, IC50=0.5),
	control = list(maxiter = 500, tolerance=5e-4))

exp.normnlme3=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Imax=0.8, IC50=0.5),
	verbose=TRUE,
	control = list(maxiter = 500, tolerance=0.001))

exp.normnlme4=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Age+Sex, Imax~1,IC50~1),
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Age=0, Sex=0,Imax=0.8, IC50=0.1),
	verbose=TRUE,
	control = list(maxiter = 500,tolerance=1e-4))


exp.normnlme14=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme15=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~1),
	random=PGIM~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme16=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2,IC50~1),
	random=PGIM~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8, IC50=0.1),
	control = list(maxiter = 500, tolerance=1e-2))
	
exp.normnlme14b=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1|Study,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme15b=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~1),
	random=PGIM~1|Study,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme16b=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2,IC50~1),
	random=PGIM~1|Study,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8, IC50=0.1),
	control = list(maxiter = 500, tolerance=1e-2))

exp.normnlme19=update(exp.normnlme14, 
	weights=~PGIM_norm,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme19b=update(exp.normnlme14b, 
	weights=~PGIM_norm,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme20=update(exp.normnlme15, 
	weights=~PGIM_norm,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme21=update(exp.normnlme16, 
	weights=~PGIM_norm,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme22=update(exp.normnlme14, weights=varPower(),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme22b=update(exp.normnlme14b, weights=varPower(),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme23=update(exp.normnlme15, weights=varPower(),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))

exp.normnlme24=update(exp.normnlme16, 
	weights=varPower(),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))

exp.normnlme25=update(exp.normnlme14, weights=varFixed(~Renal_medulla_Ptgs2),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme25b=update(exp.normnlme14b, weights=varFixed(~Renal_medulla_Ptgs2),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))

exp.normnlme26=update(exp.normnlme15, weights=varFixed(~Renal_medulla_Ptgs2),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme27=update(exp.normnlme16, weights=varFixed(~Renal_medulla_Ptgs2),
	na.action=na.pass,
	control = list(maxiter = 500, tolerance=0.01))

exp.normnlme28=update(exp.normnlme14, 
	weights=~1/PGIM_norm,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme28b=update(exp.normnlme14b, 
	weights=~1/PGIM_norm,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme29=update(exp.normnlme15, 
	weights=~1/PGIM_norm,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme30=update(exp.normnlme16, 
	weights=~1/PGIM_norm,
	control = list(maxiter = 500, tolerance=0.01))

exp.normnlme31=update(exp.normnlme14, 
	weights=~1/PGIM_norm^2,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme31b=update(exp.normnlme14b, 
	weights=~1/PGIM_norm^2,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme32=update(exp.normnlme15, 
	weights=~1/PGIM_norm^2,
	control = list(maxiter = 500, tolerance=0.01))
	
exp.normnlme33=update(exp.normnlme16, 
	weights=~1/PGIM_norm^2,
	control = list(maxiter = 500, tolerance=0.01))

pdf("NLME_Diagnostic_plots.pdf")
ggplot(data_all, aes(Celecoxib.LPS_trough,PGIM_norm))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed()+labs(title="Observed Concentration-Response",y="Urinary PGIM\n(relative to control)", x="Celecoxib Plasma Concentration\n(micromolar)")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(norm)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed(5)+labs(title="Base Nonlinear Model",y="Predicted Urinary PGIM\n(relative to control)", x="Celecoxib Plasma Concentration\n(micromolar)")
ggplot(data_all, aes(predict(norm), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="Base Nonlinear Model",x="Predicted Urinary PGIM\n(relative to control)", y="Observed Urinary PGIM\n(relative to control)")
ggplot(data_all, aes(predict(norm),resid(norm)))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed(ratio=1/5)+labs(title="Base Nonlinear Model",x="Predicted Urinary PGIM\n(relative to control)", y="Residuals")
ggplot(data_all, aes(sample=resid(norm, type="pearson")))+stat_qq()+ pub_specs+coord_fixed()+labs(title="QQ-plot: Base Nonlinear Model",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(norm.nlme1)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed(1.5)+labs(title="Base NLME Model",y="Predicted Urinary PGIM\n(relative to control)", x="Celecoxib Plasma Concentration\n(micromolar)")
ggplot(data_all, aes(predict(norm.nlme1), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="Base NLME Model",x="Predicted Urinary PGIM\n(relative to control)", y="Observed Urinary PGIM\n(relative to control)")
ggplot(data_all, aes(predict(norm.nlme1),resid(norm.nlme1, type="pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed()+labs(title="Base NLME Model",x="Predicted Urinary PGIM\n(relative to control)", y="Residuals")
ggplot(data_all, aes(sample=resid(norm.nlme1, type="pearson")))+stat_qq()+ pub_specs+coord_fixed(2.5)+labs(title="QQ-plot: Base NLME Model",x="Theoretical Quantile", y="Residuals")

plot(norm1.ranef, form=PGIM~Lung_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs1+Age+Sex+Renal_medulla_Ptgs2)

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(exp.normnlme3)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed(1.5)+labs(title="NLME Model+Covariates on PGIM\n(Renal Ptgs2+Lung Ptgs1+Sex)",y="Predicted Urinary PGIM\n(relative to control)", x="Celecoxib Plasma Concentration\n(micromolar)")
ggplot(data_all, aes(predict(exp.normnlme3), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="NLME Model+Covariates on PGIM\n(Renal Ptgs2+Lung Ptgs1+Sex)",x="Predicted Urinary PGIM\n(relative to control)", y="Observed Urinary PGIM\n(relative to control)")
ggplot(data_all, aes(predict(exp.normnlme3),resid(exp.normnlme3, type="pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed()+labs(title="NLME Model+Covariates on PGIM\n(Renal Ptgs2+Lung Ptgs1+Sex)",x="Predicted Urinary PGIM\n(relative to control)", y="Residuals")
ggplot(data_all, aes(sample=resid(exp.normnlme3, type="pearson")))+stat_qq()+ pub_specs+coord_fixed(2)+labs(title="QQ-plot: NLME Model+Covariates on PGIM\n(Renal Ptgs2+Lung Ptgs1+Sex)",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(exp.normnlme14)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed(2.25)+labs(title="NLME Model+Covariates on all parameters",y="Predicted Urinary PGIM\n(relative to control)", x="Celecoxib Plasma Concentration\n(micromolar)")
ggplot(data_all, aes(predict(exp.normnlme14), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="NLME Model+Covariates on all parameters",x="Predicted Urinary PGIM\n(relative to control)", y="Observed Urinary PGIM\n(relative to control)")
ggplot(data_all, aes(predict(exp.normnlme14),resid(exp.normnlme14, type="pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed(1/4)+labs(title="NLME Model+Covariates on all parameters",x="Predicted Urinary PGIM\n(relative to control)", y="Residuals")
ggplot(data_all, aes(sample=resid(exp.normnlme14, type="pearson")))+stat_qq()+ pub_specs+coord_fixed()+labs(title="QQ-plot: NLME Model+Covariates on all parameters",x="Theoretical Quantile", y="Residuals")

ggplot(data_all, aes(Celecoxib.LPS_trough,predict(exp.normnlme22)))+geom_point()+ pub_specs+scale_x_log10()+coord_fixed(2)+labs(title="Weighted NLME Model+Covariates on all parameters",y="Predicted Urinary PGIM\n(relative to control)", x="Celecoxib Plasma Concentration\n(micromolar)")
ggplot(data_all, aes(predict(exp.normnlme22), PGIM_norm))+geom_point()+ pub_specs+geom_abline(slope=1, intercept=0)+lims(x=c(0,3),y=c(0,3))+coord_fixed()+labs(title="Weighted NLME Model+Covariates on all parameters",x="Predicted Urinary PGIM\n(relative to control)", y="Observed Urinary PGIM\n(relative to control)")
ggplot(data_all, aes(predict(exp.normnlme22),residuals(exp.normnlme22,type= "pearson")))+geom_point()+geom_abline(slope=0, intercept=0)+pub_specs+coord_fixed(1/4)+labs(title="Weighted NLME Model+Covariates on all parameters",x="Predicted Urinary PGIM\n(relative to control)", y="Residuals")
ggplot(data_all, aes(sample=resid(exp.normnlme22, type="pearson")))+stat_qq()+ pub_specs+coord_fixed()+labs(title="QQ-plot: Weighted NLME Model+Covariates on all parameters",x="Theoretical Quantile", y="Residuals")
dev.off()
qqnorm(resid(exp.normnlme22, type="pearson"))
shapiro.test(resid(exp.normnlme22, type="pearson"))
shapiro.test(resid(exp.normnlme3, type="pearson"))
shapiro.test(resid(norm, type="pearson"))

