##Requires generation of measurement dataframe##

##Replacing N/A concentrations with zero##
measurement$Ibuprofen=ifelse(is.na(measurement$Ibuprofen)==TRUE,0,measurement$Ibuprofen)
measurement$APAP=ifelse(is.na(measurement$APAP)==TRUE,0,measurement$APAP)

library(nlme)
data_all=groupedData(data=measurement, formula=COX.2.percent~Ibuprofen+APAP|Subject/Demo_Gender)

norm.nlme1=nlme(COX.2~COX.2.basal*(1-((Imaxi*Ibuprofen)/(IC50i+Ibuprofen))), 
	data=data_all,
	fixed=COX.2.basal+Imaxi+IC50i~1,
	random=COX.2.basal~1|Subject,
	start=c(COX.2.basal=24.5,Imaxi=0.8,IC50i=75), 
	na.action=na.exclude, 
	verbose=TRUE,
	control=list(maxiter=500, pnlsTol=1e-6, tolerance=2e-5))

norm.nlme2=nlme(COX.2~COX.2.basal*(1-((Imaxi*Ibuprofen)/(IC50i+Ibuprofen))-((Imaxa*APAP)/(IC50a+APAP))), 
	data=data_all,
	fixed=COX.2.basal+Imaxi+IC50i+Imaxa+IC50a~1,
	random=COX.2.basal~1|Subject,
	start=c(COX.2.basal=24.5,Imaxi=0.8,IC50i=75, Imaxa=0.5, IC50a=150), 
	na.action=na.exclude, 
	verbose=TRUE,
	control=list(maxiter=500, pnlsTol=1e-6, tolerance=2e-5))

CBF=groupedData(data=rCBF_all_response, formula=Insula~Status2+dTime|Demo_Gender/Subject)
lmesettings=lmeControl(maxIter=500,msMaxIter=500)
insula=lme(Insula~Status2+dTime,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
S1=lme(S1~Status2+dTime,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
S2=lme(S2~Status2+dTime,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
ACC=lme(ACC~Status2+dTime,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
amygdala=lme(Amygdala~Status2+dTime,random=~1|Demo_Gender/Subject,data=CBF, na.action=na.exclude,control=lmesettings)
thalamus=lme(Thalamus~Status2+dTime,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
hippocampus=lme(Hippocampus~Status2+dTime,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
parahippocampus=lme(Parahippocampus~Status2+dTime,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)

insula.1=lme(Insula~Status2*dTime,random=~1|Demo_Gender/Subject,data=CBF, na.action=na.exclude,control=lmesettings)
S1.1=lme(S1~Status2*dTime,random=~1|Demo_Gender/Subject,data=CBF, na.action=na.exclude,control=lmesettings)
S2.1=lme(S2~Status2*dTime,random=~1|Demo_Gender/Subject,data=CBF, na.action=na.exclude,control=lmesettings)
ACC.1=lme(ACC~Status2*dTime,random=~1|Demo_Gender/Subject,data=CBF, na.action=na.exclude,control=lmesettings)
amygdala.1=lme(Amygdala~Status2*dTime,random=~1|Demo_Gender/Subject,data=CBF, na.action=na.exclude,control=lmesettings)
thalamus.1=lme(Thalamus~Status2*dTime,random=~1|Demo_Gender/Subject,data=CBF, na.action=na.exclude,control=lmesettings)
hippocampus.1=lme(Hippocampus~Status2*dTime,random=~1|Demo_Gender/Subject,data=CBF, na.action=na.exclude,control=lmesettings)
parahippocampus.1=lme(Parahippocampus~Status2*dTime,random=~1|Demo_Gender/Subject,data=CBF, na.action=na.exclude,control=lmesettings)

insula.2=lme(Insula~Status2*dTime*Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
S1.2=lme(S1~Status2*dTime*Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
S2.2=lme(S2~Status2*dTime*Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
ACC.2=lme(ACC~Status2*dTime*Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
amygdala.2=lme(Amygdala~Status2*dTime*Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
thalamus.2=lme(Thalamus~Status2*dTime*Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
hippocampus.2=lme(Hippocampus~Status2*dTime*Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
parahippocampus.2=lme(Parahippocampus~Status2*dTime*Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)

insula.3=lme(Insula~Status2*dTime+Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
S1.3=lme(S1~Status2*dTime+Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
S2.3=lme(S2~Status2*dTime+Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
ACC.3=lme(ACC~Status2*dTime+Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
amygdala.3=lme(Amygdala~Status2*dTime+Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
thalamus.3=lme(Thalamus~Status2*dTime+Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
hippocampus.3=lme(Hippocampus~Status2*dTime+Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
parahippocampus.3=lme(Parahippocampus~Status2*dTime+Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)

thalamus.4=lme(Thalamus~Status2+dTime,random=~1|Subject,data=CBF, na.action=na.exclude,control=lmesettings)
hippocampus.3=lme(Hippocampus~Status2*dTime+Demo_Gender,random=~1|Subject,data=CBF, na.action=na.exclude,control=l


luminex.i=luminex.all[which(luminex.all$Status2!="Placebo"),]

luminex.basal=luminex.all[which(luminex.all$Event=="Baseline"),]
luminex.PS1=luminex.all[which(luminex.all$Event=="Post-surgery 1"),]
luminex.PS2=luminex.all[which(luminex.all$Event=="Post-surgery 2"),]

luminex.i.basal=luminex.i[which(luminex.i$Event=="Baseline"),]
luminex.i.PS1=luminex.i[which(luminex.i$Event=="Post-surgery 1"),]
luminex.i.PS2=luminex.i[which(luminex.i$Event=="Post-surgery 2"),]

PTGS2.0=lme(PTGS2~Time, random=~1|Subject,data=luminex.all, na.action=na.exclude)
PTGS2.1=lme(PTGS2~Status2*Time, random=~1|Subject,data=luminex.all, na.action=na.exclude)
IL10.0=lme(IL10~Time, random=~1|Subject,data=luminex.all, na.action=na.exclude)
IL10.1=lme(IL10~Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)

PTGS2.2=lme(PTGS2~Time*Status2*Demo_Gender, random=~1|Subject,data=luminex.all, na.action=na.exclude)
PTGS2.3=lme(PTGS2~Time*Status2, random=~1|Demo_Gender/Subject,data=luminex.all, na.action=na.exclude)
PTGS2.4=lme(PTGS2~Time+Demo_Gender+Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)

PTGS2.percent.0=lme(PTGS2.percent~Time*Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)
PTGS2.percent.3=lme(PTGS2.percent~Time*Demo_Gender+Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)

PGEM.0=lme(PGEM~Time, random=~1|Subject,data=luminex.all, na.action=na.exclude)
PGEM.1=lme(PGEM~Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)

PGEM.percent.0=lme(PGEM.percent~Time, random=~1|Subject,data=luminex.all, na.action=na.exclude)
PGEM.percent.1=lme(PGEM.percent~Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)

PGEM.2=lme(PGEM~Time*Status2*Demo_Gender, random=~1|Subject,data=luminex.all, na.action=na.exclude)
PGEM.3=lme(PGEM~Time*Demo_Gender+Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)
PGEM.4=lme(PGEM~Time+Demo_Gender+Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)
PGEM.5=lme(PGEM~Time+Demo_Gender+Status2+Ibuprofen, random=~1|Subject,data=luminex.all, na.action=na.exclude)

PGEM.percent.3=lme(PGEM.percent~Time*Demo_Gender+Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)

PGIM.0=lme(PGIM~Time+Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)
PGIM.1=lme(PGIM~Time*Status2, random=~1|Subject,data=luminex.all, na.action=na.exclude)
PGIM.2=lme(PGIM~Time*Status2*Demo_Gender, random=~1|Subject,data=luminex.all, na.action=na.exclude)

PGEM.i.0=lme(PGEM~Time+Status2, random=~1|Subject,data=luminex.i, na.action=na.exclude)
PGEM.i.1=lme(PGEM~Time+Status2+Demo_Gender, random=~1|Subject,data=luminex.i, na.action=na.exclude)
PGEM.i.2=lme(PGEM~Time+Status2+Demo_Gender+Ibuprofen, random=~1|Subject,data=luminex.i, na.action=na.exclude)

TNFa.i.2=lme(TNFa~Time+Status2+Demo_Gender+Ibuprofen+Age, random=~1|Subject,data=luminex.i, na.action=na.exclude)
IL10.i.2=lme(IL10~Time+Status2+Demo_Gender, random=~1|Subject,data=luminex.i, na.action=na.exclude)

COX.2.i.1=lme(COX.2~Time+Status2+Ibuprofen, random=~1|Subject,data=luminex.i, na.action=na.exclude)
COX.2.i.2=lme(COX.2~Time+Status2+Demo_Gender+Ibuprofen, random=~1|Subject,data=luminex.i, na.action=na.exclude)

COX.2.percent.i.1=lme(COX.2.percent~Time+Status2+Ibuprofen, random=~1|Subject,data=luminex.i, na.action=na.exclude)
COX.2.percent.i.2=lme(COX.2.percent~Time+Status2+Demo_Gender+Ibuprofen, random=~1|Subject,data=luminex.i, na.action=na.exclude)

TNFa.i.2=lme(TNFa~Time+Status2+Demo_Gender+Ibuprofen, random=~1|Subject,data=luminex.i, na.action=na.exclude)

library(dunn.test)

IL10.basal=dunn.test(all_bio$IL10.basal,all_bio$Status2)
IL6.basal=dunn.test(all_bio$IL6.basal,all_bio$Status2)
IL8.basal=dunn.test(all_bio$IL8.basal,all_bio$Status2)
MCP1.basal=dunn.test(all_bio$MCP1.basal,all_bio$Status2)
TNFa.basal=dunn.test(all_bio$TNFa.basal,all_bio$Status2)
PTGS1.basal=dunn.test(all_bio$PTGS1.basal,all_bio$Status2)
PTGS2.basal=dunn.test(all_bio$PTGS2.basal,all_bio$Status2)

IL10=dunn.test(luminex.all$IL10,luminex.all$Event)
IL6=dunn.test(luminex.all$IL6,luminex.all$Event)
IL8=dunn.test(luminex.all$IL8,luminex.all$Event)
MCP1=dunn.test(luminex.all$MCP1,luminex.all$Event)
TNFa=dunn.test(luminex.all$TNFa,luminex.all$Event)
PTGS1=dunn.test(luminex.all$PTGS1,luminex.all$Event)
PTGS2=dunn.test(luminex.all$PTGS2,luminex.all$Event)

IL10.2=dunn.test(luminex.all$IL10,luminex.all$Event)
IL6.2=dunn.test(luminex.all$IL6,luminex.all$Event)
IL8.2=dunn.test(luminex.all$IL8,luminex.all$Event)
MCP1.2=dunn.test(luminex.all$MCP1,luminex.all$Event)
TNFa.2=dunn.test(luminex.all$TNFa,luminex.all$Event)
PTGS1.2=dunn.test(luminex.all$PTGS1,luminex.all$Event)
PTGS2.2=dunn.test(luminex.all$PTGS2,luminex.all$Event)

IL10.3=dunn.test(luminex.i.PS2$IL10.percent,luminex.i.PS2$Status2)
IL6.3=dunn.test(luminex.i.PS2$IL6.percent,luminex.i.PS2$Status2)
IL8.3=dunn.test(luminex.i.PS2$IL8.percent,luminex.i.PS2$Status2)
MCP1.3=dunn.test(luminex.i.PS2$MCP1.percent,luminex.i.PS2$Status2)
TNFa.3=dunn.test(luminex.i.PS2$TNFa.percent,luminex.i.PS2$Status2)
PTGS1.3=dunn.test(luminex.i.PS2$PTGS1.percent,luminex.i.PS2$Status2)
PTGS2.3=dunn.test(luminex.i.PS2$PTGS2.percent,luminex.i.PS2$Status2)

IL10.3=dunn.test(luminex.i.PS2$IL10,luminex.i.PS2$Status2)
IL6.3=dunn.test(luminex.i.PS2$IL6,luminex.i.PS2$Status2)
IL8.3=dunn.test(luminex.i.PS2$IL8,luminex.i.PS2$Status2)
MCP1.3=dunn.test(luminex.i.PS2$MCP1,luminex.i.PS2$Status2)
TNFa.3=dunn.test(luminex.i.PS2$TNFa,luminex.i.PS2$Status2)
PTGS1.3=dunn.test(luminex.i.PS2$PTGS1,luminex.i.PS2$Status2)
PTGS2.3=dunn.test(luminex.i.PS2$PTGS2,luminex.i.PS2$Status2)

dunn.test(luminex.i.PS1$PGEM,luminex.i.PS1$Status2)
dunn.test(luminex.i.PS1$PGIM,luminex.i.PS1$Status2)
dunn.test(luminex.i.PS1$PGDM,luminex.i.PS1$Status2)
dunn.test(luminex.i.PS1$TxM,luminex.i.PS1$Status2)

dunn.test(luminex.i.PS1$PGEM.percent,luminex.i.PS1$Status2)
dunn.test(luminex.i.PS1$PGIM.percent,luminex.i.PS1$Status2)
dunn.test(luminex.i.PS1$PGDM.percent,luminex.i.PS1$Status2)
dunn.test(luminex.i.PS1$TxM.percent,luminex.i.PS1$Status2)
