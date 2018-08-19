library(nlme)
data_all=groupedData(data=data_LPS_cele_all[which(!is.na(data_LPS_cele_all$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Cage)
data_diet=groupedData(data=data_LPS_celediet[which(!is.na(data_LPS_celediet$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Cage)
data_gavage=groupedData(data=data_LPS_acutecele[which(!is.na(data_LPS_acutecele$Study )),], formula=PGIM_norm~Celecoxib.LPS_trough|Cage)

##Model using normalized PGIM, standard Emax equation, random effects by cage##
norm.nlme1=nlme(PGIM_norm~(PGIM-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=PGIM~1,
	start=c(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude, 
	control=list(maxiter=500, pnlsTol=1e-6, tolerance=2e-5))

exp.normnlme1=update(norm.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=1e-5))

##Model using raw PGIM, fractional Emax equation, random effects by cage within study##
PGIM.nlme1=nlme(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=PGIM~1|Study/Cage,
	start=c(PGIM=1,Imax=0.8, IC50=0.5), 
	na.action=na.exclude, 
	control=list(maxiter=500, pnlsTol=1e-6, tolerance=2e-5))

PGIM.nlme2=nlme(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))), 
	data=data_all,
	fixed=PGIM+Imax+IC50~1,
	random=PGIM~1|Study/Cage/Mouse_ID,
	start=c(PGIM=1,Imax=0.8, IC50=0.5), 
	verbose=TRUE,
	na.action=na.exclude, 
	control=list(maxiter=500, pnlsTol=1e-6, tolerance=2e-5))

exp.PGIMnlme1=update(PGIM.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Sex,IC50~Renal_medulla_Ptgs2),
	random=PGIM~1|Study/Cage,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=0),
	control = list(maxiter = 500, tolerance=1e-5))

exp.PGIMnlme2=update(PGIM.nlme1, 
	fixed=list(PGIM~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs1+Sex, Imax~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs1+Sex,IC50~Renal_medulla_Ptgs2+Lung_Ptgs2+Renal_medulla_Ptgs1+Lung_Ptgs1+Sex),
	random=PGIM~1|Study/Cage,
	start=c(PGIM=1,Renal_medulla_Ptgs2=100,Renal_medulla_Ptgs1=0,Lung_Ptgs2=100, Lung_Ptgs1=0,Sex=0,Renal_medulla_Ptgs2=100,Imax=0.8,Renal_medulla_Ptgs1=0,Lung_Ptgs2=100, Lung_Ptgs1=0,Sex=0, IC50=0.1,Renal_medulla_Ptgs2=100,Renal_medulla_Ptgs1=0,Lung_Ptgs2=100, Lung_Ptgs1=0,Sex=0))

