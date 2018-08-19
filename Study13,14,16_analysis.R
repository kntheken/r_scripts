## Read in data##
data_all=read.csv("Study13,14,16,17B.csv")

##Making a second dataset, replacing 0 concentrations with 0.001##
data_all_nozero=data_all
data_all_nozero$Celecoxib.LPS_trough[data_all_nozero$Celecoxib.LPS_trough=="0"]=0.001

##Creating subsets by sex##
data_males=data_all[which(data_all$Sex=="M"),]
data_females=data_all[which(data_all$Sex=="F"),]
data_neg=data_all[which(data_all$Cre=="-"),]
data_neg_control=data_neg[which(data_neg$Treatment=="none"),]
data_control=data_all[which(data_neg$Treatment=="none"),]

##Testing for correlation among expression levels##
cor.test(data_all$Renal_medulla_Ptgs1,data_all$Renal_medulla_Ptgs2,use="complete.obs", method="spearman")
cor.test(data_all$Renal_medulla_Ptgs1,data_all$Lung_Ptgs1,use="complete.obs", method="spearman")
cor.test(data_all$Renal_medulla_Ptgs1, data_all$Lung_Ptgs2, use="complete.obs", method="spearman")
cor.test(data_all$Renal_medulla_Ptgs2,data_all$Lung_Ptgs1, use="complete.obs",method="spearman")
cor.test(data_all$Renal_medulla_Ptgs2,data_all$Lung_Ptgs2, use="complete.obs",method="spearman")
cor.test(data_all$Lung_Ptgs1,data_all$Lung_Ptgs2, use="complete.obs", method="spearman")

##Linear modeling of stimulated PGIM and TxB2 concentrations##
##Full model, including COX-1 and COX-2 in both lung and medulla, sex, age, and celecoxib levels##
full_lm_PGIM_all=lm(PGIM_stimulated~Lung_Ptgs1+Lung_Ptgs2+Renal_medulla_Ptgs1+Renal_medulla_Ptgs2 + Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
full_lm_Tx_all=lm(TxB2_stimulated~Lung_Ptgs1+Lung_Ptgs2+Renal_medulla_Ptgs1+Renal_medulla_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
full_lm_PGIM_FC_all=lm(PGIM_stimulated_FC~Lung_Ptgs1+Lung_Ptgs2+Renal_medulla_Ptgs1+Renal_medulla_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
full_lm_Tx_FC_all=lm(TxB2_stimulated_FC~Lung_Ptgs1+Lung_Ptgs2+Renal_medulla_Ptgs1+Renal_medulla_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough,data=data_all)

##Model including only renal COX-1 and COX-2##
renal_lm_PGIM_all=lm(PGIM_stimulated~Renal_medulla_Ptgs1+Renal_medulla_Ptgs2 + Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
renal_lm_Tx_all=lm(TxB2_stimulated~Renal_medulla_Ptgs1+Renal_medulla_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
renal_lm_PGIM_FC_all=lm(PGIM_stimulated_FC~Renal_medulla_Ptgs1+Renal_medulla_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
renal_lm_Tx_FC_all=lm(TxB2_stimulated_FC~Renal_medulla_Ptgs1+Renal_medulla_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough,data=data_all)

##Model including only lung COX-1 and COX-2##
lung_lm_PGIM_all=lm(PGIM_stimulated~Lung_Ptgs1+Lung_Ptgs2+ Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
lung_lm_Tx_all=lm(TxB2_stimulated~Lung_Ptgs1+Lung_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
lung_lm_PGIM_FC_all=lm(PGIM_stimulated_FC~Lung_Ptgs1+Lung_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
lung_lm_Tx_FC_all=lm(TxB2_stimulated_FC~Lung_Ptgs1+Lung_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough,data=data_all)

##Model including only COX-1##
COX1_lm_PGIM_all=lm(PGIM_stimulated~Lung_Ptgs1+Renal_medulla_Ptgs1 + Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
COX1_lm_Tx_all=lm(TxB2_stimulated~Lung_Ptgs1+Renal_medulla_Ptgs1+Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
COX1_lm_PGIM_FC_all=lm(PGIM_stimulated_FC~Lung_Ptgs1+Renal_medulla_Ptgs1+Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
COX1_lm_Tx_FC_all=lm(TxB2_stimulated_FC~Lung_Ptgs1+Renal_medulla_Ptgs1+Sex+Age_at_sac+Celecoxib.LPS_trough,data=data_all)

##Model including only COX-2##
COX2_lm_PGIM_all=lm(PGIM_stimulated~Lung_Ptgs2+Renal_medulla_Ptgs2 + Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
COX2_lm_Tx_all=lm(TxB2_stimulated~Lung_Ptgs2+Renal_medulla_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
COX2_lm_PGIM_FC_all=lm(PGIM_stimulated_FC~Lung_Ptgs2+Renal_medulla_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
COX2_lm_Tx_FC_all=lm(TxB2_stimulated_FC~Lung_Ptgs2+Renal_medulla_Ptgs2+Sex+Age_at_sac+Celecoxib.LPS_trough,data=data_all)

##Model including only celecoxib and demographic variables##

base_lm_PGIM_all=lm(PGIM_stimulated~Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
base_lm_Tx_all=lm(TxB2_stimulated~Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
base_lm_PGIM_FC_all=lm(PGIM_stimulated_FC~Sex+Age_at_sac+Celecoxib.LPS_trough, data=data_all)
base_lm_Tx_FC_all=lm(TxB2_stimulated_FC~Sex+Age_at_sac+Celecoxib.LPS_trough,data=data_all)

summary(full_lm_PGIM_all)
summary(full_lm_PGIM_FC_all)
summary(full_lm_Tx_all)
summary(full_lm_Tx_FC_all)
summary(base_lm_PGIM_all)
summary(base_lm_PGIM_FC_all)
summary(base_lm_Tx_all)
summary(base_lm_Tx_FC_all)
summary(COX2_lm_PGIM_all)
summary(COX2_lm_Tx_all)
summary(COX1_lm_PGIM_all)
summary(COX1_lm_Tx_all)
summary(renal_lm_PGIM_all)
summary(renal_lm_Tx_all)
summary(lung_lm_PGIM_all)
summary(lung_lm_Tx_all)
anova(full_lm_PGIM_all)



##Load Mixed Effects Modeling library##
library(lme4)

##Load nlme package##
library(nlme)

##Index plot/correlation matrix of all variables to assess independence of covariates##
pdf("Index_plots.pdf")

plot(data_all$Celecoxib.LPS_trough,data_all$PGIM_stimulated, xlab="Celecoxib Plasma Concentration (uM)", ylab="Urinary PGIM (ng/mg creatinine)")
plot(log10(data_all_nozero$Celecoxib.LPS_trough),data_all_nozero$PGIM_stimulated, xlab="log(Celecoxib Plasma Concentration (uM))", ylab="Urinary PGIM (ng/mg creatinine)")
plot(data_all$Celecoxib.LPS_trough,data_all$TxB2_stimulated, xlab="Celecoxib Plasma Concentration (uM)", ylab="Urinary TxB2 (ng/mg creatinine)")

par(mfrow=c(1,1))
pairs(~Mouse_ID+PGIM_stimulated+TxB2_stimulated+Sex+Age_at_sac, data=data_all, 
         panel=function(x,y) { points(x,y)} )
par(mfrow=c(1,1))
pairs(~PGIM_stimulated+TxB2_stimulated+Lung_Ptgs1+Lung_Ptgs2+Renal_medulla_Ptgs1+Renal_medulla_Ptgs2, data=data_all, 
         panel=function(x,y) { points(x,y)} )

dev.off()

##Base Emax model##
base_nls_PGIM=nls(PGIM_stimulated~PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))), start=list(PGIM=44.2,Imax=80, IC50=0.5), data=data_all)
summary(base_nls_PGIM)

##Diagnostic plots, observed vs. predicted, residuals, etc##
pdf("Base_Diagnostics.pdf")
par(mfrow=c(2,2))
plot(predict(base_nls_PGIM),data_all$PGIM_stimulated, main="Base Model Obs vs Pred", xlab="Predicted PGIM", ylab="Observed PGIM", ylim=c(0,140), xlim=c(0,140))
abline(a=0,b=1)
pred=predict(base_nls_PGIM)
res=data_all$PGIM_stimulated-pred
plot(pred, res, main="Base Model Residuals vs Predicted", xlab="Predicted", ylab="Residuals")
abline(a=0,b=0)

plot(data_all$Celecoxib.LPS_trough,predict(base_nls_PGIM))
dev.off()

##Addition of covariates: Ptgs1, Ptgs2, Age, Sex##
add_nls_PGIM_lung_full=nls(PGIM_stimulated~PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))+beta1*Lung_Ptgs2+beta2*Lung_Ptgs1+beta4*Age_at_sac), start=list(PGIM=44.2,Imax=0.8, IC50=0.5, beta1=0.5, beta2=0.5, beta4=0.5), data=data_all)
add_nls_PGIM_renal_full=nls(PGIM_stimulated~PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))+beta1*Renal_medulla_Ptgs2+beta2*Renal_medulla_Ptgs1+beta4*Age_at_sac), start=list(PGIM=44.2,Imax=0.8, IC50=0.5, beta1=0.5, beta2=0.5, beta4=0.5), data=data_all)
summary(add_nls_PGIM_renal_full)
summary(add_nls_PGIM_lung_full)

add_nls_PGIM_lung_COX2=nls(PGIM_stimulated~PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))+beta1*Lung_Ptgs2), start=list(PGIM=44.2,Imax=0.8, IC50=0.5, beta1=0.5), data=data_all)
add_nls_PGIM_renal_COX2=nls(PGIM_stimulated~PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))+beta1*Renal_medulla_Ptgs2), start=list(PGIM=44.2,Imax=0.8, IC50=0.5, beta1=0.5), data=data_all)
summary(add_nls_PGIM_renal_COX2)
summary(add_nls_PGIM_lung_COX2)

add_nls_PGIM_lung_COX=nls(PGIM_stimulated~PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))+beta1*Lung_Ptgs2+beta2*Lung_Ptgs1), start=list(PGIM=44.2,Imax=0.8, IC50=0.5, beta1=0.5, beta2=0.5), data=data_all)
add_nls_PGIM_renal_COX=nls(PGIM_stimulated~PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough))+beta1*Renal_medulla_Ptgs2+beta2*Renal_medulla_Ptgs1), start=list(PGIM=44.2,Imax=0.8, IC50=0.5, beta1=0.5, beta2=0.5), data=data_all)
summary(add_nls_PGIM_renal_COX)
summary(add_nls_PGIM_lung_COX)

AIC(base_nls_PGIM)
AIC(add_nls_PGIM_renal_full)
AIC(add_nls_PGIM_renal_COX2)
AIC(add_nls_PGIM_renal_COX)
AIC(add_nls_PGIM_lung_full)
AIC(add_nls_PGIM_lung_COX2)
AIC(add_nls_PGIM_lung_COX)

##Diagnostic plots, observed vs. predicted, residuals, etc##
pdf("Additive_Diagnostics.pdf")
par(mfrow=c(2,2))
plot(predict(add_nls_PGIM_renal_COX),data_all$PGIM_stimulated, main="Renal COX Model Obs vs Pred", xlab="Predicted PGIM", ylab="Observed PGIM", ylim=c(0,140), xlim=c(0,140))
abline(a=0,b=1)
pred=predict(add_nls_PGIM_renal_COX)
res=data_all$PGIM_stimulated-pred
plot(pred, res, main="Renal COX Residuals vs Predicted", xlab="Predicted", ylab="Residuals")
abline(a=0,b=0)

par(mfrow=c(2,2))
plot(predict(add_nls_PGIM_renal_COX2),data_all$PGIM_stimulated, main="Renal COX2 Model Obs vs Pred", xlab="Predicted PGIM", ylab="Observed PGIM", ylim=c(0,140), xlim=c(0,140))
abline(a=0,b=1)
pred=predict(add_nls_PGIM_renal_COX2)
res=data_all$PGIM_stimulated-pred
plot(pred, res, main="Renal COX2 Residuals vs Predicted", xlab="Predicted", ylab="Residuals")
abline(a=0,b=0)

par(mfrow=c(2,2))
plot(predict(add_nls_PGIM_lung_COX),data_all$PGIM_stimulated, main="Lung COX Model Obs vs Pred", xlab="Predicted PGIM", ylab="Observed PGIM", ylim=c(0,140), xlim=c(0,140))
abline(a=0,b=1)
pred=predict(add_nls_PGIM_lung_COX)
res=data_all$PGIM_stimulated-pred
plot(pred, res, main="Lung COX Residuals vs Predicted", xlab="Predicted", ylab="Residuals")
abline(a=0,b=0)

par(mfrow=c(2,2))
plot(predict(add_nls_PGIM_lung_COX2),data_all$PGIM_stimulated, main="Lung COX2 Model Obs vs Pred", xlab="Predicted PGIM", ylab="Observed PGIM", ylim=c(0,140), xlim=c(0,140))
abline(a=0,b=1)
pred=predict(add_nls_PGIM_lung_COX2)
res=data_all$PGIM_stimulated-pred
plot(pred, res, main="Lung COX2 Residuals vs Predicted", xlab="Predicted", ylab="Residuals")
abline(a=0,b=0)

dev.off()

##Power model for COX ##
power_nls_PGIM_renal_COX2=nls(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))*Renal_medulla_Ptgs2^beta1), start=list(PGIM=44.2,Imax=0.8, IC50=0.5, beta1=0.05), data=data_all)
power_nls_PGIM_renal_COX=nls(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))*Renal_medulla_Ptgs2^beta1*Renal_medulla_Ptgs1^beta2), start=list(PGIM=44.2,Imax=0.8, IC50=0.5, beta1=0.05, beta2=0.05), data=data_all)
power_nls_PGIM_lung_COX2=nls(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))*Lung_Ptgs2^beta1), start=list(PGIM=44.2,Imax=0.8, IC50=0.5, beta1=0.05), data=data_all)
power_nls_PGIM_lung_COX=nls(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))*Lung_Ptgs2^beta1*Lung_Ptgs1^beta2), start=list(PGIM=44.2,Imax=0.8, IC50=0.5, beta1=0.05,beta2=0.05), data=data_all)

summary(power_nls_PGIM_renal_COX2)
summary(power_nls_PGIM_renal_COX)
summary(power_nls_PGIM_lung_COX2)
summary(power_nls_PGIM_lung_COX)

confint(power_nls_PGIM_renal_COX2)
confint(power_nls_PGIM_renal_COX)
confint(power_nls_PGIM_lung_COX2)
confint(power_nls_PGIM_lung_COX)

AIC(power_nls_PGIM_renal_COX2)
AIC(power_nls_PGIM_renal_COX)
AIC(power_nls_PGIM_lung_COX2)
AIC(power_nls_PGIM_lung_COX)

##Diagnostic plots, observed vs. predicted, residuals, etc##
pdf("Power_Diagnostics.pdf")
par(mfrow=c(2,2))
plot(predict(power_nls_PGIM_renal_COX),data_all$PGIM_stimulated, main="Renal COX Model Obs vs Pred", xlab="Predicted PGIM", ylab="Observed PGIM", ylim=c(0,140), xlim=c(0,140))
abline(a=0,b=1)
pred=predict(power_nls_PGIM_renal_COX)
res=data_all$PGIM_stimulated-pred
plot(pred, res, main="Renal COX Residuals vs Predicted", xlab="Predicted", ylab="Residuals")
abline(a=0,b=0)

par(mfrow=c(2,2))
plot(predict(power_nls_PGIM_renal_COX2),data_all$PGIM_stimulated, main="Renal COX2 Model Obs vs Pred", xlab="Predicted PGIM", ylab="Observed PGIM", ylim=c(0,140), xlim=c(0,140))
abline(a=0,b=1)
pred=predict(power_nls_PGIM_renal_COX2)
res=data_all$PGIM_stimulated-pred
plot(pred, res, main="Renal COX2 Residuals vs Predicted", xlab="Predicted", ylab="Residuals")
abline(a=0,b=0)

par(mfrow=c(2,2))
plot(predict(power_nls_PGIM_lung_COX),data_all$PGIM_stimulated, main="Lung COX Model Obs vs Pred", xlab="Predicted PGIM", ylab="Observed PGIM", ylim=c(0,140), xlim=c(0,140))
abline(a=0,b=1)
pred=predict(power_nls_PGIM_lung_COX)
res=data_all$PGIM_stimulated-pred
plot(pred, res, main="Lung COX Residuals vs Predicted", xlab="Predicted", ylab="Residuals")
abline(a=0,b=0)

par(mfrow=c(2,2))
plot(predict(power_nls_PGIM_lung_COX2),data_all$PGIM_stimulated, main="Lung COX2 Model Obs vs Pred", xlab="Predicted PGIM", ylab="Observed PGIM", ylim=c(0,140), xlim=c(0,140))
abline(a=0,b=1)
pred=predict(power_nls_PGIM_lung_COX2)
res=data_all$PGIM_stimulated-pred
plot(pred, res, main="Lung COX2 Residuals vs Predicted", xlab="Predicted", ylab="Residuals")
abline(a=0,b=0)

dev.off()


##Addition of random effects##

##Diagnostics##

