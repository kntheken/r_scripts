##Read in data##
data_LPS_celediet=read.csv("COX2_PartialKO_LPS_LPScelediet.csv")
data_LPS_acutecele=read.csv("COX2_PartialKO_LPS_acutecele.csv")

data_LPS_celediet$Kidney_tertile= with(data_LPS_celediet, cut (Renal_medulla_Ptgs2, breaks=quantile(Renal_medulla_Ptgs2, probs=seq(0,1,by=0.33), na.rm=TRUE),include.lowest=TRUE))
data_LPS_celediet$Lung_tertile= with(data_LPS_celediet, cut (Lung_Ptgs2, breaks=quantile(Lung_Ptgs2, probs=seq(0,1,by=0.33), na.rm=TRUE),include.lowest=TRUE))


data_LPS_cele_all=merge(data_LPS_celediet, data_LPS_acutecele, all=TRUE)
data_LPS_cele_all$Kidney_tertile= with(data_LPS_cele_all, cut (Renal_medulla_Ptgs2, breaks=quantile(Renal_medulla_Ptgs2, probs=seq(0,1,by=0.33), na.rm=TRUE),include.lowest=TRUE))
data_LPS_cele_all$Lung_tertile= with(data_LPS_cele_all, cut (Lung_Ptgs2, breaks=quantile(Lung_Ptgs2, probs=seq(0,1,by=0.33), na.rm=TRUE),include.lowest=TRUE))
data_LPS_cele_all$Kidney_quartile= with(data_LPS_cele_all, cut (Renal_medulla_Ptgs2, breaks=quantile(Renal_medulla_Ptgs2, probs=seq(0,1,by=0.25), na.rm=TRUE),include.lowest=TRUE))
data_LPS_cele_all$Lung_quartile= with(data_LPS_cele_all, cut (Lung_Ptgs2, breaks=quantile(Lung_Ptgs2, probs=seq(0,1,by=0.25), na.rm=TRUE),include.lowest=TRUE))
data_LPS_cele_all$Celecoxib.LPS_trough[is.na(data_LPS_cele_all$Celecoxib.LPS_trough)]=0
data_LPS_cele_all$PGIM_control_mean=ifelse(data_LPS_cele_all$Study==5|data_LPS_cele_all$Study==8 | data_LPS_cele_all$Study==9, as.numeric(mean(subset(data_LPS_cele_all$PGIM_stimulated, data_LPS_cele_all$Treatment=="vehicle"))), as.numeric(mean(subset(data_LPS_cele_all$PGIM_stimulated, data_LPS_cele_all$Treatment=="none"))))
data_LPS_cele_all$PGIM_norm=data_LPS_cele_all$PGIM_stimulated/data_LPS_cele_all$PGIM_control_mean
data_LPS_cele_all$NSAID_route=as.factor(ifelse(data_LPS_cele_all$Study==5|data_LPS_cele_all$Study==8 | data_LPS_cele_all$Study==9, "gavage", "diet"))
data_LPS_cele_all$Age=as.numeric(as.Date(data_LPS_cele_all$Sac_date)-as.Date(data_LPS_cele_all$DOB))
data_LPS_cele_all$TM_time=as.numeric(as.Date(data_LPS_cele_all$Sac_date)-as.Date(data_LPS_cele_all$TM_date))
data_LPS_cele_male=data_LPS_cele_all[which(data_LPS_cele_all$Sex=="M"),]
data_LPS_cele_female=data_LPS_cele_all[which(data_LPS_cele_all$Sex=="F"),]
data_all_nozero=data_LPS_cele_all
data_all_nozero$Celecoxib.LPS_trough[data_all_nozero$Celecoxib.LPS_trough=="0"]=0.001
data_LPS_cele_control=data_LPS_cele_all[which(data_LPS_cele_all$Celecoxib.LPS_trough=="0"),]
data_LPS_cele=data_LPS_cele_all[which(data_LPS_cele_all$Celecoxib.LPS_trough!="0"),]
data_LPS_cele_kidneylow=data_LPS_celediet[which(as.numeric(data_LPS_celediet$Kidney_tertile)=="1"),]
data_LPS_cele_kidneymid=data_LPS_celediet[which(as.numeric(data_LPS_celediet$Kidney_tertile)=="2"),]
data_LPS_cele_kidneyhigh=data_LPS_celediet[which(as.numeric(data_LPS_celediet$Kidney_tertile)=="3"),]
data_LPS_cele_lunglow=data_LPS_celediet[which(as.numeric(data_LPS_celediet$Lung_tertile)=="1"),]
data_LPS_cele_lungmid=data_LPS_celediet[which(as.numeric(data_LPS_celediet$Lung_tertile)=="2"),]
data_LPS_cele_lunghigh=data_LPS_celediet[which(as.numeric(data_LPS_celediet$Lung_tertile)=="3"),]
 t.test(data_LPS_cele_lunghigh$Celecoxib.LPS_trough~data_LPS_cele_lunghigh$Treatment)
  t.test(data_LPS_cele_lungmid$PGIM_stimulated~data_LPS_cele_lungmid$Treatment)
   t.test(data_LPS_cele_lunglow$Celecoxib.LPS_trough~data_LPS_cele_lunglow$Treatment)
    t.test(data_LPS_cele_kidneyhigh$Celecoxib.LPS_trough~data_LPS_cele_kidneyhigh$Treatment)
     t.test(data_LPS_cele_kidneymid$PGIM_stimulated~data_LPS_cele_kidneymid$Treatment)
      t.test(data_LPS_cele_kidneylow$Celecoxib.LPS_trough~data_LPS_cele_kidneylow$Treatment)

library(ggplot2)
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"))
ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough, PGIM_norm))+geom_point()+scale_x_log10()+ pub_specs
ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough, PGIM_norm, color=Lung_tertile))+geom_point()+scale_x_log10()+ pub_specs
inhibEmax=PGIM*(1-((Imax*C)/(IC50+C)))

##Power model for COX ##
library(nlme)
base=nls(PGIM_norm~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))), start=list(PGIM=1,Imax=0.8, IC50=0.5), na.action=na.exclude,data=data_LPS_cele_all)
power_nls_PGIM_renal_COX2=nls(PGIM_norm~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))*Renal_medulla_Ptgs2^beta1), start=list(PGIM=1,Imax=0.8, IC50=0.5, beta1=0.05), na.action=na.exclude,data=data_LPS_cele_all)
power_nls_PGIM_renal_COX=nls(PGIM_norm~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))*Renal_medulla_Ptgs2^beta1*Renal_medulla_Ptgs1^beta2), start=list(PGIM=1,Imax=0.8, IC50=0.5, beta1=0.05, beta2=0.05), na.action=na.exclude,data=data_LPS_cele_all)
power_nls_PGIM_lung_COX2=nls(PGIM_norm~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))*Lung_Ptgs2^beta1), start=list(PGIM=1,Imax=0.8, IC50=0.5, beta1=0.05), na.action=na.exclude,data=data_LPS_cele_all)
power_nls_PGIM_lung_COX=nls(PGIM_norm~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))*Lung_Ptgs2^beta1*Lung_Ptgs1^beta2), start=list(PGIM=1,Imax=0.8, IC50=0.5, beta1=0.05,beta2=0.05), na.action=na.exclude,data=data_LPS_cele_all)

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

ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough, PGIM_norm))+geom_point()+scale_x_log10()+ pub_specs +geom_line(aes(y=predict(base)))
ggplot(data_LPS_cele_all, aes(Celecoxib.LPS_trough, PGIM_norm, color=Kidney_tertile))+geom_point()+scale_x_log10()+ pub_specs +geom_line(aes(y=predict(base)))

base_diet=nls(PGIM_stimulated~(PGIM*(1-((Imax*Celecoxib.LPS_trough)/(IC50+Celecoxib.LPS_trough)))), start=list(PGIM=1,Imax=0.5, IC50=0.2), na.action=na.exclude,data=data_LPS_celediet)
ggplot(data_LPS_celediet, aes(Celecoxib.LPS_trough, PGIM_stimulated, color=Kidney_tertile))+geom_point()+scale_x_log10()+ pub_specs +geom_line(aes(y=predict(base_diet)))

##Diagnostic plots, observed vs. predicted, residuals, etc##
pdf("Power_Diagnostics.pdf")
par(mfrow=c(2,2))
plot(predict(power_nls_PGIM_renal_COX),data_LPS_cele_all$PGIM_norm, main="Renal COX Model Obs vs Pred", xlab="Predicted PGIM", ylab="Observed PGIM", ylim=c(0,5), xlim=c(0,5))
abline(a=0,b=1)
pred=predict(power_nls_PGIM_renal_COX)
res=data_LPS_cele_all$PGIM_norm-pred
plot(pred, res, main="Renal COX Residuals vs Predicted", xlab="Predicted", ylab="Residuals", ylim=c(-2,2))
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