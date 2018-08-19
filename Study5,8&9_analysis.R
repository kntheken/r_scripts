## Read in data and make subsets by sex and celecoxib dose##
data=read.csv("IndCOX2KO_study5,8&9_dataset.csv")
data_males= data[which(data$Sex=="M"),]
data_females= data[which(data$Sex=="F"),]
control=subset(data, Coxib.dose=="none")
control_males=subset(data_males, Coxib.dose=="none")
low_males=subset(data_males, Coxib.dose=="low")
high_males=subset(data_males, Coxib.dose=="high")
control_females=subset(data_females, Coxib.dose=="none")
low_females=subset(data_females, Coxib.dose=="low")
high_females=subset(data_females, Coxib.dose=="high")
neg_males=subset(data_males, Cre=="-")
neg_females=subset(data_females, Cre=="-")
neg=subset(data, Cre=="-")

##Regression##
fitallPGIM <- lm(PGIM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough , data=data)
summary(fitallPGIM)

fitallPGIM2 <- lm(PGIM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough + Medulla.Ptgs2*Celecoxib_trough, data=data)
summary(fitallPGIM2) 

fitmalesPGIM <- lm(PGIM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough , data=data_males)
summary(fitmalesPGIM) 

fitmalesPGIM2 <- lm(PGIM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough + Medulla.Ptgs2*Celecoxib_trough, data=data_males)
summary(fitmalesPGIM2)

fitfemalesPGIM <- lm(PGIM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough , data=data_females)
summary(fitfemalesPGIM)

fitfemalesPGIM2 <- lm(PGIM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough + Medulla.Ptgs2*Celecoxib_trough, data=data_females)
summary(fitfemalesPGIM2) 

fitallPGEM <- lm(PGEM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough , data=data)
summary(fitallPGEM) 

fitallPGEM2 <- lm(PGEM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough + Medulla.Ptgs2*Celecoxib_trough, data=data)
summary(fitallPGEM2) 

fitmalesPGEM <- lm(PGEM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough , data=data_males)
summary(fitmalesPGEM) 

fitmalesPGEM2 <- lm(PGEM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough + Medulla.Ptgs2*Celecoxib_trough, data=data_males)
summary(fitmalesPGEM2) 

fitfemalesPGEM <- lm(PGEM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough , data=data_females)
summary(fitfemalesPGEM) 

fitfemalesPGEM2 <- lm(PGEM_coxib.LPS ~ Medulla.Ptgs2 + Celecoxib_trough + Medulla.Ptgs2*Celecoxib_trough, data=data_females)
summary(fitfemalesPGEM2)

##Correlation## 
cor.test(data$PGIM_coxib.LPS,data$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(data$PGEM_coxib.LPS,data$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(data$PGIM_coxib.LPS,data$Medulla.Ptgs2, use="complete.obs", method="spearman")
cor.test(data$PGEM_coxib.LPS,data$Medulla.Ptgs2, use="complete.obs", method="spearman")

cor.test(data_males$PGIM_coxib.LPS,data_males$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(data_males$PGEM_coxib.LPS,data_males$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(data_males$PGIM_coxib.LPS,data_males$Medulla.Ptgs2, use="complete.obs", method="spearman")
cor.test(data_males$PGEM_coxib.LPS,data_males$Medulla.Ptgs2, use="complete.obs", method="spearman")

cor.test(data_females$PGIM_coxib.LPS,data_females$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(data_females$PGEM_coxib.LPS,data_females$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(data_females$PGIM_coxib.LPS,data_females$Medulla.Ptgs2, use="complete.obs", method="spearman")
cor.test(data_females$PGEM_coxib.LPS,data_females$Medulla.Ptgs2, use="complete.obs", method="spearman")

cor.test(control$PGIM_coxib.LPS,control$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(control$PGEM_coxib.LPS,control$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(control$PGIM_coxib.LPS,control$Medulla.Ptgs2, use="complete.obs", method="spearman")
cor.test(control$PGEM_coxib.LPS,control$Medulla.Ptgs2, use="complete.obs", method="spearman")
cor.test(control$PGIM_coxib.LPS,control$Lung.Ptgs2, use="complete.obs", method="spearman")
cor.test(control$PGEM_coxib.LPS,control$Lung.Ptgs2, use="complete.obs", method="spearman")
cor.test(control$PGIM_coxib.LPS,control$Medulla.Ptgs1, use="complete.obs", method="spearman")
cor.test(control$PGEM_coxib.LPS,control$Medulla.Ptgs1, use="complete.obs", method="spearman")
cor.test(control$PGIM_coxib.LPS,control$Lung.Ptgs1, use="complete.obs", method="spearman")
cor.test(control$PGEM_coxib.LPS,control$Lung.Ptgs1, use="complete.obs", method="spearman")
cor.test(control$TxB2_coxib.LPS,control$Lung.Ptgs1, use="complete.obs", method="spearman")

cor.test(control_males$PGIM_coxib.LPS,control_males$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(control_males$PGEM_coxib.LPS,control_males$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(control_males$PGIM_coxib.LPS,control_males$Medulla.Ptgs2, use="complete.obs", method="spearman")
cor.test(control_males$PGEM_coxib.LPS,control_males$Medulla.Ptgs2, use="complete.obs", method="spearman")
cor.test(control_males$PGIM_coxib.LPS,control_males$Lung.Ptgs2, use="complete.obs", method="spearman")
cor.test(control_males$PGEM_coxib.LPS,control_males$Lung.Ptgs2, use="complete.obs", method="spearman")
cor.test(control_males$PGIM_coxib.LPS,control_males$Medulla.Ptgs1, use="complete.obs", method="spearman")
cor.test(control_males$PGEM_coxib.LPS,control_males$Medulla.Ptgs1, use="complete.obs", method="spearman")
cor.test(control_males$PGIM_coxib.LPS,control_males$Lung.Ptgs1, use="complete.obs", method="spearman")
cor.test(control_males$PGEM_coxib.LPS,control_males$Lung.Ptgs1, use="complete.obs", method="spearman")
cor.test(control_males$TxB2_coxib.LPS,control_males$Lung.Ptgs1, use="complete.obs", method="spearman")

cor.test(control_females$PGIM_coxib.LPS,control_females$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(control_females$PGEM_coxib.LPS,control_females$Celecoxib_trough, use="complete.obs", method="spearman")
cor.test(control_females$PGIM_coxib.LPS,control_females$Medulla.Ptgs2, use="complete.obs", method="spearman")
cor.test(control_females$PGEM_coxib.LPS,control_females$Medulla.Ptgs2, use="complete.obs", method="spearman")
cor.test(control_females$PGIM_coxib.LPS,control_females$Lung.Ptgs2, use="complete.obs", method="spearman")
cor.test(control_females$PGEM_coxib.LPS,control_females$Lung.Ptgs2, use="complete.obs", method="spearman")
cor.test(control_females$PGIM_coxib.LPS,control_females$Medulla.Ptgs1, use="complete.obs", method="spearman")
cor.test(control_females$PGEM_coxib.LPS,control_females$Medulla.Ptgs1, use="complete.obs", method="spearman")
cor.test(control_females$PGIM_coxib.LPS,control_females$Lung.Ptgs1, use="complete.obs", method="spearman")
cor.test(control_females$PGEM_coxib.LPS,control_females$Lung.Ptgs1, use="complete.obs", method="spearman")
cor.test(control_females$TxB2_coxib.LPS,control_females$Lung.Ptgs1, use="complete.obs", method="spearman")

shapiro.test(data$PGIM_coxib.LPS)
shapiro.test(log(data$PGIM_coxib.LPS))
qqnorm(data$PGIM_coxib.LPS);qqline(data$PGIM_coxib.LPS, col=2)
qqnorm(log(data$PGIM_coxib.LPS));qqline(log(data$PGIM_coxib.LPS), col=2)

