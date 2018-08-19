data=read.csv("Study5_data.csv")
data_males= data[which(data$Sex=="M"),]
data_females= data[which(data$Sex=="F"),]
control_males=subset(data_males, Coxib.dose=="none")
low_males=subset(data_males, Coxib.dose=="low")
high_males=subset(data_males, Coxib.dose=="high")

plot(PGEM_coxib.LPS~Medulla.Ptgs2, data=data_males, type="n")
points(control_males$Medulla.Ptgs2, control_males$PGEM_coxib.LPS, pch=20)
points(low_males$Medulla.Ptgs2, low_males$PGEM_coxib.LPS, pch=10)
points(high_males$Medulla.Ptgs2, high_males$PGEM_coxib.LPS, pch=1)

plot(PGIM_coxib.LPS~Medulla.Ptgs2, data=data_males, type="n")
points(control_males$Medulla.Ptgs2, control_males$PGIM_coxib.LPS, pch=20)
points(low_males$Medulla.Ptgs2, low_males$PGIM_coxib.LPS, pch=10)
points(high_males$Medulla.Ptgs2, high_males$PGIM_coxib.LPS, pch=1)

plot(PGEM_coxib.LPS~Lung.Ptgs2, data=data_males, type="n")
points(control_males$Lung.Ptgs2, control_males$PGEM_coxib.LPS, pch=20)
points(low_males$Lung.Ptgs2, low_males$PGEM_coxib.LPS, pch=10)
points(high_males$Lung.Ptgs2, high_males$PGEM_coxib.LPS, pch=1)

plot(PGIM_coxib.LPS~Lung.Ptgs2, data=data_males, type="n")
points(control_males$Lung.Ptgs2, control_males$PGIM_coxib.LPS, pch=20)
points(low_males$Lung.Ptgs2, low_males$PGIM_coxib.LPS, pch=10)
points(high_males$Lung.Ptgs2, high_males$PGIM_coxib.LPS, pch=1)

control_females=subset(data_females, Coxib.dose=="none")
low_females=subset(data_females, Coxib.dose=="low")
high_females=subset(data_females, Coxib.dose=="high")

plot(PGEM_coxib.LPS~Medulla.Ptgs2, data=data_females, type="n")
points(control_females$Medulla.Ptgs2, control_females$PGEM_coxib.LPS, pch=20)
points(low_females$Medulla.Ptgs2, low_females$PGEM_coxib.LPS, pch=10)
points(high_females$Medulla.Ptgs2, high_females$PGEM_coxib.LPS, pch=1)

plot(PGIM_coxib.LPS~Medulla.Ptgs2, data=data_females, type="n")
points(control_females$Medulla.Ptgs2, control_females$PGIM_coxib.LPS, pch=20)
points(low_females$Medulla.Ptgs2, low_females$PGIM_coxib.LPS, pch=10)
points(high_females$Medulla.Ptgs2, high_females$PGIM_coxib.LPS, pch=1)

plot(PGEM_coxib.LPS~Lung.Ptgs2, data=data_females, type="n")
points(control_females$Lung.Ptgs2, control_females$PGEM_coxib.LPS, pch=20)
points(low_females$Lung.Ptgs2, low_females$PGEM_coxib.LPS, pch=10)
points(high_females$Lung.Ptgs2, high_females$PGEM_coxib.LPS, pch=1)

plot(PGIM_coxib.LPS~Lung.Ptgs2, data=data_females, type="n")
points(control_females$Lung.Ptgs2, control_females$PGIM_coxib.LPS, pch=20)
points(low_females$Lung.Ptgs2, low_females$PGIM_coxib.LPS, pch=10)
points(high_females$Lung.Ptgs2, high_females$PGIM_coxib.LPS, pch=1)

tertile=cut(data_males$Medulla.Ptgs2,breaks=3, labels=c("low","medium","high"))

neg_males=subset(data_males, Cre=="-")
malePGIM=with(neg_males, bargraph.CI(x.factor=Coxib.dose, response=PGIM_coxib.LPS))
malePGEM=with(neg_males, bargraph.CI(x.factor=Coxib.dose, response=PGEM_coxib.LPS))

neg_females=subset(data_females, Cre=="-")
femalePGEM=with(neg_females, bargraph.CI(x.factor=Coxib.dose, response=PGEM_coxib.LPS))
femalePGIM=with(neg_females, bargraph.CI(x.factor=Coxib.dose, response=PGIM_coxib.LPS))

with(neg_males,dotplot(PGIM_coxib.LPS~Coxib.dose))
with(neg_males,dotplot(PGEM_coxib.LPS~Coxib.dose))

with(neg_females,dotplot(PGIM_coxib.LPS~Coxib.dose))
with(neg_females,dotplot(PGEM_coxib.LPS~Coxib.dose))

with(neg_males,dotplot(PGEM_coxib~Coxib.dose))
with(neg_males,dotplot(PGIM_coxib~Coxib.dose))

with(neg_females,dotplot(PGIM_coxib~Coxib.dose))
with(neg_females,dotplot(PGEM_coxib~Coxib.dose))

neg=subset(data, Cre=="-")
with(neg,dotplot(PGEM_coxib~Coxib.dose, groups=Sex))
with(neg,dotplot(PGEM_coxib.LPS~Coxib.dose, groups=Sex))
with(neg,dotplot(PGIM_coxib.LPS~Coxib.dose, groups=Sex))
with(neg,dotplot(PGIM_coxib~Coxib.dose, groups=Sex))