##Read in data##
data_30=read.csv("Study30_thrombosis.csv")
data_31=read.csv("Study31_thrombosis.csv")
data_32=read.csv("Study32_thrombosis.csv")
data_3031=rbind(data_30,data_31)
data_all=rbind(data_3031,data_32)
data_all$Mouse.rep=with(data_all, interaction(Mouse,Rep))

library(reshape)
data_thrombosis2=melt(data_all,id.vars=c("Mouse","Rep", "Mouse.rep"))

##Convert time variable to numeric##
data_thrombosis2$time=as.numeric (data_thrombosis2$variable)-1

library(MESS)
library(dplyr)
by_rep=group_by(data_thrombosis2,Mouse.rep) 
AUC=summarize(by_rep,auc=auc(time,value))
agg.AUC=summarize(by_rep,agg.auc=auc(time,value, to=which.max(value)))
dis.AUC=summarize(by_rep,dis.auc=auc(time,value, from=which.max(value)))
TMAX=summarize(by_rep, tmax=which.max(value))
VMAX=summarize(by_rep, vmax=max(value))

data_1=merge(AUC, agg.AUC, all=TRUE)
data_2=merge(dis.AUC, TMAX, all=TRUE)
data_3=merge(data_1,VMAX,all=TRUE)
data_4=merge(data_3,data_2,all=TRUE)

data_5=data_all[c("Mouse","Rep", "Mouse.rep")]

data_summary=merge(data_5,data_4, by="Mouse.rep",all=TRUE)
write.csv(data_summary,"Study30-32_thrombosis_summary.csv")
