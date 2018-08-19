##Read in data##
data_BP=read.csv("Study26_BP.csv")
str(data_BP)

library(tidyr)
library(chron)
library(psych)
library (ggplot2)
library(reshape)

##Reformat to split by measurement and position##
data_BP2=gather(data_BP, key="Measurement", value="Value", -c("Time","Period"))%>% 
separate(col="Measurement", into=c("Position","Measurement"))

##Convert times##
data_BP2$Time2=as.POSIXlt(data_BP2$Time, "%m/%d/%Y %H:%M", tz="")


##converts to character##
data_BP2$Time3=format(as.POSIXct(data_BP2$Time2),format="%H:%M")

##Add active/rest phase and other variables##
data_BP2$Phase=ifelse(data_BP2$Time2$hour>=19|data_BP2$Time2$hour<7, "active", "rest")
data_BP2$Period.Phase=with(data_BP2, interaction(Period,Phase))


data_BP2$Period_time=ifelse(data_BP2$Period=="baseline", as.numeric(difftime(data_BP2$Time2, "2016-10-28 0:00:00")), ifelse(data_BP2$Period=="HSD2", as.numeric(difftime(data_BP2$Time2, "2016-11-11 0:00:00")),as.numeric(difftime(data_BP2$Time2, "2016-11-25 0:00:00"))))

data_BP2$Period_time=as.numeric(difftime(data_BP2$Time2, "2016-10-28 0:00:00"))

data_BP_all$Period_day=ifelse(data_BP_all$Period_time<24, "1",
 ifelse(data_BP_all$Period_time>24 & data_BP_all$Period_time<48, "2", 
 ifelse(data_BP_all$Period_time>=48 & data_BP_all$Period_time<72, "3",
 ifelse(data_BP_all$Period_time>=72 & data_BP_all$Period_time<96, "4",
 ifelse(data_BP_all$Period_time>=96 & data_BP_all$Period_time<120, "5","6")))))

data_BP_all$Phase_day=ifelse(data_BP_all$Period_time<19, "Rest1",
 ifelse(data_BP_all$Period_time>=19 & data_BP_all$Period_time<31, "Active1", 
 ifelse(data_BP_all$Period_time>=31 & data_BP_all$Period_time<43, "Rest2",
 ifelse(data_BP_all$Period_time>=43 & data_BP_all$Period_time<55, "Active2",
 ifelse(data_BP_all$Period_time>=55 & data_BP_all$Period_time<67, "Rest3",
ifelse(data_BP_all$Period_time>=67 & data_BP_all$Period_time<79, "Active3",
ifelse(data_BP_all$Period_time>=79 & data_BP_all$Period_time<91, "Rest4",
ifelse(data_BP_all$Period_time>=91 & data_BP_all$Period_time<103, "Active4","Rest5"))))))))





pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))
ggplot(data_BP2[which(data_BP2$Measurement=="Systolic"|data_BP2$Measurement=="Diastolic"|data_BP2$Measurement=="Pressure"),], aes(Period_time,Value,color=Measurement))+geom_line(alpha=0.5)+pub_specs+ylim(50,200)+facet_grid(Position~.)


