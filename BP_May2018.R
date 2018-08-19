##Read in data##
data_BP=read.csv("MayBP2.csv")
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
data_BP2$Period_time=ifelse(data_BP2$Period=="baseline", as.numeric(difftime(data_BP2$Time2, "2018-05-04 0:00:00")), ifelse(data_BP2$Period=="HSD", as.numeric(difftime(data_BP2$Time2, "2018-05-18 0:00:00")),as.numeric(difftime(data_BP2$Time2, "2018-06-01 0:00:00"))))

##Summarizing data for each measurement##
library(dplyr)

Q1=function(x){
	quantile(x,0.25, na.rm=TRUE)}
Q3=function(x){
	quantile(x,0.75, na.rm=TRUE)}
sem=function (x) {sd(x, na.rm=TRUE)/sqrt(length(x))}


summary_BP=subset(data_BP2, select=-Time2) %>% group_by(Position,Period,Phase,Measurement) %>% summarize (average=mean(Value, na.rm=TRUE), sd=sd(Value, na.rm=TRUE),sem=sem(Value))

summary_BP2=subset(summary_BP, select=-c(sd,sem))%>% spread(key=Phase,value=average)
summary_BP2$percent.dip=(summary_BP2$active-summary_BP2$rest)*100/summary_BP2$active
summary_BP2$delta.dip=summary_BP2$active-summary_BP2$rest

pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))
ggplot(data_BP2[which(data_BP2$Measurement=="Systolic"|data_BP2$Measurement=="Diastolic"|data_BP2$Measurement=="Pressure"),], aes(Period_time,Value,color=Measurement))+geom_point(alpha=0.5)+pub_specs+facet_grid(Position~Period)+ylim(50,200)+xlim(0,150)


ggplot(summary_BP, aes(Period,average,group=Position, color=Position))+geom_point()+geom_line()+pub_specs+facet_grid(Measurement~Phase, scales="free_y")+geom_errorbar(aes(ymin=average-sem,ymax=average+sem, width=0.1))
ggplot(summary_BP2, aes(Period,percent.dip,group=Position, color=Position))+geom_point()+geom_line()+pub_specs+facet_grid(Measurement~Position, scales="free_y")
ggplot(summary_BP2, aes(Period,delta.dip,group=Position, color=Position))+geom_point()+geom_line()+pub_specs+facet_grid(Measurement~Position, scales="free_y")


