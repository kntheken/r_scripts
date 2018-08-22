##Read in data##
data_BP=read.csv("./r_scripts/source_data/Study19.csv")
str(data_BP)

library(readxl)
mouse_id=read_excel("./r_scripts/source_data/BP_study_mouse_info.xlsx",sheet="Study19")

library(tidyr)
library(chron)
library(psych)
library (ggplot2)
library(reshape)

##Reformat to split by measurement and position##
data_BP2=gather(data_BP, key="Measurement", value="Value", -c("Time","Period", "Phase"))%>% 
separate(col="Measurement", into=c("Position","Measurement"))

##Convert times##
data_BP2$Time2=as.POSIXlt(data_BP2$Time, "%m/%d/%Y %H:%M", tz="")


##Set non-physiologic values to NA##
data_BP2$Value=ifelse(data_BP2$Measurement=="Diastolic" & data_BP2$Value<=30,NA, data_BP2$Value)
data_BP2$Value=ifelse(data_BP2$Measurement=="Diastolic" & data_BP2$Value>250,NA, data_BP2$Value)
data_BP2$Value=ifelse(data_BP2$Measurement=="Systolic" & data_BP2$Value<=40,NA, data_BP2$Value)
data_BP2$Value=ifelse(data_BP2$Measurement=="Systolic" & data_BP2$Value>250,NA, data_BP2$Value)
data_BP2$Value=ifelse(data_BP2$Measurement=="Pressure" & data_BP2$Value<=30,NA, data_BP2$Value)
data_BP2$Value=ifelse(data_BP2$Measurement=="Pressure" & data_BP2$Value>250,NA, data_BP2$Value)

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

summary_BP3=subset(summary_BP, select=-c(sd,sem))%>% unite(col=MPP, Measurement,Period, Phase, sep=".") %>% spread(key=MPP,value=average)

BP1=merge(mouse_id,summary_BP,  by="Position")
BP2=merge( mouse_id, summary_BP2, by= "Position")
BP3=merge(mouse_id, summary_BP3, by="Position")

write.csv(BP3, file="Study19BP_summary.csv")


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


