##Read in data##
data_BP_cele=read.csv("Study22BP.csv")
str(data_BP_cele)
library(chron)
library(psych)
library (ggplot2)

##Convert times##
data_BP_cele$Time2=as.POSIXlt(data_BP_cele$Time, "%m/%d/%Y %H:%M", tz="")
data_BP_cele$Time3=format(as.POSIXct(data_BP_cele$Time2),format="%H:%M")##converts to character##

##Add active/rest phase and other variables##
data_BP_cele$Phase=ifelse(data_BP_cele$Time2$hour>=19|data_BP_cele$Time2$hour<7, "active", "rest")
data_BP_cele$Period.Phase=with(data_BP_cele, interaction(Period,Phase))

##Summary statistics by phase and period##
describeBy(data_BP_cele, data_BP_cele$Period.Phase)

##Graph##
pub_specs=theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"),
 axis.title.x=element_text(size=12), axis.title.y=element_text(size=12),title=element_text(size=14))
pdf("BP_by_day.pdf")
ggplot(data_BP_cele, aes(Day,A1.Pressure,color=Phase))+ geom_point(alpha=0.3)+labs(title="MAP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A3.Pressure,color=Phase))+ geom_point(alpha=0.3)+labs(title="MAP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A4.Pressure,color=Phase))+ geom_point(alpha=0.3)+labs(title="MAP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A5.Pressure,color=Phase))+ geom_point(alpha=0.3)+labs(title="MAP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A9.Pressure,color=Phase))+ geom_point(alpha=0.3)+labs(title="MAP")+ pub_specs
ggplot(data_BP_cele, aes(Day,C3.Pressure,color=Phase))+ geom_point(alpha=0.3)+labs(title="MAP")+ pub_specs
ggplot(data_BP_cele, aes(Day,C5.Pressure,color=Phase))+ geom_point(alpha=0.3)+labs(title="MAP")+ pub_specs
ggplot(data_BP_cele, aes(Day,C10.Pressure,color=Phase))+ geom_point(alpha=0.3)+labs(title="MAP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A1.Systolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="SBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A3.Systolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="SBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A4.Systolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="SBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A5.Systolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="SBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A9.Systolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="SBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,C3.Systolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="SBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,C5.Systolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="SBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,C10.Systolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="SBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A1.Diastolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="DBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A3.Diastolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="DBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A4.Diastolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="DBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A5.Diastolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="DBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A9.Diastolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="DBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,C3.Diastolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="DBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,C5.Diastolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="DBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,C10.Diastolic,color=Phase))+ geom_point(alpha=0.3)+labs(title="DBP")+ pub_specs
ggplot(data_BP_cele, aes(Day,A1.Heart_Rate,color=Phase))+ geom_point(alpha=0.3)+labs(title="HR")+ pub_specs
ggplot(data_BP_cele, aes(Day,A3.Heart_Rate,color=Phase))+ geom_point(alpha=0.3)+labs(title="HR")+ pub_specs
ggplot(data_BP_cele, aes(Day,A4.Heart_Rate,color=Phase))+ geom_point(alpha=0.3)+labs(title="HR")+ pub_specs
ggplot(data_BP_cele, aes(Day,A5.Heart_Rate,color=Phase))+ geom_point(alpha=0.3)+labs(title="HR")+ pub_specs
ggplot(data_BP_cele, aes(Day,A9.Heart_Rate,color=Phase))+ geom_point(alpha=0.3)+labs(title="HR")+ pub_specs
ggplot(data_BP_cele, aes(Day,C3.Heart_Rate,color=Phase))+ geom_point(alpha=0.3)+labs(title="HR")+ pub_specs
ggplot(data_BP_cele, aes(Day,C5.Heart_Rate,color=Phase))+ geom_point(alpha=0.3)+labs(title="HR")+ pub_specs
ggplot(data_BP_cele, aes(Day,C10.Heart_Rate,color=Phase))+ geom_point(alpha=0.3)+labs(title="HR")+ pub_specs
dev.off()

##Create subsets##
data_BP_baseline_active=data_BP_cele[which(data_BP_cele$Period.Phase=="baseline.active"),]
data_BP_baseline_rest=data_BP_cele[which(data_BP_cele$Period.Phase=="baseline.rest"),]
data_BP_HSD1_active=data_BP_cele[which(data_BP_cele$Period.Phase=="HSD1.active"),]
data_BP_HSD1_rest=data_BP_cele[which(data_BP_cele$Period.Phase=="HSD1.rest"),]
data_BP_HSDC_active=data_BP_cele[which(data_BP_cele$Period.Phase=="HSDC.active"),]
data_BP_HSDC_rest=data_BP_cele[which(data_BP_cele$Period.Phase=="HSDC.rest"),]
data_BP_HSD2_active=data_BP_cele[which(data_BP_cele$Period.Phase=="HSD2.active"),]
data_BP_HSD2_rest=data_BP_cele[which(data_BP_cele$Period.Phase=="HSD2.rest"),]