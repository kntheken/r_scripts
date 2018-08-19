data=read.table("PACER.COX2.txt")
data2=as.data.frame(t(data))
data2$PACER=as.numeric(data2$V1)
data2$PTGS2=as.numeric(data2$V2)
library(reshape)
data3=melt(data, id=c("V1","V2"))
PACER=data3[which(data3$V2=="RP5-973M2.2"),]
PTGS2=data3[which(data3$V2=="PTGS2"),]
PACER$PTGS2=PTGS2$value


library(ggplot2)
pub_specs=theme(panel.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(), axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))
ggplot(PACER, aes(value, PTGS2))+geom_point()+labs(x="PACER",y="PTGS2")+pub_specs+scale_x_log10()+scale_y_log10()
ggplot(PACER, aes(value, PTGS2))+geom_point()+labs(x="PACER",y="PTGS2")+pub_specs+ylim(0,10)

cor(PACER$value,PACER$PTGS2)