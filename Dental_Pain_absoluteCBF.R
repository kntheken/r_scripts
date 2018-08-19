##Read in data, gather, and merge##
library(readxl)

pre5=read_excel("Dental_Pain_CBF.xlsx",sheet="Pre-5")%>% gather(key="Region", value="Pre5",-Subject) 
pre45=read_excel("Dental_Pain_CBF.xlsx",sheet="Pre-4")%>% gather(key="Region", value="Pre4",-Subject)%>% merge (.,pre5,by=c("Subject","Region"))
pre35=read_excel("Dental_Pain_CBF.xlsx",sheet="Pre-3")%>% gather(key="Region", value="Pre3",-Subject)%>% merge (.,pre45,by=c("Subject","Region"))
pre25=read_excel("Dental_Pain_CBF.xlsx",sheet="Pre-2")%>% gather(key="Region", value="Pre2",-Subject)%>% merge (.,pre35,by=c("Subject","Region"))
pre15=read_excel("Dental_Pain_CBF.xlsx",sheet="Pre-1")%>% gather(key="Region", value="Pre1",-Subject)%>% merge (.,pre25,by=c("Subject","Region"))
pre=read_excel("Dental_Pain_CBF.xlsx",sheet="Pre")%>% gather(key="Region", value="Pre",-Subject)%>% merge (.,pre15,by=c("Subject","Region"))
post=read_excel("Dental_Pain_CBF.xlsx",sheet="Post")%>% gather(key="Region", value="Post",-Subject)%>% merge (.,pre,by=c("Subject","Region"))
post1=read_excel("Dental_Pain_CBF.xlsx",sheet="Post1")%>% gather(key="Region", value="Post1",-Subject)%>% merge (.,post,by=c("Subject","Region"))
post2=read_excel("Dental_Pain_CBF.xlsx",sheet="Post2")%>% gather(key="Region", value="Post2",-Subject)%>% merge (.,post1,by=c("Subject","Region"))
post3=read_excel("Dental_Pain_CBF.xlsx",sheet="Post3")%>% gather(key="Region", value="Post3",-Subject)%>% merge (.,post2,by=c("Subject","Region"))
CBF=read_excel("Dental_Pain_CBF.xlsx",sheet="Post4")%>% gather(key="Region", value="Post4",-Subject)%>% merge (.,post3,by=c("Subject","Region"))

rCBF=CBF
rCBF$Pre5=CBF$Pre5/CBF$Pre
rCBF$Pre4=CBF$Pre4/CBF$Pre
rCBF$Pre3=CBF$Pre3/CBF$Pre
rCBF$Pre2=CBF$Pre2/CBF$Pre
rCBF$Pre1=CBF$Pre1/CBF$Pre
rCBF$Pre=CBF$Pre/CBF$Pre
rCBF$Post=CBF$Post/CBF$Pre
rCBF$Post1=CBF$Post1/CBF$Pre
rCBF$Post2=CBF$Post2/CBF$Pre
rCBF$Post3=CBF$Post3/CBF$Pre
rCBF$Post4=CBF$Post4/CBF$Pre

CBF2=gather(CBF, key="Timepoint", value="Value",-c("Subject","Region"))%>% spread(Region,Value)
rCBF2=gather(rCBF, key="Timepoint", value="Value",-c("Subject","Region"))%>% spread(Region,Value)
rCBF2$Pain=CBF2$Pain