##Read in data##
data_basal_LPS=read.csv("COX2_PartialKO_basal_LPS.csv")
##Dropping outlier 81-1##
data_basal_LPS=data_basal_LPS[!(data_basal_LPS$Mouse_ID =="081-1 "),]

data_basal_heatmap=data_basal[c("Mouse_ID","Sex","KO_Group","PGDM_basal","PGEM_basal","PGIM_basal","TxB2_basal")]

data_LPS_heatmap=data_LPS[c("Mouse_ID","Sex","KO_Group","PGDM_basal","PGEM_basal","PGIM_basal","TxB2_basal",
	"PGDM_NSAID","PGEM_NSAID","PGIM_NSAID", "TxB2_NSAID",
	"PGDM_stimulated","PGEM_stimulated", "PGIM_stimulated", "TxB2_stimulated")]

data_LPS_heatmap=data_LPS_heatmap[order(data_LPS_heatmap$KO_Group),]
row.names(data_LPS_heatmap)=data_LPS_heatmap$Mouse_ID
data_LPS_heatmap=data_LPS_heatmap[,2:15]
LPS.matrix=data.matrix(data_LPS_heatmap)
LPS_heatmap = heatmap(LPS.matrix, Rowv=NA, Colv=NA, col = heat.colors(256), 
	scale="column", margins=c(5,10))

data_basal_heatmap=data_basal_heatmap[order(data_basal_heatmap$KO_Group),]
row.names(data_basal_heatmap)=data_basal_heatmap$Mouse_ID
data_basal_heatmap=data_basal_heatmap[,4:7]
basal.matrix=data.matrix(data_basal_heatmap)
basal_heatmap = heatmap(basal.matrix2, Rowv=NA, Colv=NA, col = heat.colors(256), 
	scale="column", margins=c(5,10))

library(dplyr)
library(NMF)
library(RColorBrewer)
iris2 = iris # prep iris data for plotting
rownames(data_LPS_heatmap)=make.names(data_LPS_heatmap$Mouse_ID)
basal.matrix2 = data_basal_heatmap %>% select(-Sex,-KO_Group,-Mouse_ID) %>% as.matrix()
basal.matrix3 = data_basal_heatmap %>% select(-Sex,-KO_Group,-Mouse_ID, -PGDM_basal, -TxB2_basal) %>% as.matrix()
LPS.matrix2 = data_LPS_heatmap %>% select(-Sex,-KO_Group,-Mouse_ID) %>% as.matrix()
aheatmap(LPS.matrix2, color = "-RdBu:50", scale = "col", breaks = 0,
         annRow = data_LPS_heatmap["KO_Group"], annColors = "Set2", 
         distfun = "pearson", treeheight=c(200, 50), 
         fontsize=13, cexCol=.7, 
         filename="heatmap.png", width=8, height=16)

ggplot(data_LPS_heatmap, aes(Sex,KO_Group,fill = PGEM_stimulated)) + geom_raster()
ggplot(data_LPS, aes(PGIM_stimulated,PGEM_stimulated,fill = PGDM_stimulated)) + geom_raster()

ggplot(data_LPS, aes(Renal_medulla_Ptgs2, KO_Group))+stat_density(aes(fill = ..density..), geom = "raster", position = "identity")