##Need all_bio dataframe from dental analysis##

cytokine=luminex.all[c("Subject","Status2","Demo_Gender","Event","IL6.percent","TNFa.percent","MCP1.percent","IL8.percent","IL10.percent")]

cytokine$Group=interaction(cytokine$Status2,cytokine$Demo_Gender)

cytokine.PS1=cytokine[which(cytokine$Event=="Post-surgery 1"),]
cytokine.PS1$Group2=interaction(cytokine.PS1$Group,cytokine.PS1$Subject)

cytokine.PS2=cytokine[which(cytokine$Event=="Post-surgery 2"),]

PS1.heatmap=cytokine.PS1[c("Group2","IL6.percent","TNFa.percent","MCP1.percent","IL8.percent","IL10.percent")]

ggplot(PS1.heatmap.sort, aes(sort(Group2), variable )) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylab("List of cytokines ") +
  xlab("List of patients") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Expression level")

library(superheat)
superheat(PS1.heatmap[,sapply(PS1.heatmap,is.numeric)],scale=TRUE)

pretty.order.rows=FALSE,pretty.order.cols=FALSE,order.cols=as.numeric(PS1.heatmap$Group2)