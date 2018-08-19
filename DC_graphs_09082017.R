pdf("DC_graphs_09082017.pdf")

ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,value,color=Treatment))+geom_point()+stat_summary(fun.y="median", size=1, geom="line")+scale_x_log10()+pub_specs +labs(title="Ex vivo COX-2 Activity",x="Time (h)",y="Plasma PGE2 (ng/ml)")+theme(legend.position="bottom")+ylim(0,50)
ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,value,color=Treatment))+geom_point()+stat_summary(fun.y="median", size=1, geom="line")+scale_x_log10()+pub_specs +labs(title="Ex vivo COX-1 Activity",x="Time (h)",y="Serum TxB2 (ng/ml)")+theme(legend.position="bottom")+ylim(0,300)

ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,norm_plac,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+scale_x_log10()+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-1 Activity",x="Time (h)",y="Serum TxB2\n (%activity relative to placebo)")
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,norm_plac,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+scale_x_log10()+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-2 Activity",x="Time (h)",y="Plasma PGE2\n (%activity relative to placebo)")

ggplot(data4[which(data4$Assay=="TxB2"),],aes(time,norm_plac,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+scale_x_log10()+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-1 Activity",x="Time (h)",y="Serum TxB2\n (%activity relative to placebo)")
ggplot(data4[which(data4$Assay=="PGE2"),],aes(time,norm_plac,color=Treatment))+geom_point(size=3, na.rm=TRUE)+geom_line(na.rm=TRUE)+scale_x_log10()+ylim(0,300)+pub_specs +facet_wrap(~SubjectID)+theme(legend.position="bottom")+geom_hline(yintercept=100)+labs(title="COX-2 Activity",x="Time (h)",y="Plasma PGE2\n (%activity relative to placebo)")

ggplot(summary_ABPM_COX,aes(Treatment,auc_peak_norm_plac,fill=Treatment))+geom_boxplot()+pub_specs +facet_grid(.~Assay)+theme(legend.position="bottom")+labs(title="COX Suppression over Time by Treatment",subtitle="9 am to 1 pm",y="AUC")
ggplot(summary_ABPM_COX,aes(Treatment,auc12_norm_plac,fill=Treatment))+geom_boxplot()+pub_specs +facet_grid(.~Assay)+theme(legend.position="bottom")+labs(title="COX Suppression over Time by Treatment",subtitle="8 am to 8 pm",y="AUC")
ggplot(summary_ABPM_COX,aes(Treatment,auc24_norm_plac,fill=Treatment))+geom_boxplot()+pub_specs +facet_grid(.~Assay)+theme(legend.position="bottom")+labs(title="COX Suppression over Time by Treatment",subtitle="24 hours",y="AUC")

ggplot(data_ABPMpeak,aes(Treatment,MAP_mean, color=Treatment))+geom_point()+geom_errorbar(aes(ymin=MAP_mean-MAP_sd, ymax=MAP_mean+MAP_sd))+pub_specs+facet_wrap(~SubjectID, nrow=3, ncol=5)+theme(legend.position="bottom")+labs(title="Mean Arterial Pressure", subtitle="9 am - 1 pm", y="Mean Arterial Pressure (mm Hg)")+theme(axis.text.x=element_text(angle=-45, hjust=0))
ggplot(data_ABPM12,aes(Treatment,MAP_mean, color=Treatment))+geom_point()+geom_errorbar(aes(ymin=MAP_mean-MAP_sd, ymax=MAP_mean+MAP_sd))+pub_specs+facet_wrap(~SubjectID, nrow=3, ncol=5)+theme(legend.position="bottom")+labs(title="Mean Arterial Pressure", subtitle="8 am - 8 pm", y="Mean Arterial Pressure (mm Hg)")+theme(axis.text.x=element_text(angle=-45, hjust=0))

ggplot(summary_ABPMpeakd_COX,aes(Group2,auc_peak_norm_plac,fill=Group2))+geom_boxplot()+facet_grid(Assay~Treatment)+pub_specs +theme(legend.position="bottom")+labs(title="COX Suppression by Blood Pressure Response Group*",subtitle="9 am - 1 pm",y="AUC", x="Group",caption="*Greater than 5 mm Hg change in MAP from placebo for either NSAID")+ guides(fill=guide_legend(title="Group"))+scale_y_log10()
ggplot(summary_ABPM12d_COX,aes(Group2,auc12_norm_plac,fill=Group2))+geom_boxplot()+facet_grid(Assay~Treatment)+pub_specs +theme(legend.position="bottom")+labs(title="COX Suppression by Blood Pressure Response Group*",subtitle="8 am - 8 pm",y="AUC", x="Group",caption="*Greater than 5 mm Hg change in MAP from placebo for either NSAID")+ guides(fill=guide_legend(title="Group"))+ scale_y_log10()


ggplot(data_ABPM_COX2,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(.~Group2)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", subtitle="9 am - 1 pm",x="COX-2 Activity (% relative to placebo)")+ guides(color=guide_legend(title="Group"))
ggplot(data_ABPM_COX2,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", subtitle="9 am - 1 pm",x="COX-2 Activity (% relative to placebo)")+ guides(color=guide_legend(title="Group"))


ggplot(data_ABPM_COX4,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+geom_smooth(method="lm", se=FALSE)+facet_grid(.~Group2)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", subtitle="8 am - 8 pm",x="COX-2 Activity (% relative to placebo)")+ guides(color=guide_legend(title="Group"))
ggplot(data_ABPM_COX4,aes(PGE2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-2 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", subtitle="8 am - 8 pm",x="COX-2 Activity (% relative to placebo)")+ guides(color=guide_legend(title="Group"))


ggplot(data_ABPM_COX2,aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", subtitle="9 am - 1 pm",x="COX-1 Activity (% relative to placebo)")+ guides(color=guide_legend(title="Group"))
ggplot(data_ABPM_COX4,aes(TxB2_norm_plac,MAP_mean, color=Group2))+geom_point()+pub_specs+theme(legend.position="bottom")+facet_wrap(~SubjectID, nrow=3, ncol=5)+scale_x_log10()+labs(title="COX-1 Activity vs Mean Arterial Pressure", y="Mean Arterial Pressure (mm Hg)", subtitle="8 am - 8 pm",x="COX-1 Activity (% relative to placebo)")+ guides(color=guide_legend(title="Group"))



dev.off()