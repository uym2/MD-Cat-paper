require(ggplot2)

d = read.table("all_divergence.txt",header=T)

h = data.frame("treeModel"=c("D750_11_10","D750_3_25","D995_11_10","D995_3_25"),
               "height"=c(29.3667,66.8334,22.4934,32.4669),
               "name"=c("M3","M4","M1","M2"))

d1 = merge(d,h)
         
d1$method = factor(d1$method,levels=c("Bstrict","Blnorm","lsd","wlogdate","emd"),
                   labels=c("BEAST-strict-clock","BEAST-lognormal","LSD","wLogDate","MD-Cat"))
d1$clockModel =factor(d1$clockModel,levels = c("exp","gamma","lognorm",
                                               "clock4","clock3","clock1","clock2",
                                               "trilnormcave","trilnormvex","trilnorm",
                                               "quartlnorm","uniform"),
                      labels = c("Exponential","Gamma","Lognormal",
                                 "Bimodal 1","Bimodal 2","Bimodal 3","Bimodal 4",
                                 "Trimodal 1","Trimodal 2","Trimodal 3",
                                 "Quartmodal","Uniform"))

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.05, 0.5, 0.95, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

d1$error = sqrt(d1$mse)/d1$height

d1$host = "intra-host"
d1[d1$name %in% c("M3","M4"),]$host = "inter-host"
d1$host = factor(d1$host,levels = c("intra-host","inter-host"))

d1$nsmpltime = "11 sampling times"
d1[d1$name %in% c("M2","M4"),]$nsmpltime = "3 sampling times"

ggplot(d1[!d1$method %in% c("BEAST-lognormal") & d1$clockModel %in% c("Exponential","Lognormal","Gamma","Uniform"),],
       aes(x=clockModel,y=100*error,fill=method)) + 
  #geom_boxplot(outlier.alpha = 0.5,outlier.size = 0.1,notch=T) + 
  stat_summary(position=position_dodge2(width=0.75),fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position=position_dodge2(width=0.9)) +
  scale_fill_brewer(palette = "Dark2") +
  #scale_y_log10() +
  #coord_cartesian(ylim = c(0.007,5)) +
  #facet_wrap(~clockModel,nrow=3) +
  ylab("normalized error (%)") + 
  theme_classic() + theme(legend.title = element_blank(),
                          legend.position = c(0.5,0.8),axis.title.x = element_blank()) +
  theme(panel.spacing = unit(0,"pt"))
ggsave("results_mse_canonical.pdf",width = 4,height = 4)

ggplot(d1[!d1$method %in% c("BEAST-lognormal") & d1$clockModel %in% c("Bimodal 1","Bimodal 2","Bimodal 3","Bimodal 4"),],aes(x=clockModel,y=100*error,fill=method)) + 
  #geom_boxplot(outlier.alpha = 0.5,outlier.size = 0.1,notch=T) + 
  stat_summary(position=position_dodge2(width=0.75),fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position=position_dodge2(width=0.9)) +
  scale_fill_brewer(palette = "Dark2") +
  #scale_y_log10() +
  #coord_cartesian(ylim = c(0.007,5)) +
  #facet_wrap(~clockModel,nrow=3) +
  ylab("normalized error (%)") + 
  theme_classic() + theme(legend.title = element_blank(),
                          legend.position = "None",axis.title.x = element_blank()) +
  theme(panel.spacing = unit(0,"pt"))
ggsave("results_mse_bimodal.pdf",width = 4,height = 4)

ggplot(d1[!d1$method %in% c("BEAST-lognormal") & d1$clockModel %in% c("Trimodal 1","Trimodal 2","Trimodal 3","Quartmodal"),],aes(x=clockModel,y=100*error,fill=method)) + 
  #geom_boxplot(outlier.alpha = 0.5,outlier.size = 0.1,notch=T) + 
  stat_summary(position=position_dodge2(width=0.75),fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position=position_dodge2(width=0.9)) +
  scale_fill_brewer(palette = "Dark2") +
  #scale_y_log10() +
  #coord_cartesian(ylim = c(0.007,5)) +
  #facet_wrap(~clockModel,nrow=3) +
  ylab("normalized error (%)") + xlab("tree model") +
  theme_classic() + theme(legend.title = element_blank(),
                          legend.position = "None",axis.title.x = element_blank()) +
  theme(panel.spacing = unit(0,"pt"))
ggsave("results_mse_multimodal.pdf",width = 4,height = 4)

