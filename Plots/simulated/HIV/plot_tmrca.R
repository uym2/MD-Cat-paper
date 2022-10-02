require(ggplot2)

d = read.table("tmrca.txt",header=T)
h = data.frame("treeModel"=c("D750_11_10","D750_3_25","D995_11_10","D995_3_25"),
               "height"=c(29.3667,66.8334,22.4934,32.4669),
               "name"=c("M3","M4","M1","M2"))

d = merge(d,h)
d$clockModel =factor(d$clockModel,levels = c("exp","gamma","lognorm",
                                              "clock4","clock3","clock1","clock2",
                                              "trilnormcave","trilnormvex","trilnorm",
                                             "quartlnorm","uniform"),
                     labels = c("Exponential","Gamma","Lognormal",
                                "Bimodal 1","Bimodal 2","Bimodal 3","Bimodal 4",
                                "Trimodal 1","Trimodal 2","Trimodal 3",
                                "Quartmodal","Uniform"))
d$method = factor(d$method,levels=c("Bstrict","Blnorm","lsd","wlogdate","emd"),
                  labels=c("BEAST−strict−clock","BEAST−lognormal","LSD","wLogDate","MD-Cat"))

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.05, 0.5, 0.95, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

ggplot(d[!d$method %in% c("BEAST−lognormal") & d$clockModel %in% c("Uniform","Exponential","Gamma","Lognormal"),],
       aes(x=clockModel,y=-tmrca/height*100,fill=method)) + 
  stat_summary(position=position_dodge2(width=0.75),fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position = position_dodge2(width=0.9)) +
  #stat_summary(aes(group=method),geom="line") +
  geom_hline(yintercept = 0,linetype=2,size=1) + 
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(name="") +
  #facet_wrap(~name,scale="free") +
  ylab("tMRCA normalized bias (%)") + xlab("clock model") +
  theme_classic() + theme(legend.title = element_blank(), legend.position = c(0.5,0.8)) +
  theme(panel.spacing = unit(0,"pt"))
ggsave("results_tmrca_canonical.pdf",width = 4,height = 4)

ggplot(d[!d$method %in% c("BEAST−lognormal") & d$clockModel %in% c("Bimodal 1","Bimodal 2","Bimodal 3","Bimodal 4"),],
       aes(x=clockModel,y=-tmrca/height*100,fill=method)) + 
  stat_summary(position=position_dodge2(width=0.75),fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position = position_dodge2(width=0.9)) +
  #stat_summary(aes(group=method),geom="line") +
  geom_hline(yintercept = 0,linetype=2,size=1) + 
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(name="") +
  #facet_wrap(~name,scale="free") +
  ylab("tMRCA normalized bias (%)") + xlab("clock model") +
  theme_classic() + theme(legend.title = element_blank(), legend.position = "None") +
  theme(panel.spacing = unit(0,"pt"))
ggsave("results_tmrca_bimodals.pdf",width = 4,height = 4)

ggplot(d[!d$method %in% c("BEAST−lognormal") & d$clockModel %in% c("Trimodal 1","Trimodal 2","Trimodal 3","Quartmodal"),],
       aes(x=clockModel,y=-tmrca/height*100,fill=method)) + 
  stat_summary(position=position_dodge2(width=0.75),fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position = position_dodge2(width=0.9)) +
  #stat_summary(aes(group=method),geom="line") +
  geom_hline(yintercept = 0,linetype=2,size=1) + 
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(name="") +
  #facet_wrap(~name,scale="free") +
  ylab("tMRCA normalized bias (%)") + xlab("clock model") +
  theme_classic() + theme(legend.title = element_blank(), legend.position = "None") +
  theme(panel.spacing = unit(0,"pt"))
ggsave("results_tmrca_multimodals.pdf",width = 4,height = 4)

