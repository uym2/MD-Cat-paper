require(ggplot2)

dht = read.table("../HIV/tmrca.txt",header=T)
h = data.frame("treeModel"=c("D750_11_10","D750_3_25","D995_11_10","D995_3_25"),
               "height"=c(29.3667,66.8334,22.4934,32.4669),
               "name"=c("M3","M4","M1","M2"))

dht = merge(dht,h)
dht$clockModel =factor(dht$clockModel,levels = c("exp","gamma","lognorm",
                                              "clock4","clock3","clock1","clock2",
                                              "trilnormcave","trilnormvex","trilnorm",
                                             "quartlnorm","uniform"),
                     labels = c("Exponential","Gamma","Lognormal",
                                "Bimodal 1","Bimodal 2","Bimodal 3","Bimodal 4",
                                "Trimodal 1","Trimodal 2","Trimodal 3",
                                "Quartmodal","Uniform"))
dht$method = factor(dht$method,levels=c("Bstrict","Blnorm","lsd","wlogdate","emd"),
                  labels=c("BEAST: strict","BEAST: LogNormal","LSD","wLogDate","MD-Cat"))


dht$host = "intra-host"
dht[dht$name %in% c("M3","M4"),]$host = "inter-host"
dht$host = factor(dht$host,levels = c("intra-host","inter-host"))

dht$nsmpltime = "11 sampling times"
dht[dht$name %in% c("M2","M4"),]$nsmpltime = "3 sampling times"
head(dht)

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.5, 0.95))
  names(r) <- c("ymin", "y", "ymax")
  r
}
quantiles_75 <- function(x) {
  r <- quantile(x, probs=c(0.25, 0.5, 0.75))
  names(r) <- c("ymin", "y", "ymax")
  r
}
view(dht %>% group_by(clockModel,method) %>% summarise(n=n()))

require(scales)
ggplot(dht[!dht$method %in% c("BEAST: LogNormal") & dht$clockModel %in% c("Uniform","Exponential","Gamma","Lognormal"),],
       aes(x=clockModel,y=-tmrca/height,fill=method)) + 
  stat_summary(position=position_dodge(width=0.9),width=.5,
               fun.data = quantiles_95,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width = 0.9)) +
  #stat_summary(aes(group=method),geom="line") +
  geom_hline(yintercept = 0,linetype=3) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(2,7,5:6)]) +
  scale_y_continuous(label=percent)+
  scale_x_discrete(name="") +
  #facet_wrap(~name,scale="free") +
  ylab("tMRCA normalized bias") + xlab("clock model") +
  theme_classic() + theme(legend.title = element_blank(), legend.position = "none") +
  theme(panel.spacing = unit(0,"pt"))
ggsave("../HIV/results_tmrca_canonical.pdf",width = 4,height = 4)

ggplot(dht[!dht$method %in% c("BEAST: LogNormal") & dht$clockModel %in% c("Bimodal 1","Bimodal 2","Bimodal 3","Bimodal 4"),],
       aes(x=clockModel,y=-tmrca/height,fill=method)) + 
  stat_summary(position=position_dodge(width=0.9),width=.5,
               fun.data = quantiles_95,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width = 0.9)) +
  #stat_summary(aes(group=method),geom="line") +
  geom_hline(yintercept = 0,linetype=3) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(2,7,5:6)]) +
  scale_y_continuous(label=percent)+
  scale_x_discrete(name="") +
  #facet_wrap(~name,scale="free") +
  ylab("tMRCA normalized bias") + xlab("clock model") +
  theme_classic() + theme(legend.title = element_blank(), legend.position = c(.3,0.8)) +
  theme(panel.spacing = unit(0,"pt"))
ggsave("../HIV/results_tmrca_bimodals.pdf",width = 4,height = 4)

ggplot(dht[!dht$method %in% c("BEAST: LogNormal") & dht$clockModel %in% c("Trimodal 1","Trimodal 2","Trimodal 3","Quartmodal"),],
       aes(x=clockModel,y=-tmrca/height,fill=method)) + 
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width = 0.9)) +
  #stat_summary(aes(group=method),geom="line") +
  geom_hline(yintercept = 0,linetype=3) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(2,7,5:6)]) +
  scale_y_continuous(label=percent)+
  scale_x_discrete(name="") +
  scale_x_discrete(name="") +
  #facet_wrap(~name,scale="free") +
  ylab("tMRCA normalized bias (%)") + xlab("clock model") +
  theme_classic() + theme(legend.title = element_blank(), legend.position = "None") +
  theme(panel.spacing = unit(0,"pt"))
ggsave("../HIV/results_tmrca_multimodals.pdf",width = 4,height = 4)


ggplot(dht[!dht$method %in% c("BEAST: LogNormal","LSD"),],
       aes(y=clockModel,x=-tmrca/height,fill=method)) + 
  stat_summary(position=position_dodge(width=0.9),width=.5,
               fun.data = quantiles_95,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width = 0.9),size=0.2) +
  #stat_summary(aes(group=method),geom="line") +
  geom_vline(xintercept = 0,linetype=1) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(2,5:6)]) +
  scale_x_continuous(label=percent)+
  scale_y_discrete(name="") +
  facet_wrap(nsmpltime~host,scale="free_x") +
  xlab("tMRCA normalized bias (%)") + ylab("") +
  theme_bw() + theme(legend.title = element_blank(), legend.position = "right") +
  theme(panel.spacing = unit(0,"pt"))
ggsave("../HIV/results_tmrca_all.pdf",width = 11.2,height = 7)
