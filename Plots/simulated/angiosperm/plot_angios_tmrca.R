setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/angiosperm")

require(ggplot2)

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

d = read.table("results_tmrca.txt",header=T)
#d2 = read.table("results_tmrca_moreBEAST.txt",header=F)
#d = rbind(d1,d2)

d$method = factor(d$method,levels=c("wlogdate","emd","Blnorm",
                                    "BEAST_strict","BEAST_rcla","reltime","BEAST_lnorm"),
                  labels = c("wLogDate","MD-Cat","BEAST_lognorm",
                             "BEAST_strict","BEAST_rcla","RelTime","BEAST_lnorm_old"))

ggplot(d[! d$method %in% c("BEAST_lnorm_old"),],aes(x=method,y=140-tmrca,fill=method)) + 
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_95,geom="boxplot") +
  stat_summary() + scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 140,linetype=2,size=1) + 
  facet_wrap(~clockModel,scale="free",nrow = 1) + 
  ylab("tMRCA") + 
  theme_classic() + 
  theme(axis.ticks.x = element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank()) +
  theme(legend.title = element_blank(),legend.position = "bottom")
ggsave("angios_tmrca.pdf",width=6,height=4)

