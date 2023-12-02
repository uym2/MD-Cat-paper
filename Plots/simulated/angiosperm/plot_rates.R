require(ggplot2)
setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/angiosperm")

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

d1 = read.table("mutation_rates.txt",header=T)
d2 = read.table("results_rate_moreBEAST.txt",header=T)
d=rbind(d1,d2)
d$method = factor(d$method,levels = c("wLogDate","MD-Cat","BEAST_lnorm","BEAST_strict","BEAST_rcla"))

ggplot(d,aes(x=scenario,y=log10(exp(1))*error,fill=method)) + 
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_95,geom="boxplot") +
  #geom_boxplot(position="dodge") + 
  stat_summary(position=position_dodge2(width = 0.85)) + 
  #scale_fill_manual(values = c("#D95F02","#7570B3")) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("simulation") + ylab("root mean squared logarithmic error (RMSLE)") +
  theme_classic() + theme(legend.title = element_blank(),legend.position = "bottom") +
  theme(axis.title.x = element_blank())
ggsave("mutation_rates.pdf",width=4,height=4)
