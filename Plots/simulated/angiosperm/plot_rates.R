require(ggplot2)
#setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/angiosperm")

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

d1 = read.table("mutation_rates.txt",header=T)
d2 = read.table("results_rate_moreBEAST.txt",header=T)
d=rbind(d1,d2)
#d$method = factor(d$method,levels = c("wLogDate","MD-Cat","BEAST_lnorm","BEAST_strict","BEAST_rcla"))


d$method = factor(d$method,levels = c("BEAST_lnorm",
                             "BEAST_strict","BEAST_rcla","RelTime","wLogDate","MD-Cat"))

ggplot(d,aes(x=method,y=log10(exp(1))*error,fill=method)) + 
  stat_summary(position=position_dodge(width=0.9),width=.5,
               fun.data = quantiles_95,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  #geom_boxplot(position="dodge") + 
  stat_summary(position=position_dodge2(width = 0.9)) + 
  #scale_fill_manual(values = c("#D95F02","#7570B3")) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,5:6)]) +
  xlab("simulation") + ylab("root mean squared logarithmic error (RMSLE)") +
  theme_classic() +
  facet_grid(~sub(pattern = "scenario","sc. ",scenario),scales="free_x",space="free", switch = "x") + 
  theme(axis.ticks.x = element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank()) +
  theme(legend.title = element_blank(),legend.position = "none") 
ggsave("mutation_rates.pdf",width=4,height=4)

ggplot(d,aes(y=method,x=log10(exp(error)),fill=method)) + 
  stat_summary(position=position_dodge(width=0.9),width=.5,
               fun.data = quantiles_95,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  geom_vline(xintercept = 0)  stat_summary(position=position_dodge2(width = 0.9)) + 
  #scale_fill_manual(values = c("#D95F02","#7570B3")) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,5:6)]) +
  ylab("simulation") + xlab("rate logarithmic error (RMSLE)") +
  theme_classic() +
  facet_grid(sub(pattern = "scenario","Sc. ",scenario)~.,scales="free_y",space="free", switch = "y") + 
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        legend.title = element_blank(),legend.position = "none") 
ggsave("mutation_rates_flipped.pdf",width=3,height=5)

