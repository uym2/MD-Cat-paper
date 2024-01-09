setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/angiosperm")

require(ggplot2)

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

d = read.table("results_tmrca.txt",header=T)
#d2 = read.table("results_tmrca_moreBEAST.txt",header=F)
#d = rbind(d1,d2)

d$method = factor(d$method,levels=c("Blnorm",
                                    "BEAST_strict","BEAST_rcla","BEAST_lnorm","reltime","wlogdate","emd"),
                  labels = c("BEAST: LogNorm",
                             "BEAST: strict","BEAST: RCLA","BEAST_lnorm_old","RelTime","wLogDate","MD-Cat"))

ggplot(d[! d$method %in% c("BEAST_lnorm_old"),],
       aes(x=method,y=140-tmrca,fill=method)) + 
  geom_hline(yintercept = 140,linetype=1,color="grey60") + 
  stat_summary(position=position_dodge(width=0.9),width=.5,
               fun.data = quantiles_95,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge(width=0.9),size=0.4) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,7,5:6)]) +
  ylab("tMRCA") + 
  theme_classic() + 
  facet_grid(~clockModel,scale="free_x",space = "free", switch = "x") + 
  theme(axis.ticks.x = element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank()) +
  theme(legend.title = element_blank(),legend.position = c(.75,.9))+
  guides(fill  = guide_legend(nrow = 3))
ggsave("angios_tmrca.pdf",width=6.5,height=4)


ggplot(d[! d$method %in% c("BEAST_lnorm_old"),],
       aes(y=method,x=140-tmrca,fill=method)) + 
  geom_vline(xintercept = 140,linetype=1,color="grey50",size=1) + 
  stat_summary(position=position_dodge(width=0.9),width=.5,
               fun.data = quantiles_95,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge(width=0.9),size=0.4) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,7,5:6)]) +
  scale_x_continuous("tMRCA",breaks = c(140,200,300,400)) + 
  theme_classic() + 
  facet_grid(sub("scenario","Scenario ",clockModel)~.,scale="free_y",space = "free", switch = "y", as.table = F) + 
  theme(axis.ticks.y = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank()) +
  theme(legend.title = element_blank(),legend.position = c(.8,.87))+
  guides(fill  = guide_legend(ncol = 1))
ggsave("angios_tmrca_flipped.pdf",width=3.8,height=5.5)
