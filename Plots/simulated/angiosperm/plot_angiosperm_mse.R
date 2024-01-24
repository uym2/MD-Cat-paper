require(ggplot2)
require(scales)
require(reshape2)
require(tidyverse)

d1 = read.table("all_divergence.txt",header=T)

#d1 = merge(d,h)
         
#d1$method = factor(d1$method,levels=c("Bstrict","Blnorm","lsd","wlogdate","emd"),
#                   labels=c("BEAST-strict-clock","BEAST-lognormal","LSD","wLogDate","MD-Cat"))

d1$method = factor(d1$method,levels=c("Blnorm","Bstrict","Brcla","RelTime","BEAST_lognorm","wLogDate","MD-Cat"),
       labels = c("BEAST: LogNorm","BEAST: strict","BEAST: RLC","RelTime","BEAST_lnorm_old","wLogDate","MD-Cat"))


head(d1)

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

ggplot(d1[!d1$method %in% c("BEAST_lnorm_old"),],
       aes(x=method,y=norm_rmse,fill=method)) + 
  stat_summary(position=position_dodge(width=0.9),width=.5,
               fun.data = quantiles_95,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width = 0.9)) +
  geom_hline(yintercept = 0,linetype=3) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,7,5,6)]) +
  scale_y_continuous(label=percent)+
  ylab("Divergence time RMSE (height normalized)") + 
  theme_classic() +
  facet_grid(~sub(pattern = "scenario","sc. ",scenario),scales="free_x",space="free", switch = "x") + 
  theme(axis.ticks.x = element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank()) +
  theme(legend.title = element_blank(),legend.position = "none") 
ggsave("results_mse_angiosperm.pdf",width=6.5,height=4)



s = d1 %>% group_by(scenario,method) %>% summarise(e= mean(norm_rmse)) %>%
  recast(scenario~method)
s

d1 %>% group_by(method) %>% summarise(e= mean(norm_rmse))

