require(ggplot2)

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

d = read.table("results_tmrca.txt",header=T)
d$method = factor(d$method,levels=c("reltime","wlogdate","emd","Blnorm"),
                  labels = c("RelTime","wLogDate","MD-Cat","BEAST"))

ggplot(d,aes(x=method,y=140-tmrca,fill=method)) + 
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

