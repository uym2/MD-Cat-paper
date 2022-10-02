require(ggplot2)

d = read.table("mutation_rates.txt",header=T)
d$method = factor(d$method,levels = c("wLogDate","MD-Cat"))

ggplot(d,aes(x=scenario,y=log10(exp(1))*error,fill=method)) + geom_boxplot(position="dodge") + 
  stat_summary(position=position_dodge2(width = 0.8)) + 
  scale_fill_manual(values = c("#D95F02","#7570B3")) +
  #scale_fill_brewer(palette = "Dark2") +
  xlab("simulation") + ylab("mean squared logarithmic error (MSLE)") +
  theme_classic() + theme(legend.title = element_blank(),legend.position = "bottom") +
  theme(axis.title.x = element_blank())

ggsave("mutation_rates.pdf",width=4,height=4)
