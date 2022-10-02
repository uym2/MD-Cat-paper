require(ggplot2)

d = read.table("mutation_rates.txt",header=T)
d$method = factor(d$method,levels = c("wLogDate","MD-Cat"))

d$treeModel = factor(d$treeModel,levels=c("D995_11_10","D995_3_25","D750_11_10","D750_3_25"), 
                     labels = c("M1","M2","M3","M4"))
d$clockModel = factor(d$clockModel,levels = c("exp","unif","gamma","lognorm",
                                              "clock4","clock3","clock1","clock2",
                                              "trilnormcave","trilnormvex","trilnorm","quartlnorm"),
                      labels = c("Exponential","Uniform","Gamma","Lognormal",
                                 "Bimodal 1","Bimodal 2","Bimodal 3","Bimodal 4",
                                 "Trimodal 1","Trimodal 2","Trimodal 3","Quartmodal"))

ggplot(d[d$method != "Bstrict",],aes(x=clockModel,y=log10(exp(1))*error,fill=method)) + geom_boxplot(position="dodge") + 
  stat_summary(position=position_dodge2(width = 0.8)) + 
  xlab("clock model") + ylab("mean squared logarithmic error (MSLE)") + 
  scale_fill_manual(values = c("#D95F02","#7570B3")) +
  #scale_fill_brewer(palette = "Dark2") + 
  theme_classic() + 
  theme(legend.title=element_blank(),legend.position = "bottom",axis.text.x = element_text(angle = 15))
ggsave("mus_HIV_sim.pdf",width=6,height=4)
  
  
  
  


