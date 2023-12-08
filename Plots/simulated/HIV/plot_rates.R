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

ggplot(d[d$method != "Bstrict",],aes(x=clockModel,y=log10(exp(1))*error,fill=method)) + 
  stat_summary(position=position_dodge(width=0.9),width=.5,
               fun.data = quantiles_95,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width = 0.9)) +
  xlab("clock model") + ylab("mean squared logarithmic error (MSLE)") + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(5:6)]) +
  #scale_fill_brewer(palette = "Dark2") + 
  theme_classic() + 
  theme(legend.title=element_blank(),legend.position = "bottom",axis.text.x = element_text(angle = 15))
ggsave("mus_HIV_sim.pdf",width=6,height=4)
  
  
  
  


