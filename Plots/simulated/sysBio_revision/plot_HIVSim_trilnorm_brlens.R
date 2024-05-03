setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/sysBio_revision")
require(ggplot2)

options(scipen = 999)

d = read.table("HIVSim_trilnorm_brlens.txt",header=T)

ggplot(d,aes(x=brlen,color=treeModel)) + stat_ecdf() + scale_x_log10() + 
  geom_vline(xintercept=0.001,linetype=2,color="red") +
  geom_vline(xintercept=0.005,linetype=2,color="orange") + facet_wrap(~clockModel) + theme_classic() + theme(legend.position = "bottom")
ggsave("HIVSim_trilnorm_brlens.pdf")


ggplot(d,aes(x=brlen)) + geom_histogram(color="black") + scale_x_log10() + geom_vline(xintercept = 0.001,color="red") +
  geom_vline(xintercept = 0.005,color="orange") +
  facet_wrap(~treeModel,scale="free") + theme_classic()

