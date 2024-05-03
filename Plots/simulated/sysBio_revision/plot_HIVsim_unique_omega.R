setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/sysBio_revision")
require(ggplot2)

d = read.table("MDCat_HIVsim_unique_omega.txt",header=T)

ggplot(d,aes(x=k_uniq,fill=treeModel)) + geom_density() + facet_wrap(~clockModel,scale="free")


#ggplot(d[d$k_input==50,],aes(x=treeModel,y=k_uniq,fill=clockModel)) + geom_boxplot() + theme_classic() + 
#  theme(legend.position = "bottom",legend.title = element_blank())

ggplot(d[d$k_input %in% c(2,5,10,25,50,100,148,218),],aes(x=k_input,y=k_uniq,color=treeModel)) + 
  stat_summary(geom="path",fun=median) + 
  stat_summary(fun=median) + xlab("Input no. rate categories") + ylab("No. unique rate categories") +
  theme_classic() + theme(legend.position = "bottom")
ggsave("MDCat_HIVsim_unique_omega.pdf")
