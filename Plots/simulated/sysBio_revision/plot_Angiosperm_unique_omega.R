setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/sysBio_revision")
require(ggplot2)

d = read.table("MDCat_Angiosperm_unique_omega.txt",header=T)

ggplot(d[d$k_input %in% c(2,5,10,25,50,100,180) & d$k_uniq<=d$k_input,],aes(x=k_input,y=k_uniq,color=scenario)) + 
  stat_summary(geom="path",fun=median) + 
  stat_summary(fun=median) + xlab("Input no. rate categories") + ylab("No. unique rate categories") +
  theme_classic() + theme(legend.position = "bottom")
ggsave("MDCat_Angiosperm_unique_omega.pdf")
