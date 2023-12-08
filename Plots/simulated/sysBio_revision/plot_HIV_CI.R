setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/sysBio_revision")
require(ggplot2)

d = read.table("MDCat_HIVsim_CI.txt",header=T)
d$inside = (d$trueTime >= d$p0025 & d$trueTime <= d$p0975)
h = data.frame("treeModel"=c("D750_11_10","D750_3_25","D995_11_10","D995_3_25"),
               "height"=c(29.3667,66.8334,22.4934,32.4669),
               "name"=c("M3","M4","M1","M2"))

d1 = merge(d,h)
d1$clockModel = factor(d1$clockModel,levels=c("trilnormcave","trilnormvex","trilnorm"),labels=c("Trimodal1","Trimodal2","Trimodal3"))

ggplot(d1) + geom_segment(aes(x=trueTime/height,y=p0025/height,xend=trueTime/height,yend=p0975/height,color=inside),size=0.2,alpha=0.5) +
geom_abline() + facet_grid(rows=vars(clockModel),cols=vars(treeModel),scale="free") + 
  theme_classic() + theme(legend.position = "None",panel.border = element_rect(colour = "black", fill=NA)) + 
  xlab("Normalized true branch length (year/height)") + ylab("Normalized estimated branch length (year/height)")
ggsave("MDCat_HIVsim_CI.pdf",width=7,height=4)

head(d1)

library(dplyr)

d1 %>% group_by(clockModel,treeModel) %>% 
  mutate(blg=cut(trueTime/height, quantile(trueTime/height,(0:10)/10), include.lowest = T,
                      labels = c(1:10)*10)) %>% 
  group_by(clockModel,treeModel,blg) %>% 
  mutate(c=n()) %>%
                group_by(clockModel,treeModel,blg,inside) %>% 
  summarise(n=n()/unique(c)[1]) %>% 
ggplot(aes(x=blg,fill=inside,y=n))+
  geom_bar(aes(),stat="identity")+
  facet_grid(rows=vars(clockModel),cols=vars(treeModel),scale="free") + 
  geom_hline(yintercept = 0.95,color="red")+
  scale_y_continuous(labels=percent,name="Portion of branches")+
  scale_x_discrete(name="branch length percentile")+
  scale_fill_brewer(palette = 7,direction = -1,name="True length inside 95% CI: ")+
  theme_classic() + theme(legend.position = "bottom",
                          panel.border = element_rect(colour = "black", fill=NA)) 
ggsave("HIV-support.pdf",width=8.5,height = 4.7)

head(d1)

d1 %>%
  ggplot()+
  stat_ecdf(aes(x=p0025/trueTime,linetype=p0025/trueTime>1),color="blue")+
  stat_ecdf(aes(x=trueTime/p0975,linetype=trueTime/p0975>1),color="green")+
  facet_grid(rows=vars(clockModel),cols=vars(treeModel),scale="free") + 
  geom_vline(xintercept = 1,color="red")+
  scale_x_continuous(name="Portion of branches")+
  #scale_x_discrete(name="branch length percentile")+
  scale_fill_brewer(palette = 7,direction = -1,name="Inside 95% CI: ")+
  theme_classic() + theme(legend.position = "bottom",
                          panel.border = element_rect(colour = "black", fill=NA)) 

d1 %>%  filter(trueTime<p0025 | trueTime>p0975) %>%
  mutate(d=ifelse(trueTime<p0025,(p0025-trueTime)/trueTime,(trueTime-p0975)/trueTime)) %>%
  ggplot()+
  stat_ecdf(aes(x=d,color=clockModel,linetype=treeModel))+
  #facet_grid(cols=vars(treeModel),scale="free") + 
  #geom_vline(xintercept = 1,color="red")+
  scale_color_brewer(palette = "Set2")+
  scale_x_continuous(name=expression(abs(true - CI~boundary)/true),trans="log10",labels=percent,
                     breaks = c(0.01,0.033,0.1,0.33,1,3.3,1))+
  coord_cartesian(xlim=c(0.0005,8))+
  #scale_x_discrete(name="branch length percentile")+
  scale_fill_brewer(palette = 7,direction = -1,name="Inside 95% CI: ")+
  theme_bw() + theme(legend.position = "right",
                          panel.border = element_rect(colour = "black", fill=NA)) 

ggsave("HIV-support-delta.pdf",width=8,height = 2.3)
