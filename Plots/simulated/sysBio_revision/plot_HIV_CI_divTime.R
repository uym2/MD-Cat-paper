setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/sysBio_revision")
require(ggplot2)
require(scales)
library(dplyr)

d = read.table("MDCat_HIVsim_CI_new4_divTime.txt",header=T)
d$inside = (d$trueAge >= d$p0025 & d$trueAge <= d$p0975)
h = data.frame("treeModel"=c("D750_11_10","D750_3_25","D995_11_10","D995_3_25"),
               "height"=c(29.3667,66.8334,22.4934,32.4669),
               "name"=c("M3","M4","M1","M2"))
d1=merge(d,h,by="treeModel")
d1$host = "intra-host"
d1[d1$name %in% c("M3","M4"),]$host = "inter-host"
d1$host = factor(d1$host,levels = c("intra-host","inter-host"))

d1$nsmpltime = "11 sampling times"
d1[d1$name %in% c("M2","M4"),]$nsmpltime = "3 sampling times"

d1$clockModel = factor(d1$clockModel,levels=c("trilnormcave","trilnormvex","trilnorm"),labels=c("Trimodal1","Trimodal2","Trimodal3"))

ggplot(d1) + 
  geom_segment(aes(x=trueAge/height,y=p0025/height,xend=trueAge/height,yend=p0975/height,color=inside),linewidth=0.2,alpha=0.5) +
  geom_abline() + 
  facet_grid(rows=vars(clockModel),cols=vars(host,nsmpltime),scale="free") + 
  #facet_grid(rows=vars(clockModel),cols=vars(host),scale="free") + 
  #facet_wrap(~host+nsmpltime,scale="free") + 
  theme_classic() + 
  theme(legend.position = "None",panel.border = element_rect(colour = "black", fill=NA)) + 
  xlab("Normalized true divergence time (year/height)") + 
  ylab("Normalized estimated divergence time (year/height)")
ggsave("MDCat_HIVsim_CI_new_Divtime.pdf",width=7,height=4)

head(d1)



d1 %>% group_by(clockModel,host,nsmpltime) %>% 
  mutate(blg=cut(trueAge/height, quantile(trueAge/height,(0:10)/10), include.lowest = T,
                 labels = c(1:10)*10)) %>% 
  group_by(clockModel,host,nsmpltime,blg) %>% 
  #group_by(clockModel,host,blg) %>% 
  mutate(c=n()) %>%
  group_by(clockModel,host,nsmpltime,blg,inside) %>% 
  #group_by(clockModel,host,blg,inside) %>% 
  summarise(n=n()/unique(c)[1]) %>% 
  ggplot(aes(x=blg,fill=inside,y=n))+
  geom_bar(aes(),stat="identity")+
  facet_grid(rows=vars(clockModel),cols=vars(host,nsmpltime),scale="free") + 
  #facet_grid(rows=vars(clockModel),cols=vars(host),scale="free") + 
  #facet_wrap(~host+nsmpltime,scale="free") + 
  geom_hline(yintercept = 0.95,color="red")+
  scale_y_continuous(labels=percent,name="Portion of nodes")+
  scale_x_discrete(name="divergence time percentile")+
  scale_fill_brewer(palette = 7,direction = -1,name="True divergence time inside 95% CI: ")+
  theme_classic() + theme(legend.position = "bottom",
                          panel.border = element_rect(colour = "black", fill=NA)) 
ggsave("HIV-support_new_Divtime.pdf",width=8.5,height = 4.7)

head(d1)


d1 %>%  filter(trueAge<p0025 | trueAge>p0975) %>%
  mutate(d=ifelse(trueAge<p0025,(p0025-trueAge)/trueAge,(trueAge-p0975)/trueAge)) %>%
  ggplot()+
  stat_ecdf(aes(x=d,linetype=clockModel,color=interaction(nsmpltime,host,sep="\n")))+
  #facet_grid(cols=vars(treeModel),scale="free") + 
  #geom_vline(xintercept = 1,color="red")+
  scale_color_brewer(palette = "Set2")+
  scale_x_continuous(name=expression(abs( true - CI~boundary )/true),trans="log10",labels=percent,
                     breaks = c(0.01,0.033,0.1,0.33,1,3.3,1))+
  coord_cartesian(xlim=c(0.0005,8))+
  #scale_x_discrete(name="branch length percentile")+
  scale_linetype(name="")+
  scale_fill_brewer(palette = 7,direction = -1,name="Inside 95% CI: ")+
  theme_bw() + theme(legend.position = "right",
                     panel.border = element_rect(colour = "black", fill=NA),legend.title = element_blank()) 

ggsave("HIV-support-delta_new_divTime.pdf",width=8,height = 2.5)



d2 = d1 %>% group_by(clockModel,host,nsmpltime) %>% 
  mutate(blg=cut(trueAge/height, quantile(trueAge/height,(0:10)/10), include.lowest = T,
                 labels = c(1:10)*10)) %>% 
  group_by(clockModel,host,nsmpltime) %>% 
  mutate(c=n()) %>%
  group_by(host,nsmpltime,inside,clockModel) %>% 
  summarise(n=n()/unique(c)[1]) %>% filter(inside == FALSE)

