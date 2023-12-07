require(ggplot2)

quantiles_75 <- function(x) {
  r <- quantile(x, probs=c(0.25, 0.5, 0.75))
  names(r) <- c("ymin", "y", "ymax")
  r
}

d1 = read.table("results_brTime.txt",header=T)
d2 = read.table("results_brTime_moreBEAST.txt",header=T)
d=rbind(d1,d2)
d$method = factor(d$method,levels=c("wlogdate","emd","Blnorm","BEAST_strict","BEAST_rcla","reltime","BEAST_lognorm"),
                  labels = c("wLogDate","MD-Cat","BEAST_lognorm","BEAST_strict","BEAST_rcla","RelTime","BEAST_lnorm_old"))

ggplot(d[d$method != "BEAST_lnorm_old",],aes(x=trueLength,y=estLength,color=method)) + 
  #geom_point(alpha=0.3,size=0.1) + 
  stat_summary(size=0.1, alpha=0.2) + 
  #stat_summary(geom="line") + 
  geom_smooth() + 
  geom_abline(linetype=1) + 
  scale_x_log10() + scale_y_log10() + 
  scale_color_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,7,5:6)]) +
  facet_grid(~clockModel) + 
  theme_classic() + theme(legend.position = "none",legend.title = element_blank()) + 
  xlab("true branch length (million years)") + ylab("estimated branch length (million years)")+
  coord_cartesian(xlim=c(0.0005,200),ylim=c(0.0005,200))
ggsave("angios_brTime.png",width = 9,height = 4)

ggplot(aes(x=trueLength),data=d)+
         stat_ecdf()+
  theme_bw()+
  scale_x_continuous(name="true length (million years)",
                     trans="log",
                     #breaks=c(0.0001,1,5,50,100,150),
                     labels = scales::number)+
  geom_vline(xintercept = c(1,5,50,100,150),color="red",linetype=2)
ggsave("truelengthdist.pdf",width=2.9*0.9,height = 3.8*0.9)

ggplot(d[d$method != "BEAST_lnorm_old",],
       aes(x=cut(trueLength,c(0,1,5,50,100,150),
                  labels=c("<1","[1,5]",",50]",",100]",",150]")
                 ),
           y=(trueLength-estLength)/trueLength,color=method)) + 
  #geom_point(alpha=0.3,size=0.1) + 
  geom_hline(yintercept = 0, linetype=1,color="grey40") + 
  stat_summary(geom = "errorbar",width=0.3)+
  stat_summary(aes(group=method),geom="line")+
  scale_y_continuous(trans="pseudo_log",labels=scales::percent,
                     breaks = c(-1,-2,-4,-8,0,1/2),
          name = expression((estimated - ture) / true)) + 
  #scale_x_log10() + 
  scale_color_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,7,5:6)]) +
  facet_grid(~clockModel) + 
  theme_classic() + theme(legend.position = "none",legend.title = element_blank()) + 
  xlab("true branch length brackets (million years)") 
  #stat_summary(fun.data = quantiles_75,geom="crossbar",size=0.2,width=0.2) 
ggsave("angios_bias.pdf",width = 9.7*0.9,height = 3.8*0.9)

