require(ggplot2)
require(scales)
require(tidyverse)

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

t1 = read.table("results_brTime.txt",header=T)
t2 = read.table("results_brTime_moreBEAST.txt",header=T)
t=rbind(t1,t2)
t$method = factor(t$method,levels=c("Blnorm","BEAST_strict","BEAST_rcla","reltime","BEAST_lognorm","wlogdate","emd"),
                  labels = c("BEAST_lognorm","BEAST_strict","BEAST_rcla","RelTime","BEAST_lnorm_old","wLogDate","MD-Cat"))


require(dplyr)

ggplot(t[t$method != "BEAST_lnorm_old",],
       aes(x=trueLength,y=estLength,color=method)) + 
  #geom_point(alpha=0.3,size=0.1) + 
  geom_abline(linetype=1) + 
  stat_summary(size=0.15, alpha=0.25) + 
  #stat_summary(geom="line") + 
  #geom_smooth() + 
  scale_x_log10() + scale_y_log10() + 
  scale_color_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,7,5:6)]) +
  facet_grid(sub("_.*","",method)~clockModel) + 
  theme_classic() + theme(legend.position = "bottom",legend.title = element_blank()) + 
  xlab("true branch length (million years)") + 
  ylab("estimated branch length (million years)")
  #coord_cartesian(xlim=c(0.0005,400),ylim=c(0.0005,400))
ggsave("angios_brTime.png",width = 7,height = 7,dpi = "retina")

ggplot(aes(x=trueLength),data=t)+
         stat_ecdf()+
  theme_bw()+
  scale_x_continuous(name="true length (million years)",
                     trans="log",
                     #breaks=c(0.0001,1,5,50,100,150),
                     labels = scales::number)+
  geom_vline(xintercept = c(1,5,50,100,150),color="red",linetype=2)
ggsave("truelengthdist.pdf",width=2.9*0.9,height = 3.8*0.9)

t[t$method != "BEAST_lnorm_old",] %>%
  mutate(estLength = ifelse(estLength==0,10^-5,estLength)) %>%
ggplot(
       aes(x=cut(trueLength, round(quantile(t$trueLength,(0:10)/10)),labels = FALSE),
           y=log10(trueLength/estLength),color=method)) + 
  #geom_point(alpha=0.3,size=0.1) + 
  geom_hline(yintercept = 0, linetype=1,color="grey40") + 
  stat_summary(geom = "errorbar",width=0.3)+
  stat_summary(aes(group=method),geom="line")+
  #scale_y_continuous(tran=scales::trans_new("a",function(x) 
  #  after_stat(sign(x)*(abs(x))),inverse = function(x) sign(x)*1^(abs(x))),labels=scales::percent,
  #                   breaks = c(-1,-2,-4,-8,0,1/2),
  #        name = expression((estimated - ture) / true)) + 
  #scale_x_log10() + 
  scale_y_continuous(name=expression(log[10](true/estimated)))+
  scale_color_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,7,5:6)]) +
  facet_grid(~clockModel) + 
  theme_classic() + theme(legend.position = "none",legend.title = element_blank()) + 
  scale_x_continuous(name="true branch length brackets decile",breaks=(1:5)*2) +
  geom_text(aes(label=round(..y..,2),x=8),stat="summary")
  #stat_summary(fun.data = quantiles_75,geom="crossbar",size=0.2,width=0.2) 
ggsave("angios_bias.pdf",width = 9.7*0.9,height = 3.8*0.9)


t[t$method != "BEAST_lnorm_old",] %>%
  mutate(estLength = ifelse(estLength==0,10^-5,estLength)) %>%
  ggplot(
    aes(y=as.factor(cut(trueLength, round(quantile(t$trueLength,(0:10)/10)), labels = FALSE)),
    x=log10(trueLength/estLength),color=method)) + 
  #geom_point(alpha=0.3,size=0.1) + 
  facet_grid(clockModel~.) + 
  geom_vline(xintercept = 0, linetype=1,color="grey40") + 
  stat_summary(geom = "errorbar",width=0.3)+
  stat_summary(aes(group=method),geom="line")+
  scale_x_continuous(name=expression(log[10](true/estimated)))+
  scale_color_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,7,5:6)]) +
  theme_classic() + theme(legend.position = "none",legend.title = element_blank()) + 
  scale_y_discrete(name="true branch length brackets (decile)",breaks=(1:5)*2) +
  geom_text(aes(label=round(..x..,2),y=8),stat="summary")

ggplot(t[t$method != "BEAST_lnorm_old" &t$clockModel=="scenario1",],
       aes(x=cut(trueLength, round(quantile(t$trueLength,(0:10)/10)),
                 labels = FALSE,
                 #c(0,5,50,75,100,150),
                 # labels=c("[1,5]",",50]",",100]",",150]")
       ),
       y=(trueLength-estLength),color=method)) + 
  #geom_point(alpha=0.3,size=0.1) + 
  geom_hline(yintercept = 0, linetype=1,color="grey40") + 
  stat_summary(geom = "errorbar",width=0.3)+
  stat_summary(aes(group=method),geom="line")+
  #scale_y_continuous(tran=scales::trans_new("a",function(x) 
  #  after_stat(sign(x)*(abs(x))),inverse = function(x) sign(x)*1^(abs(x))),labels=scales::percent,
  #                   breaks = c(-1,-2,-4,-8,0,1/2),
  #        name = expression((estimated - ture) / true)) + 
  #scale_x_log10() + 
  scale_y_continuous(name=expression(( " "~true - estimated~" " )))+
  scale_color_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,7,5:6)]) +
  facet_grid(~clockModel) + 
  theme_classic() + theme(legend.position = "none",legend.title = element_blank()) + 
  scale_x_continuous(name="true branch length brackets decile",breaks=(1:5)*2) +
  geom_text(aes(label=round(..y..,2),x=1),stat="summary")
#stat_summary(fun.data = quantiles_75,geom="crossbar",size=0.2,width=0.2) 
ggsave("angios_bias_sc1.pdf",width = 2.8*0.9,height = 3.8*0.9)


t %>%  mutate(estLength = ifelse(estLength==0,10^-5,estLength)) %>%
  mutate(bias=trueLength-estLength,
         logbias=log10(trueLength/estLength)) %>%
  group_by(clockModel,method) %>% 
  summarize(mbias=mean(bias),
  #  mlogbias=mean(logbias)
    ) %>%
  pivot_wider(names_from = method, values_from = c(mbias))

t[t$method != "BEAST_lnorm_old" ,] %>%
  mutate(bias=trueLength-estLength) %>%
  group_by(clockModel,method, rep) %>% 
  summarise(bias=mean(bias)) %>%
ggplot(aes(x=method,
       y=(bias),fill=method)) + 
  #geom_point(alpha=0.3,size=0.1) + 
  geom_hline(yintercept = 0, linetype=1,color="grey40") + 
  stat_summary(position=position_dodge(width=0.9),width=.5,
               fun.data = quantiles_95,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_75,geom="crossbar",size=0.2) +
  stat_summary(position=position_dodge2(width = 0.9)) + 
  #scale_y_continuous(tran=scales::trans_new("a",function(x) 
  #  after_stat(sign(x)*(abs(x))),inverse = function(x) sign(x)*1^(abs(x))),labels=scales::percent,
  #                   breaks = c(-1,-2,-4,-8,0,1/2),
  #        name = expression((estimated - ture) / true)) + 
  #scale_x_log10() + 
  scale_y_continuous(name=expression( true - estimated~length~(million~years)))+
  scale_fill_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1:3,7,5:6)]) +
  facet_grid(~sub(pattern = "scenario","Sc. ",clockModel),scales="free_x",space="free", switch = "x") +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank()) +
  theme(legend.title = element_blank(),legend.position = "none") 
#stat_summary(fun.data = quantiles_75,geom="crossbar",size=0.2,width=0.2) 
ggsave("angios_bias_all.pdf",width = 7,height = 4.2)
