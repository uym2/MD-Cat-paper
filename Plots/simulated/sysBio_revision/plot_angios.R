#setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/sysBio_revision")
require(ggplot2)
require(scales)
require(tidyverse)
dk = read.table("MDCat_Angiosperm_vary_k_nodeAge.txt",header=T)

dk$error = abs(dk$trueNodeAge-dk$estNodeAge)/140
dk %>% group_by(scenario  ,  rep , k) %>%
  summarise(rmsn = sqrt(mean(trueNodeAge-estNodeAge)^2)/140) %>%
ggplot(aes(x=k,y=rmsn)) +
  stat_summary() + geom_line(stat="summary",size=1) + 
  scale_x_log10(breaks=c(2,5,10,25,50,100)) +  
  xlab("# rate categories") + 
  facet_wrap(~scenario,nrow=1)+
  scale_color_brewer(palette = "Set2")+
  scale_y_continuous(name="divergence time RSME (height normalized)") + 
  theme_bw() + theme(legend.title = element_blank(),legend.position = c(0.8,0.8))
ggsave("MDCat_Angiosperm_vary_k.pdf",width = 11,height=4)

d1 = read.table("../angiosperm/results_brTime.txt",header=T)
d2 = read.table("../angiosperm/results_brTime_moreBEAST.txt",header=T)
do=rbind(d1,d2)
do$method = factor(do$method,levels=c("Blnorm","BEAST_strict","BEAST_rcla","reltime","BEAST_lognorm","wlogdate","emd"),
                   labels = c("BEAST_lognorm","BEAST_strict","BEAST_rcla","RelTime","BEAST_lnorm_old","wLogDate","MD-Cat"))
do = do[do$method != "BEAST_lnorm_old",]


require(reshape2)
dcast(dk[,c(1,3,7)],k~scenario,fun.aggregate = mean)

########################################
require(reshape2)

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


dkc = read.table("MDCat_Angiosperm_vary_k_with_crossval.txt",header=T)
dkc$error = abs(dkc$trueNodeAge-dkc$estNodeAge)/140
dkc$selected = "Others"
dkc[dkc$k == "kselected",]$selected = "Cross-validation"
dkc[dkc$k == "k50",]$selected = "Default (k=50)"
dkc$selected = factor(dkc$selected, levels=c("Default (k=50)","Cross-validation","Others"))


dcast(dkc[,c(1,8,7)],scenario~selected,fun.aggregate = mean)
mean((dcast(dkc[,c(1,8,7)],scenario~selected,fun.aggregate = mean)[,3]-
  dcast(dkc[,c(1,8,7)],scenario~selected,fun.aggregate = mean)[,2])*100)


dcast(dkc[,c(1,3,7)],scenario~k,fun.aggregate = mean)

qplot((sub("k","",merge(dkc[dkc$k == "kselected" & dkc$nodeName =="I0",c(1,2,4,5,6,7)],
                        dkc[dkc$k != "kselected"& dkc$nodeName =="I0",c(1,2,3,4,5,6,7)],
      by=c("scenario","rep","nodeName","trueNodeAge","estNodeAge","error"))$k)
))+theme_bw()+xlab("k")+ylab("# replicates")
ggsave("selectedk_angio.pdf",width=3.2, height = 4)

d2 = dcast(selected+scenario+rep+k~"error",data=dkc,value.var = "error",fun.aggregate = mean)

require(scales)

head(d2)
ggplot(d2,aes(x=scenario,y=error,fill=selected)) + 
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position=position_dodge2(width = 0.9),size=0.3) + 
  theme_classic() + theme(axis.text.x = element_text(angle = 0), axis.title.x = element_blank(),
                          legend.title = element_blank(),legend.position = "bottom") + 
  ylab("divergence time error")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(labels = percent)
nrow(d2[d2$selected=="Default (k=50)",])
ggsave("MDCat_Angiosperm_crossval_k.pdf",width=6, height =4 )


head(dkc)
require(tidyr) 

merge(dkc[dkc$selected=="Cross-validation",],dkc[dkc$k=="k100",],by=c(1,2,4,5)) %>%
  group_by(scenario  ,  rep ) %>% summarise(mes=mean(error.x),meh=mean(error.y)) %>% 
  filter(mes==meh)
