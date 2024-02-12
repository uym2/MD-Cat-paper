#setwd("/Users/uym2/my_gits/MD-Cat-paper/Plots/simulated/sysBio_revision")
require(ggplot2)
require(scales)
require(tidyverse)
dk = read.table("../sysBio_revision/MDCat_Angiosperm_vary_k_nodeAge_revised.txt",header=T)

d1 = read.table("../angiosperm/all_divergence.txt",header=T)

d1$method = factor(d1$method,levels=c("Blnorm","Bstrict","Brcla","RelTime","BEAST_lognorm","wLogDate","MD-Cat"),
                   labels = c("BEAST: LogNorm","BEAST: RLC","BEAST: strict","RelTime","BEAST_lnorm_old","wLogDate","MD-Cat"))

dk %>% group_by(scenario  ,  rep , k) %>%
        summarise(norm_rmse = sqrt(mean( (trueNodeAge-estNodeAge)^2) )/140) %>%
        mutate(method="MD-Cat") %>%
#dk %>% group_by(scenario  ,  rep , k) %>%
#  summarise(norm_rmse = sqrt(mean( (trueNodeAge-estNodeAge)^2) )/140) %>%
ggplot(aes(x=k,y=norm_rmse,color=method)) +
  geom_line(stat="summary",linewidth=1) + 
  stat_summary() + 
  scale_x_log10(breaks=c(2,5,10,25,50,100)) +  
  xlab("# rate categories") + 
  facet_wrap(~scenario,nrow=1)+
  geom_hline(aes(yintercept=norm_rmse,color=method),
             data= d1 %>% filter(method!="MD-Cat") %>% group_by(method,scenario) %>% summarise(norm_rmse=mean(norm_rmse)))+
  scale_color_manual(values = RColorBrewer::brewer.pal(7,'Paired')[c(1,3,2,6,7,5)]) +
  scale_y_continuous(name="divergence time RSME (height normalized)") + 
  theme_classic() + theme(legend.position = c(0.75,0.76)) +
  guides(color  = guide_legend(nrow = 3))
ggsave("../sysBio_revision/MDCat_Angiosperm_vary_k_revised.pdf",width = 8.2,height=3.6)

dk %>% group_by(scenario  ,  rep , k) %>%
  summarise(rmsn = sqrt(mean( (trueNodeAge-estNodeAge)^2) )/140) %>%
  group_by(scenario,k) %>% summarise(mean=mean(rmsn)) %>%
  pivot_wider(names_from = scenario,values_from=mean)


########################################
require(reshape2)

quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


dkc = read.table("MDCat_Angiosperm_vary_k_with_crossval_revised.txt",header=T)
dkc$error = abs(dkc$trueNodeAge-dkc$estNodeAge)/140
dkc$selected = "Others"
dkc[dkc$k == "kselected",]$selected = "Cross-validation"
dkc[dkc$k == "k50",]$selected = "Default (k=50)"
dkc$selected = factor(dkc$selected, levels=c("Default (k=50)","Cross-validation","Others"))


dcast(dkc[,c(1,8,7)],scenario~selected,fun.aggregate = mean)
mean((dcast(dkc[,c(1,8,7)],scenario~selected,fun.aggregate = mean)[,3]-
  dcast(dkc[,c(1,8,7)],scenario~selected,fun.aggregate = mean)[,2])*100)


dcast(dkc[,c(1,3,7)],scenario~k,fun.aggregate = mean)

qplot(factor(sub("k","",merge(dkc[dkc$k == "kselected" & dkc$nodeName =="I0",c(1,2,4,5,6,7)],
                        dkc[dkc$k != "kselected"& dkc$nodeName =="I0",c(1,2,3,4,5,6,7)],
      by=c("scenario","rep","nodeName","trueNodeAge","estNodeAge","error"))$k),levels=c(2,5,10,25,50,100)))+theme_bw()+xlab("k")+ylab("# replicates")
ggsave("selectedk_angio_revised.pdf",width=3.2, height = 4)

#d2 = dcast(selected+scenario+rep+k~"error",data=dkc,value.var = "error",fun.aggregate = mean)

require(scales)

head(dkc)
dkc %>% group_by(  scenario ,   rep  , k, selected) %>%
  summarise(e = sqrt(mean( (trueNodeAge-estNodeAge)^2) ) , height=140) %>%
ggplot(aes(x=scenario,y=e/height,fill=selected)) + 
  stat_summary(position=position_dodge2(width=0.75),
               fun.data = quantiles_95,geom="boxplot") +
  stat_summary(position=position_dodge2(width = 0.9),size=0.3) + 
  theme_classic() + theme(axis.text.x = element_text(angle = 0), axis.title.x = element_blank(),
                          legend.title = element_blank(),legend.position = "bottom") + 
  ylab("divergence time error")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(labels = percent)
ggsave("MDCat_Angiosperm_crossval_k_revised.pdf",width=6, height =4 )



